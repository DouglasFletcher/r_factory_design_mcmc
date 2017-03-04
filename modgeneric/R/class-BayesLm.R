
source(paste0(getwd(),"/modgeneric/R", "/init-RegressionBase.R"))

# ============================
# == Douglas Fletcher
# == Class: Bayes Modelling
# == Purpose: run bayes model
# == Dependencies: R2WinBugs
# == , BRugs, foreign
# == Parent: RegressionBase
# ============================

options(scipen=99999)

library(R2OpenBUGS)

BayesLm <- setRefClass("BayesLm"
	# ===================
	# == Class: BayesLm
	# ===================
	, contains = c("RegressionBase")
	# set additional fields if required
	, fields = list(
		  bugsFunction = "character"
		, windata = "list"
		, params = "list"
		, inits = "function"
		, modelOut = "list"
	)
	# need to overide base methods (minimum)
	, methods= list(
		# ======================
		# == implemented methods
		# ======================
		runChecks = function() {
			print("=======================")
			print("=======================")
			print("=======================")
			# model params checks
			if(!(.self$checkModelParams())){
				stop("error in inputs: check modelParams")
			}
			# model vars checks
			if(!(.self$checkModelVars())){
				stop("error in inputs: check modelVars")
			}
			# model set checks
			if(!(.self$checkModelSet())){
				stop("error in inputs: check modelSet")
			}
			print("all user checks passed ...")
		},

		runModel = function() {
			print("running model...")
			# bugs input function
			.self$modelTextCreate()
			.self$bugsParamsInitCreate()
			.self$modelAddValues()
		},

		# ================================
		#== bayes modelling: check methods
		# ================================
		checkModelParams = function(){
			# modelParams check
			if(!all(c("bugsParams","bayesParams") %in% names(.self$modelParams))){
				print(sprintf("modelParams must be a list with elements: %s", "(bugsParams, bayesParams)"))
				return(FALSE)
			}
			# check bugsParams
			if (!all(c("debug", "DIC", "bugs.directory", "program","working.directory") %in% names(.self$modelParams[["bugsParams"]]))){
				print(sprintf("bugsParams must be a list with elements: %s", "(debug, DIC, bugs.directory, program, working.directory)"))
				return(FALSE)
			}
			# check bayesParams
			if (!all(c("ni", "nc", "nb", "nt") %in% names(.self$modelParams[["bayesParams"]]))){
				print(sprintf("bayesParams must be a list with elements: %s", "(ni, nc, nb, nt)"))
				return(FALSE)
			}
			return(TRUE)
		},

		checkModelVars = function(){
			# modelVars check
			if (!all(c("errorDist", "varsIndep", "baseline") %in% names(.self$modelVars))){
				print(sprintf("modelVars should contain elements: %s", "(errorDist, varsIndep, baseline)"))
				return(FALSE)
			}
			# check errorDist
			if (!all(c("vars","distr","tauVar","tauDistr","tauStart") %in% names(.self$modelVars[["errorDist"]]))){
				print(sprintf("errorDist must be a list with elements: %s", "(vars, distr, tauVar, tauDistr, tauStart)"))
				return(FALSE)
			}
			# check varsIndep
			for (cols in .self$modelVars[["varsIndep"]]){
				if(!all(c("vars","distr","startVal") %in% names(cols))){
					print(sprintf("varsIndep must be a list of list all with elements: %s", "(vars, distr, startVal)"))
					return(FALSE)
				}
			}
			# check baseline
			if (!all(c("vars","distr","startVal") %in% names(.self$modelVars[["baseline"]]))){
				print(sprintf("baseline must be a list of list all with elements: %s", "(vars, distr, startVal)"))
				return(FALSE)
			}
			# baseline check
			if(.self$modelVars[["baseline"]]["vars"] != "bzero"){
				print("baseline var != bzero hence no intercept will be added in the model.")
			}
			return(TRUE)
		},

		checkModelSet = function(){
			# check data.frame
			if(class(.self$modelSet) != "data.frame"){
				return(FALSE)
			}
			# check all variables
			varsDataset <- names(.self$modelSet)
			for(colNum in 1:length(.self$modelVars[["varsIndep"]])){
				if(!(.self$modelVars[["varsIndep"]][[colNum]]["vars"] %in% varsDataset)){
					print(sprintf("variable %s not in the dataset ... ", .self$modelVars[["varsIndep"]][[colNum]]["vars"]))
					return(FALSE)
				}
			}
			if (!(.self$modelVars[["errorDist"]]["vars"] %in% varsDataset)){
				print(sprintf("variable %s not in the dataset ... ", .self$modelVars[["errorDist"]]["vars"]))
				return(FALSE)
			}
			# round all variables (necessary for bayes)
			tryCatch({
					numericVars <- sapply(.self$modelSet, is.numeric)
					.self$modelSet[numericVars] <- lapply(.self$modelSet[numericVars], round, 8)
				}, error = function(e){
					print("error rounding variables in dataset")
					return(FALSE)
				}
			)
			return(TRUE)
		},

		# =================================
		# == bayes modelling: model methods
		# =================================
		modelTextCreate = function(){
			# default function none
			.self$bugsFunction = ""
			# independent variables
			indepVars <- lapply(.self$modelVars[["varsIndep"]], function(col){
				varPrep = sprintf("%s[i]", col["vars"])
				sprintf("f_%s * %s \n", col["vars"], varPrep)
			})
			# prior distributions
			priors <- lapply(.self$modelVars[["varsIndep"]], function(prior){
				sprintf("f_%s ~ %s", prior["vars"], prior["distr"])
			})
			# base if required
			if (.self$modelVars[["baseline"]]["vars"] == "bzero"){
				indepVars[length(indepVars) + 1] <- .self$modelVars[["baseline"]]["vars"]
				priors[length(priors) + 1] <- sprintf("%s ~ %s", .self$modelVars[["baseline"]]["vars"]
					, .self$modelVars[["baseline"]]["distr"])
			}
			# dependent variable
			depVar <- sprintf("%s[i] ~ %s(mu[i], %s)", .self$modelVars[["errorDist"]]["vars"]
				, .self$modelVars[["errorDist"]]["distr"], .self$modelVars[["errorDist"]]["tauVar"])
			# error distribution
			priors[length(priors) + 1] <- sprintf("%s ~ %s", .self$modelVars[["errorDist"]]["tauVar"]
				, .self$modelVars[["errorDist"]]["tauDistr"])
			# function to print
			fnString <- sprintf("model {for(i in 1:ndata) {%s mu[i] <- %s} %s}"
				, depVar
				, do.call("paste", c(indepVars, sep = " \t\t\t\t\t\t +"))
				, do.call("paste", c(priors, sep = "\n \t\t\t\t\t"))
			)
			# model file location
			if (.self$modelParams[["bugsParams"]]["working.directory"] == "temp"){
				.self$bugsFunction <- normalizePath(tempdir())
			}
			else {
				.self$bugsFunction <- toString(.self$modelParams[["bugsParams"]]["working.directory"])
			}
			print(sprintf("model file saved in: %s", paste0(.self$bugsFunction, "/modelfile.txt")))
			sink(paste0(.self$bugsFunction, "/modelfile.txt"))
				cat(fnString, fill=TRUE)
			sink()
		},

		bugsParamsInitCreate = function() {
			# windata, params & inits
			.self$windata <- list()
			.self$params <- list()
			.self$inits <- function(){}
			# inits
			initsVal <- list()
			# windata
			.self$windata[toString(.self$modelVars[["errorDist"]]["vars"])] = .self$modelSet[toString(.self$modelVars[["errorDist"]]["vars"])]
			# windata & params
			count = 1
			for (var in .self$modelVars[["varsIndep"]]){
				varName <- toString(var["vars"])
				.self$windata[varName] <- .self$modelSet[varName]
				.self$params <- c(.self$params, sprintf("f_%s", varName))
				initsVal[sprintf("f_%s", varName)] <- .self$modelVars[["varsIndep"]][[count]]["startVal"]
				count = count + 1
			}
			# windata
			.self$windata["ndata"] <- nrow(.self$modelSet)
			# params & inits
			if (.self$modelVars[["baseline"]]["vars"] == "bzero"){
				.self$params <- c(.self$params, toString(.self$modelVars[["baseline"]]["vars"]))
				initsVal[toString(.self$modelVars[["baseline"]]["vars"])] <- .self$modelVars[["baseline"]]["startVal"]
			}
			# inits
			initsVal["tau"] <- .self$modelVars[["errorDist"]]["tauStart"]
			.self$inits <- function(){
				eval(initsVal)
			}
			# run model
			bugsOut = R2OpenBUGS::bugs(
				  data = .self$windata
				, inits = .self$inits
				, parameters.to.save = .self$params
				, model.file = "modelfile.txt"
				, n.thin = as.numeric(.self$modelParams[["bayesParams"]]["nt"])
				, n.chains = as.numeric(.self$modelParams[["bayesParams"]]["nc"])
				, n.burnin = as.numeric(.self$modelParams[["bayesParams"]]["nb"])
				, n.iter = as.numeric(.self$modelParams[["bayesParams"]]["ni"])
				, debug = as.logical(.self$modelParams[["bugsParams"]]["debug"])
				, DIC = as.logical(.self$modelParams[["bugsParams"]]["DIC"])
				#, bugs.directory = toString(.self$modelParams[["bugsParams"]]["bugs.directory"])
				#, program = toString(.self$modelParams[["bugsParams"]]["program"])
				, working.directory = toString(.self$bugsFunction)
			)

			# return default modelling object
			.self$modelOut[["defaultOutput"]] = bugsOut

			# clean up temp (if using - just in case...)
			if (.self$modelParams[["bugsParams"]]["working.directory"] == "temp"){
				print("cleaning up temp dir")
				unlink(paste0(toString(.self$bugsFunction),"/*"))
			}

			# get standarised coefficient
			vars <- bugsOut[["mean"]]
			coefVars <- c()
			for (col in 1:length(vars)){
				if (names(vars)[col] == "deviance"){
					deviance <- vars[[col]]
				}
				else if (names(vars)[col] == "bzero"){
					coefVars[names(vars)[col]] <- vars[[col]]
				}
				else {
					coefVars[substr(names(vars)[col], 3, nchar(names(vars)[col]))] <- vars[[col]]
				}
			}
			.self$modelOut[["coefficients"]] = coefVars
		},

		modelAddValues = function(){

			# ad stats
			sumStats <- .self$modelOut[["defaultOutput"]][["summary"]]
			dep <- toString(.self$modelVars[["errorDist"]]["vars"])
			dic <- .self$modelOut[["defaultOutput"]][["DIC"]]

			# create contribs
			colnames <- names(.self$modelOut[["coefficients"]])
			if(toString(.self$modelVars[["baseline"]]["vars"]) == "bzero"){
				dataCoef <- cbind(.self$modelSet, bzero = 1)[names(.self$modelOut[["coefficients"]])]
			} else {
				dataCoef <- .self$modelSet[names(.self$modelOut[["coefficients"]])]
			}
			contrib <- t(t(dataCoef) * .self$modelOut[["coefficients"]])

			# add tot contribs to summary stats table
			alteredStats <- sumStats
			# adjust coefficent names
			if ("deviance" %in% rownames(alteredStats)){
				rownames(alteredStats) <- c(names(.self$modelOut[["coefficients"]]), "deviance")
			} else {
				rownames(alteredStats) <- names(.self$modelOut[["coefficients"]])
			}

			# tot contribs
			totalPercent <- colSums(contrib) / sum(contrib) * 100
			# merge to summary
			outputdat <- merge(alteredStats, totalPercent, by = "row.names", all = TRUE)
			outputdatmtx <- as.matrix(outputdat[2:ncol(outputdat)])
			rownames(outputdatmtx) <- outputdat[[1]]
			colnames(outputdatmtx)[ncol(outputdatmtx)] <- "totalPercent"
			outputdatmtx <- round(outputdatmtx,1)
			sumStats <- outputdatmtx

			# all diagnostics to one object
			generalDiag <- list("sumStats" = sumStats)
			specificDiag <- list(dic = "dic")

			# save Stats
			.self$modelOut[["modelStats"]] = list(
				"diagnostics" = list("generalDiag" = generalDiag, "specificDiag" = specificDiag)
				, "dep" = dep
				, "contribs" = data.frame(contrib)
			)
		}
	)
)
