

source(paste0(getwd(),"/modgeneric/R", "/init-RegressionBase.R"))
options(scipen=99999)

# =============================
# == Douglas Fletcher
# == Class: Linear Modelling
# == Purpose: run linear model
# == Dependencies: car, lmtest
# == Parent: RegressionBase
# =============================

library(car)
library(lmtest)


LinearLm <- setRefClass("LinearLm"
	# ===============================================
	# == Class: LinearLm
	# ===============================================
	, contains = c("RegressionBase")
	# set additional fields if required
	, fields = list(
		modelOut = "list"
	)
	# need to overide base methods (minimum)
	, methods= list(

		# ======================
		# == implemented methods
		# ======================

		runChecks = function(){
			print('=======================')
			print('=======================')
			print('=======================')
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
			# linear model functions
			.self$createModelObject()
			.self$modelAddValues()
		},

		# =================================
		#== linear modelling: check methods
		# =================================
		checkModelVars = function(){
			# modelVars check
			if (!all(c("varsDep", "varsIndep", "baseline") %in% names(.self$modelVars))){
				print(sprintf("modelVars should contain elements: %s", "(varsDep, varsIndep, baseline)"))
				return(FALSE)
			}
			# check varsDep
			if (!all(c("vars") %in% names(.self$modelVars[["varsDep"]]))){
				print(sprintf("varsDep must be a list with elements: %s", "(vars)"))
				return(FALSE)
			}
			# check varsIndep
			for (cols in .self$modelVars[["varsIndep"]]){
				if(!all(c("vars") %in% names(cols))){
					print(sprintf("varsIndep must be a list of list all with elements: %s", "(vars)"))
					return(FALSE)
				}
			}
			# check baseline
			if (!all(c("vars") %in% names(.self$modelVars[["baseline"]]))){
				print(sprintf("baseline must be a list with elements: %s", "(vars)"))
				return(FALSE)
			}
			# baseline check
			if(toString(.self$modelVars[["baseline"]]["vars"]) != "bzero"){
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
				if(!(modelVars[["varsIndep"]][[colNum]]["vars"] %in% varsDataset)){
					print(sprintf("variable %s not in the dataset ... ", modelVars[["varsIndep"]][[colNum]]["vars"]))
					return(FALSE)
				}
			}
			if (!(.self$modelVars[["varsDep"]]["vars"] %in% varsDataset)){
				print(sprintf("variable %s not in the dataset ... ", .self$modelVars[["varsDep"]]["vars"]))
				return(FALSE)
			}
			return(TRUE)
		},

		# ==================================
		# == linear modelling: model methods
		# ==================================
		createModelObject = function(){
			# get independent vars
			indepVars <- lapply(.self$modelVars[["varsIndep"]], function(col){
				varPrep = sprintf("%s", col["vars"])
			})
			# create functions
			fnString <- sprintf("%s ~ %s"
				, .self$modelVars[["varsDep"]]["vars"]
				, do.call("paste", c(indepVars, sep = " + "))
			)
			# if no base
			if (.self$modelVars[["baseline"]]["vars"] != "bzero"){
				fnString <- paste0(fnString, " + -1")
			}
			.self$modelOut[["defaultOutput"]] <- lm(formula = fnString, data = .self$modelSet)

			# parent class coefficients standardize
			.self$modelOut[["coefficients"]] <- coef(.self$modelOut[["defaultOutput"]])
			for (col in 1:length(.self$modelOut[["coefficients"]])){
				if (names(.self$modelOut[["coefficients"]])[col] == "(Intercept)"){
					names(.self$modelOut[["coefficients"]])[col] = "bzero"
				}
			}
		},

		modelAddValues = function() {

			# add stats
			sumStats <- summary(.self$modelOut[["defaultOutput"]])
			vif <- NA
			if (length(.self$modelOut[["coefficients"]]) >= 3){
				vif <- vif(.self$modelOut[["defaultOutput"]])
			}
			dw <- dwtest(.self$modelOut[["defaultOutput"]])
			bp <- bptest(.self$modelOut[["defaultOutput"]])
			rs <- resettest(.self$modelOut[["defaultOutput"]])
			aic <- extractAIC(.self$modelOut[["defaultOutput"]])[2]
			bic <- BIC(.self$modelOut[["defaultOutput"]])
			dep <- toString(.self$modelVars[["varsDep"]]["vars"])

			# create contribs
			colnames <- names(.self$modelOut[["coefficients"]])
			if(toString(.self$modelVars[["baseline"]]["vars"]) == "bzero"){
				dataCoef <- cbind(.self$modelSet, bzero = 1)[names(.self$modelOut[["coefficients"]])]
			} else {
				dataCoef <- .self$modelSet[names(.self$modelOut[["coefficients"]])]
			}
			contrib <- t(t(dataCoef) * .self$modelOut[["coefficients"]])

			# get default summary table to alter
			alteredStats <- sumStats["coefficients"]

			# adjust coefficent names
			rownames(alteredStats[[1]]) <- names(.self$modelOut[["coefficients"]])
			# calculte total contribs
			totalPercent <- colSums(contrib) / sum(contrib) * 100
			print(class(totalPercent))
			# merge percent
			outputdat <- cbind(alteredStats[[1]], totalPercent)
			# add vif
			if(toString(.self$modelVars[["baseline"]]["vars"]) == "bzero"){
				outputdat <- cbind(outputdat, c("bzero"=NA,vif) )
			} else {
				outputdat <- cbind(outputdat, vif)
			}
			colnames(outputdat)[ncol(outputdat)] <- "Var. Inflation"
			# merge to summary
			alteredStats[[1]] <- round(outputdat,1)
			sumStats["coefficients"] <- alteredStats

			# all diagnostics to one object
			generalDiag = list("sumStats" = sumStats)
			specificDiag = list("vif" = vif, "dw" = dw, "bp" = bp
			, "reset" = rs, "aic" = aic, "bic" = bic)

			# save stats
			.self$modelOut[["modelStats"]] <- list(
				"diagnostics" = list("generalDiag" = generalDiag, "specificDiag" = specificDiag)
				, "dep" = dep
				, "contribs" = data.frame(contrib)
			)
		}
	)
)

