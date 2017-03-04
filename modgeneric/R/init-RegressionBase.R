options(scipen=99999)

# ===========================================================
# == Douglas Fletcher
# == Classes: RegressionBase & 
# == Purpose: 
# == RegressionBase is a parent class for all 
# == model classes. It defines a structure for
# == model classes. Having a standardised parent class
# == will allow for all modelling classes (i.e. LinearLm.r)
# == to give a standardised output, and hence 
# == all functions such as avm, contributions, etc.
# == Basicially like an interface in Java / Abstract Class
# == which lower level classes should implement / follow 
# == its blueprint. Initially I thought it would be good 
# == to make it generic inorder to create a multi-tier 
# == modelling framework with different modelling types 
# == and all with the same output standardized
# ===========================================================


RegressionBase <- setRefClass("RegressionBase"
	# ==================================================
	# == Class: RegressionBase
	# ==================================================
	# == fields:
	# == modelSet - data source (data.frame)
	# == modelParams - requirements for the model (list)
	# == modelVars - variables for model (list)
	# == modelOut - output from model (list)
	# =====================================================
	# == methods: 
	# == modelOutInIt - default return value
	# == runChecks - check user input (overide in subclass)
	# == runModel - process model (overide in subclass)
	# == checkOutput - check output is correct format
	# == getModelOut - getMethod accessors
	# =====================================================
	, fields = list(
		  modelSet = "data.frame"
		, modelParams = "list"
		, modelVars = "list"
		, modelOut = "list"
	)

	, methods = list(

		modelOutInIt = function(){
			# structured model output
			.self$modelOut = list(
				  defaultOutput= list()
				, coefficients = list()
				, modelStats = list()
			)
		},

		runChecks = function() {
			print("required to implement")
			return(0);
		},

		runModel = function() {
			print("required to implement")
			return(0);
		},

		checkOutput = function() {

			# modelVars check
			if (!all(c("defaultOutput", "coefficients", "modelStats") %in% names(.self$modelOut))){
				stop(sprintf("modelOut should contain elements: %s", "(defaultOutput, coefficients, modelStats)"))
			}
			# coefficients
			varsDataset <- c(names(.self$modelSet), "bzero")
			for(colNum in 1:length(.self$modelOut[["coefficients"]])){
				if(!(names(.self$modelOut[["coefficients"]])[[colNum]] %in% varsDataset)){
					stop(sprintf("coefficient %s not in the dataset ... ", .self$modelOut[["coefficients"]][[colNum]]))
				}
			}
			# stats check
			diagnosticsVars <- c(names(.self$modelOut[["modelStats"]]))
			if (!all(c("diagnostics","dep","contribs") %in% diagnosticsVars)){
				stop(sprintf("modelStats should contain elements: %s", "(diagnostics, dep, contribs)"))
			}
			if (class(.self$modelOut[["modelStats"]][["diagnostics"]]) != "list"){
				stop(sprintf("modelStats[diagnostics] should be class: %s", "list"))
			}
			if (class(.self$modelOut[["modelStats"]][["contribs"]]) != "data.frame"){
				stop(sprintf("modelStats[diagnostics] should be class: %s", "data.frame"))
			}
			if (class(.self$modelOut[["modelStats"]][["dep"]]) != "character"){
				stop(sprintf("modelStats[diagnostics] should be class: %s", "character"))
			}
			# check diagnostics
			diagVars <- c(names(.self$modelOut[["modelStats"]][["diagnostics"]]))
			if (!all(c("generalDiag","specificDiag") %in% diagVars)){
				stop(sprintf("modelStats[diagnostics] should contain elements: %s", "(generalDiag, specificDiag)"))
			}
			diagGeneralVar <- c(names(.self$modelOut[["modelStats"]][["diagnostics"]][["generalDiag"]]))
			if (!all(c("sumStats") %in% diagGeneralVar)){
				stop(sprintf("diagnostics[generalDiag] should contain elements: %s", "sumStats"))
			}
			if (class(.self$modelOut[["modelStats"]][["diagnostics"]][["generalDiag"]]) != "list"){
				stop(sprintf("modelStats[diagnostics] should be class: %s", "list"))
			}
		},

		getSummary = function() {
			.self$modelOut[["modelStats"]][["diagnostics"]][["generalDiag"]][["sumStats"]]
		}

	)
)
RegressionBase$accessors("modelOut")
RegressionBase$accessors("modelSet")














