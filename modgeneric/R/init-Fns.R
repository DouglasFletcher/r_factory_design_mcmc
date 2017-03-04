
RegressionFactory <- setRefClass("RegressionFactory"
	# ==================================================
	# == Class: RegressionFactory
	# Factory pattern - create instance of model
	# ==================================================
	# == fields:
	# == modelType: model variables (character) - input
	# == modelSet: model dataset (data.frame) - input
	# == modelParams: model parameters (list) - input
	# == modelVars: model variables (list) - input
	# == modelOut: model output - created
	# ==================================================
	# == methods: 
	# == getModel - create Model type
	# ==================================================
	, fields = list(
		  modelType = "character"
		, modelSet = "data.frame"
		, modelParams = "list"
		, modelVars = "list"
		, modelOut = "list"
	)
	, methods = list(

		createModel = function() {
			modelInst = NA
			# class- model files
			childClasses <- list.files(paste0(getwd(),"/modgeneric/R/"), full.names=FALSE, pattern = "class-")
			# create instance of object
			if (paste0("class-", .self$modelType, ".R") %in% childClasses){
				tryCatch({
					classText <- sprintf("%s(modelSet=.self$modelSet
						, modelParams=.self$modelParams
						, modelVars=.self$modelVars)", .self$modelType
					)
					modelInst <- eval(parse(text=classText))
				}, error= function(e){
					stop("the class was not created in RegressionBase - RegressionFactory.")
				})
			} else {
				print(sprintf("model type %s not recognised ... ", .self$modelType))
				print(sprintf("should be of type:"))
				modelTypes = do.call("paste", list(childClasses))
				print(modelTypes)
			}
			return(modelInst)
		}

	)
)

#' create model object: creates model object
#' @param modelType
#' @param modelSet
#' @param modelParams
#' @param modelVars
#' @return list
#' example: modGenCreate(modeType=modeType, modelSet=modelSet, modelParams=modelParams, modelVars=modelVars)
#' @export
modGenCreate <- function(
		  modelType = ""
		, modelSet = data.frame()
		, modelParams = list()
		, modelVars = list()
	){
	# create factory object
	regInit <- RegressionFactory(modelType = modelType, modelSet = modelSet
		, modelParams = modelParams, modelVars = modelVars
	)
	# create model object
	modelInit <- regInit$createModel()
	return(modelInit)
}


#' run model object: creates output run on model object
#' @param modelObject
#' @return list
#' example: 
#' modGenRun(modelObject=modelObject)
#' @export
modGenRun <- function(modelObject){

	# run init return value, checks & model
	modelObject$modelOutInIt()
	modelObject$runChecks()
	modelObject$runModel()
	modelObject$checkOutput()

	print("output stored in modGenRun return value")
	print("reporting functions can be run on output.")

	# print general summary output
	print(modelObject$getSummary())

	# return output
	return(list("modelOut" = modelObject$getModelOut(), "modelSet" = modelObject$getModelSet()))

}
