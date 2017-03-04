

#' export contributions in standard format
#' @param modelRun
#' @param dateVar
#' @filename default "./contribTab.csv"
#' examples:
#' modGenExportContrib(modelRunLinear, "period", filename = "./contribTabLinear.csv")
#' modGenExportContrib(modelRunBayes, "period", filename = "./contribTabBayes.csv")

#' @export
modGenExportContrib <- function(modelRun, dateVar, filename = "./contribTab.csv"){
	# depends
	library(reshape2)
	# check dataVar
	if (dateVar %in% names(modelRun[["modelSet"]])){

		cont <- modelRun[["modelOut"]][["modelStats"]][["contribs"]]
		df <- data.frame(
			id = 1:nrow(cont)
			, date = as.character(modelRun[["modelSet"]][[dateVar]])
			, depVar = modelRun[["modelSet"]][, modelRun[["modelOut"]][["modelStats"]][["dep"]]]
			, measured = rowSums(modelRun[["modelOut"]][["modelStats"]][["contribs"]])
			, cont
		)

		df <- melt(df, id.vars = c("id", "date", "depVar", "measured"))
		write.csv(df, filename)
		invisible(df)
	} else {
		stop("dateVar was not found in dataset")
	}
}
