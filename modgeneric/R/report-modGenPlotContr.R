

#' contribution plot
#' @param modelRun
#' @param dateVar
#' @ordered default TRUE
#' examples: 
#' modGenPlotContr(modelRunLinear, "period", ordered = FALSE)
#' modGenPlotContr(modelRunBayes, "period")
#' @export
modGenPlotContr <- function(modelRun, dateVar, ordered = TRUE){

	library(ggplot2)
	library(reshape2)

	if (dateVar %in% names(modelRun[["modelSet"]])){
		# get data
		modelDat <- modelRun[["modelSet"]]
		contrib <- modelRun[["modelOut"]][["modelStats"]][["contribs"]]
		fitted <- rowSums(contrib)
		# date var
		if (dateVar %in% names(modelDat)){
			date <- modelDat[dateVar]
			names(date) <- "date"
		} else {
			stop(sprintf("Variable $s is not in modelSet", dateVar))
		}
		cont <- data.frame(date, contrib)
		# flatfile
		plotDf <- melt(cont, id="date", variable.name="variable", value.name="contribution")

		# percent of contribs
		totalContr <- colSums(contrib) / sum(fitted)
		if(ordered){
			totalContr <- totalContr[order(abs(totalContr), decreasing = TRUE)]
		}
		tmpNames <- paste0(names(totalContr), "\n(", round(totalContr*100, 1) , "%)")
		plotDf$var2 <- factor(plotDf$variable, levels = names(totalContr), labels = tmpNames)

		#### contributions by variable
		g1 <- ggplot(plotDf, aes(x = date, y = contribution, group = var2)) + 
		geom_line() + facet_wrap(~var2, scales="free") +
		ggtitle("contributions per variable (ordered by %)")
		print(g1)
		return(invisible())
	} else {
		stop("dateVar was not found in dataset")
	}
}
