

#' compare two avms
#' @param modelRun1
#' @param modelRun2
#' @param dateVar1
#' @param dateVar2
#' @param dy = T or F
#' example:
#' modGenCompareAvm(modelRunLinear, modelRunBayes, "period", "period", dy=TRUE)
#' modGenCompareAvm(modelRunLinear, modelRunBayes, "period", "period")
#' @export
modGenCompareAvm <-  function(modelRun1, modelRun2, dateVar1, dateVar2, dy=FALSE){

	if ( (dateVar1 %in% names(modelRun1[["modelSet"]])) && (dateVar2 %in% names(modelRun1[["modelSet"]])) ){
		# depends
		library("car")
		library("ggplot2")
		library("reshape2")
		library("grid")

		# model 1 KPIs
		predicted1 <- c(rowSums(modelRun1[["modelOut"]][["modelStats"]][["contribs"]]))
		actual1 <- c(modelRun1[["modelSet"]][, modelRun1[["modelOut"]][["modelStats"]][["dep"]]])
		date1 <- c(modelRun1[["modelSet"]][, dateVar1])
		modelType1 <- paste0("_", class(modelRun1[["modelOut"]][["defaultOutput"]]), "mod_1")
		avm1 <- cbind(predicted1, actual1, date1)
		colnames(avm1) <- c(paste0("predicted",modelType1),paste0("actual",modelType1),"date")

		# model 2 KPIs
		predicted2 <- c(rowSums(modelRun2[["modelOut"]][["modelStats"]][["contribs"]]))
		actual2 <- c(modelRun2[["modelSet"]][, modelRun2[["modelOut"]][["modelStats"]][["dep"]]])
		date2 <- c(modelRun2[["modelSet"]][, dateVar2])
		modelType2 <- paste0("_", class(modelRun2[["modelOut"]][["defaultOutput"]]), "mod_2")
		avm2 <- data.frame(predicted2, actual2, date2)
		colnames(avm2) <- c(paste0("predicted",modelType2),paste0("actual",modelType2),"date")

		# merge data
		avm <- merge(avm2, avm1, by.x="date", by.y="date")
		m1 <- melt(avm[, c(1,2,4)], id = "date")
		m2 <- melt(avm[, c(1,3,5)], id = "date")

		# graphics: AVM & residuals
		tit1 <- sprintf("Predicted1 vs Predicted2")
		g1 <- ggplot(data = m1, aes(x = date, y = value, colour = variable, group = variable)) + 
			geom_line() + theme(legend.position = "top", axis.title.x = element_blank()) + 
			labs(title = tit1) + theme(legend.key.height = unit(0.5, "cm")
		)
		tit2 <- sprintf("Actual1 vs Actual2")
		g2 <- ggplot(data = m2, aes(x = date, y = value, colour = variable, group = variable)) + 
			geom_line() + theme(legend.position = "top", axis.title.x = element_blank()) + 
			labs(title = tit1) + theme(legend.key.height = unit(0.5, "cm")
		)
		arrangeGgPlot2(g1, g2, ncol = 1)

		# dygraph test
		if(dy == TRUE & class(avm[,"date"]) == "Date"){
			require(xts)
			require(dygraphs)

			avm_dydf <- avm[, c(paste0("predicted",modelType2), paste0("predicted",modelType1))]
			avm_dydf <- xts(avm_dydf, avm$date)
			p <- dygraph(avm_dydf, main = tit1)
			return(p)
		}

	} else {
		stop("dateVar1 or dateVar2 was not found in dataset")
	}
}

