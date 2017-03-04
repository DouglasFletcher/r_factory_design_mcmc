
#' create model avm
#' @param modelRun
#' @param dateVar
#' @param dy
#' example:
#' modGenPlotAvm(modelRunLinear, "period")
#' modGenPlotAvm(modelRunBayes, "period")
#' @export
modGenPlotAvm <-  function(modelRun, dateVar, dy=FALSE){

	if (dateVar %in% names(modelRun[["modelSet"]])){
		# depends
		require("car")
		require("ggplot2")
		require("reshape2")
		require("grid")
		# get data
		predicted <- rowSums(modelRun[["modelOut"]][["modelStats"]][["contribs"]])
		actual <- modelRun[["modelSet"]][, modelRun[["modelOut"]][["modelStats"]][["dep"]]]
		residuals <- predicted - actual
		date <- modelRun[["modelSet"]][, dateVar]
		# calculate
		avm <- data.frame(predicted, residuals, actual, date)
		colnames(avm)[c(1, 3)] <- c("modelled", "actual")
		rSquared <- sum((predicted - mean(actual))^2) / sum((actual - mean(actual))^2) 
		rSquaredCor <- cor(predicted, actual)^2
		mape <- mean(abs((predicted-actual)/actual))
		m <- melt(avm[, -2], id = "date")
		# graphics: AVM & residuals
		tit1 <- sprintf("Actual vs Modelled for model class (%s), R^2: %.2f (%.2f), mape: %.2f"
			, class(modelRun[["modelOut"]][["defaultOutput"]])
			, rSquared*100
			, rSquaredCor*100
			, mape*100
		)
		# dygraph test
		if(dy == TRUE & class(date) == "Date"){
			require(xts)
			require(dygraphs)

			avm_dydf <- avm[, c("modelled", "actual")]
			avm_dydf <- xts(avm_dydf, avm$date)
			p <- dygraph(avm_dydf, main = tit1)
			return(p)
		}
		g1 <- ggplot(data = m, aes(x = date, y = value, colour = variable, group = variable)) + 
			geom_line() + theme(legend.position = "top", axis.title.x = element_blank()) + 
			labs(title = tit1) + theme(legend.key.height = unit(0.5, "cm")
		)
		g2 <- ggplot(data = avm, aes(x = date, y = residuals)) + 
			geom_point() + geom_line(linetype = 2) + 
			theme(axis.title.x = element_blank()) + 
			labs(title = "Residuals over Time", y = "modelled - actual")
		arrangeGgPlot2(g1, g2, ncol = 1)
	} else {
		stop("dateVar was not found in dataset")
	}
}

#modGenPlotAvm(modelRunLinear, "period")
#modGenPlotAvm(modelRunBayes, "period")

