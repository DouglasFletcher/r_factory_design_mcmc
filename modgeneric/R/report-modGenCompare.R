
#' compare two models
#' @param modelRun1
#' @param modelRun2
#' example:
#' modGenCompare(modelRunLinear, modelRunBayes)
#' @export
modGenCompare <- function(modelRun1, modelRun2){

	library(grid)
	library(gridExtra)

	# model 1: kpis
	predicted1 <- rowSums(modelRun1[["modelOut"]][["modelStats"]][["contribs"]])
	actual1 <- modelRun1[["modelSet"]][, modelRun1[["modelOut"]][["modelStats"]][["dep"]]]
	modelType1 <- class(modelRun1[["modelOut"]][["defaultOutput"]])
	rSquared1 <- sum((predicted1 - mean(actual1))^2) / sum((actual1 - mean(actual1))^2) 
	rSquaredCor1 <- cor(predicted1, actual1)^2
	mape1 <- mean(abs((predicted1-actual1)/actual1))
	co1 <- modelRun1[["modelOut"]][["coefficients"]]
	contr1 <- colSums(modelRun1[["modelOut"]][["modelStats"]][["contribs"]]) / 
	sum(modelRun1[["modelOut"]][["modelStats"]][["contribs"]])

	# model 2: kpis
	predicted2 <- rowSums(modelRun2[["modelOut"]][["modelStats"]][["contribs"]])
	actual2 <- modelRun2[["modelSet"]][, modelRun2[["modelOut"]][["modelStats"]][["dep"]]]
	modelType2 <- class(modelRun2[["modelOut"]][["defaultOutput"]])
	rSquared2 <- sum((predicted2 - mean(actual2))^2) / sum((actual2 - mean(actual2))^2) 
	rSquaredCor2 <- cor(predicted2, actual2)^2
	mape2 <- mean(abs((predicted2-actual2)/actual2))
	co2 <- modelRun2[["modelOut"]][["coefficients"]]
	contr2 <- colSums(modelRun2[["modelOut"]][["modelStats"]][["contribs"]]) /
	sum(modelRun2[["modelOut"]][["modelStats"]][["contribs"]])

	cat(sprintf("ModelType: %s vs %s, R2: mod1 %.02f, mod2 %.02f\nR2Cor mod1 %.02f, mod2 %.02f\n 
		mape: mod1 %.02f,  mod2 %.02f\n\n"
		, modelType1, modelType2
		, rSquared1*100, rSquared2*100
		, rSquaredCor1*100, rSquaredCor2*100
		, mape1*100, mape2*100)
	)

	# all variable names
	varNames <- unique(c(names(co1), names(co2)))
	# create empty data.frame
	ret <- data.frame(variable = varNames,
		coef1 = NA, coef2 = NA, coefRel = NA,
		contr1 = NA, contr2 = NA, contrDiff = NA,
		stringsAsFactors=FALSE
	)

	# fill data.frame
	ret$coef1 <- round(co1, 6)[ret$variable]
	ret$coef2 <- round(co2, 6)[ret$variable]
	ret$coefRel <-ret$coef2/ret$coef1

	ret$contr1 <- round(contr1, 4)[ret$variable] 
	ret$contr2 <- round(contr2, 4)[ret$variable] 
	ret$contrDiff <- ret$contr2 - ret$contr1

	grid.table(ret)

}
