
# == testcase
#library(modgeneric)
#.libPaths(Sys.getenv("R_LIBS_USER"))

# local
wd <- getwd() 
packageLoc <- paste0(wd, "/modgeneric/R/")

# manual for updating and testing
report <- list.files(packageLoc, full.names=TRUE, pattern = ".R$")
for(rs in report){source(rs)}

# ============
# data example
# ============
data(mtcars)
dfmod <- Hartnagel

# ====================================
# specific implementations for bayesLM
# ====================================
# bugsParams
bugsParamsBayes <- list(debug = TRUE
	, DIC = TRUE
	, bugs.directory="C:/Program Files (x86)/OpenBUGS/OpenBUGS323"
	, program = "OpenBUGS"
	, working.directory = "temp"
)

# error dist
errorDistBayes <- list(vars = "fconvict", distr = "dnorm" , tauVar = "tau"
	, tauDistr = "dgamma(0.001,0.001)", tauStart = 0)
# bayesParams
bayesParamsBayes <- list(nc = 3, ni = 4000, nb = 1000, nt = 1)
varsIndepBayes <- list(
	  list(vars = "tfr"		, distr = "dnorm(0.0,1.0E-8)", startVal = 0)
	, list(vars = "partic"	, distr = "dnorm(0.0,1.0E-8)", startVal = 0)
	, list(vars = "degrees"	, distr = "dnorm(0.0,1.0E-8)", startVal = 0)
	, list(vars = "mconvict", distr = "dnorm(0.0,1.0E-8)", startVal = 0)	
)
baselineBayes <- list(vars = "bzero", distr = "dnorm(0.0,1.0E-8)", startVal = 0)

# model set
modelSetBayes <- dfmod
# model params
modelParamsBayes <- list(bugsParams = bugsParamsBayes, bayesParams = bayesParamsBayes)
# model variables
modelVarsBayes <- list(errorDist = errorDistBayes, varsIndep = varsIndepBayes, baseline = baselineBayes)
# model Type
modelTypeBayes <- "BayesLm"

# create model object
modelOutBayes <- modGenCreate(modelType = modelTypeBayes, modelSet = modelSetBayes
	, modelParams = modelParamsBayes, modelVars = modelVarsBayes)
# run model
modelRunBayes <- modGenRun(modelOutBayes)


# =====================================
# specific implementations for LinearLM
# =====================================
'
varsIndepLinear <- list(
	  list(vars = "tfr")
	, list(vars = "partic")
	, list(vars = "degrees")
	, list(vars = "mconvict")
)
baselineLinear <- list(vars = "bzero")
varsDepLinear <- list(vars = "fconvict")

# model set
modelSetLinear <- dfmod 
# model variables
modelVarsLinear <- list(varsDep = varsDepLinear, varsIndep = varsIndepLinear, baseline = baselineLinear)
# model Type
modelTypeLinear <- "LinearLm"

# create model object
modelOutLinear <- modGenCreate(modelType = modelTypeLinear
	, modelSet = modelSetLinear, modelVars = modelVarsLinear)
# run model
modelRunLinear <- modGenRun(modelOutLinear)

# =======================
# run reporting functions
# =======================
# contrib plot test
modGenPlotContr(modelRunLinear, "year")
modGenPlotContr(modelRunBayes, "year")

# avm plot test
modGenPlotAvm(modelRunLinear, "year")
modGenPlotAvm(modelRunBayes, "year")

# summary test
modGenExportContrib(modelRunLinear, "year", filename = "./testout/contribTabLinear.csv")
modGenExportContrib(modelRunBayes, "year", filename = "./testout/contribTabBayes.csv")

# compare model output
modGenCompare(modelRunLinear, modelRunBayes)

# compare model avms
modGenCompareAvm(modelRunLinear, modelRunBayes, "year", "year" , dy=TRUE)
'