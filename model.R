## Run analysis, write model results
## This is the spict model. It is not stable, sometimes it converges and sometimes it does not

## Before:
## After:
library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 
library(icesTAF)
library(spict)
library(xlsx)

mkdir("model")

try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)

base::load("data/processed_data_various.RData")

EndYear = read.table(paste0(wdir,"/bootstrap/data/thisyear.txt"))[[1]]



#' 
#+ echo = FALSE
#knitr::opts_chunk$set(comment=NA, fig.width=6, fig.height=8)
#'
#' Notes
#' -----
#' * Use help to see the documentation of a function (e.g. ?get.par or 
#'   help("get.par", "spict"))
#' * The spict vignette is a long-form documentation that explains the 
#'   functionality of spict using code examples. You can open it using
#'   `vignette("vignette", "spict")`
#'
#' 1.1 Install TMB and SPiCT
#' --------------------------
#' 
#devtools::install_github("DTUAqua/spict/spict")
inpDir <- ("bootstrap/data")



# Get peltic data for biomass index
pil <- read.csv(file.path(inpDir, "Index_core.csv"))
pil$Quarter = 4 # the survey was done in the 4th quarter
pil$Year = pil$year; pil$year <- NULL
pil$Peltic = pil$biomass..t.; pil$biomass..t. <- NULL

# Insert missing quarters - quick hacky approach...
missingqfor = pil$Year
missingq=c(rep(1,length(missingqfor)),rep(2,length(missingqfor)),rep(3,length(missingqfor)))
missingqfor = c(missingqfor,missingqfor,missingqfor)
addme = data.frame('Year'=missingqfor,'Peltic'=rep(NA,length(missingqfor)),'Quarter'=missingq)
pil = rbind(pil,addme)
pil<- pil[order(as.Date(paste(pil$Year,pil$Quarter,sep='-01-0'))),]

# Also in the 2021 run were blank years going back to 2002, and for some reason the model script breaks without these (possibly because it shortens the catch timeseries somewhere)
addmetoo = data.frame('Year'=c(rep(2002,4),rep(2003,4),rep(2004,4),rep(2005,4),rep(2006,4),rep(2007,4),rep(2008,4),rep(2009,4),rep(2010,4),rep(2011,4),rep(2012,4)),'Peltic'=rep(NA,44),'Quarter'=c(1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4))
pil = rbind(addmetoo,pil)


# Add in landings from the previous section
pil=merge(pil,landSeason,all = T)





##################################################################
########################################

inp<-list()

inp$timeC <- seq(2013,EndYear+3/4,by=1/4)
inp$obsC <- pil$landings[pil$Year>2012]
inp$timeI <- seq(2013,EndYear+3/4,by=1/4) 
inp$obsI <- pil$Peltic[pil$Year>2012]

inp$nseasons=4
inp$splineorder=3
inp$seasontype<-1 # 1 for B-splines, 2 for stochastic differential equations
inp$dtc<-0.25
#inp$dteuler<-1/16
inp$priors$logbkfrac <- c(log(0.5),0.5,1) # How do we know what priors to choose? This one seems to be the prior defining the level of depletion
#inp$priors$logn <- c(log(1.478), 0.6, 1) # summary(fit) says this is logn  ~  dnorm[log(2), 2^2] when undefined
# 

# If evidence or expert knowledge allows to infer that there was low or no exploitation before the
# beginning of the available data: initial depletion level could be assumed to be close to the carrying
# capacity (e.g. inp$priors$logbkfrac <- c(log(0.8),0.5,1))
# - If evidence or expert knowledge allows to infer that there was high exploitation before the
# beginning of the available data: initial depletion level could be assumed to be a certain fraction of
# the carrying capacity (e.g. inp$priors$logbkfrac <- c(log(0.2),0.5,1))

#inp$priors$logn <- c(log(1.478), 0.6, 1)

#inp$optimiser.control= list(iter.max = 1e10, eval.max = 1e10)

plot(1:1); dev.new() # Make a window 

inp <- check.inp(inp)
plotspict.data(inp)
fit <- fit.spict(inp)
plot(fit)


## for acceptance of the model:
#1) model converges
#2) variances are finit for all parameters:
all(is.finite(fit$sd))
#3) check residuals
fit <- calc.osa.resid(fit)
dev.off(); plot(1:1); dev.new()
plotspict.diagnostic(fit)
#4) retrospective analysis.
ret<- retro(fit, nretroyear = 2)
plotspict.retro(ret)
#5) realistic production curve. Bmsy/k should be between 0.1 and 0.9
calc.bmsyk(fit)
#6) uncertainty. intervals shouldn't span more than 1 order of magnitude
calc.om(fit)
#7) sensitivity to initial values
sens.ini <- check.ini(fit, ntrials=30     ) # JR - issue here as resmat seems to vary.'Distance' should be close to 0 for converged runs but isn't always. "The distance should preferably be close to zero. If that is not the case further investigation is required, i.e. inspection of objective function values, differences in results and residual diagnostics etc. should be performed"



# Save fitted model to R object
save(list=c("fit","inp"), file='model/fittedmodel.rdata')
