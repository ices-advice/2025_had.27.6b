#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#           model_sam.assessment.R
#
#  Takes the R data object as input, adjusts the configuration as per the benchmark
#  and runs the SAM assessment.
#
#  Before: data/data.RData
#  After: model/model.RData - containing fit, residuals, retro
#         pdf containing model output
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

last.yr <-2024

# directories
model.dir <- "model/"
output.dir <- "model/"

# base model settings
rec.age <- 1
plusgroup <- 8
survey.ma <-8

# import data options -----------------------------------------####

# load data required
load("data/data.RData", verbose = TRUE)


runName <- "had.27.6b_WGCSE"
label<-c(runName, "final_run") 

conf<-defcon(dat)
ages <-conf$minAge:conf$maxAge

conf$fbarRange <- c(2,5)

fplateau <-6 # 
conf$keyLogFsta[1,] <-0:(length(ages)-1)
conf$keyLogFsta[1,which(ages>fplateau)] <-conf$keyLogFsta[1,which(ages==fplateau)]

#biopar
conf$stockWeightModel <- 1
conf$keyStockWeightMean <- c(0,1,2,3,4,5,6,7)
conf$keyStockWeightObsVar <- c(0,0,0,0,0,0,0,0)

conf$keyLogFpar[2,] <-c(0:2,rep(3,5))
conf$keyLogFpar[3,] <-c(4:6,rep(7,5))

conf$keyQpow[3,] <-c(0,rep(1,7))

sdyrs <-2011:last.yr
conf$keyXtraSd <-cbind(rep(1,length(sdyrs)*length(rec.age:plusgroup)),as.matrix(expand.grid(sdyrs,rec.age:plusgroup,0)))

#  run
par<-defpar(dat,conf)
fit<-sam.fit(dat,conf,par)

#modeltable(fit)

# residuals and retro
res<-residuals(fit)
perr <- procres(fit)
retmat<-cbind(last.yr-0:4, 3000, last.yr-0:4)
ret<-retro(fit, year=retmat)

set.seed(123)
sim <-simstudy(fit,nsim=100)

save(conf,fit,res,perr,ret,sim,file=paste0("model/model.RData"))









