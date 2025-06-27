
##########################################################
#
#   model_sam.forecast.R 
#  
#########################################################
library(stockassessment)
#

#~~~~~~~~~~~~~Reference points~~~~~~
#~~~~~~~~~~~~~~~
# WKBGAD 2024
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Fmsy       <- 0.28
Fmsy_lower <- 0.184
Fmsy_upper <- 0.41 
Fpa         <- 0.42 
Flim        <- 1.10
Blim        <- 8542
Bpa         <- 11870.0
MSY_Btrigger<- 12877

## Previous advice
prev.advice <-31565 # given for 2025
tac.cur <-10195 # 2025

load("model/model.Rdata")

NSIM                <- 5001

## Define years
final.yr              <- max(fit$data$years)   # base year = last data year/last year in assessment
first.yr             <- min(fit$data$years)   # first year = first data year
nyears                 <- (final.yr-first.yr)+1

## Catch intermediate year
#catch_int_year  <- tac.cur #The TAC for this year

# Forecast settings
R.yrs <- first.yr:final.yr # recruitment years to resample from

# ---- Use ten year average for L-D partition and bio data
av.yrs <-(final.yr-9):final.yr

# ---- Use five year average for catch mean weights
cw.yrs <-(final.yr-4):final.yr

#----- Use 3 year average selection pattern
sel.yrs <-(final.yr-2):final.yr

#----- Final assessment year F - needed to run the forecast through 2021 to get numbers/SSB at start of intermediate year
f_final <-fbartable(fit)[as.character(final.yr),"Estimate"]

f_sq <-f_final # downward trend, so use last year as Fsq

# Want to use different year range for mean catch weights & other things (disc/land fractions)
# last 5 years instead of last 10
# Adjust the catch weights in the fit s.t we replicate the most recent 5 values twice
replace.yrs <-(final.yr-9):(final.yr-5)
fit$data$catchMeanWeight[rownames(fit$data$catchMeanWeight) %in% replace.yrs,,] <-fit$data$catchMeanWeight[rownames(fit$data$catchMeanWeight) %in% cw.yrs,,]
fit$data$landMeanWeight[rownames(fit$data$landMeanWeight) %in% replace.yrs,,] <-fit$data$landMeanWeight[rownames(fit$data$landMeanWeight) %in% cw.yrs,,]
fit$data$disMeanWeight[rownames(fit$data$disMeanWeight) %in% replace.yrs,,] <-fit$data$disMeanWeight[rownames(fit$data$disMeanWeight) %in% cw.yrs,,]
# Replace zeros with NA
fit$data$landMeanWeight[fit$data$landMeanWeight<=0] <-NA
fit$data$disMeanWeight[fit$data$disMeanWeight<=0] <-NA



FC<-list()

scen <- list(
  "MSY approach: F_MSY" = list(fval = c(f_final, f_sq, Fmsy, Fmsy)),
  "Precautionary approach: Fpa" = list(fval = c(f_final,f_sq, Fpa, Fpa)),
  "FMSY upper" = list(fval = c(f_final, f_sq, Fmsy_upper, Fmsy_upper)),
  "FMSY lower" = list(fval = c(f_final, f_sq, Fmsy_lower, Fmsy_lower)),
  "F = 0" = list(fval = c(f_final, f_sq, NA, NA),fscale=c(NA,NA,0.0,0.0)),
  "F = Flim" = list(fval = c(f_final, f_sq, Flim, Flim)),
  "Fsq"  = list(fval = c(f_final, f_sq, f_sq, f_sq))
#  "0.05*Fsq" = list(fval = c(f_final, NA, 0.05*f_sq,0.05*f_sq)),
#  "0.25*Fsq" = list(fval = c(f_final, NA, 0.25*f_sq,0.25*f_sq)),
#  "0.5*Fsq" = list(fval = c(f_final, NA, 0.5*f_sq,0.5*f_sq)),
#  "0.75*Fsq" = list(fval = c(f_final, NA, 0.75*f_sq,0.75*f_sq))
#  "hit Blim" = list(fval = c(f_final, NA, NA,NA), nextssb = c(NA, NA, Blim, Blim)),
#  "hit Bpa" = list(fval = c(f_final, NA, NA,NA), nextssb = c(NA, NA, Bpa, Bpa)) ,
#  "hit MSY Btrigger" = list(fval = c(f_final, NA, NA, NA), nextssb = c(NA, NA, MSY_Btrigger, MSY_Btrigger)),
#  "SSB Y+2 = SSB Y+1"  = list(fval = c(f_final, NA, NA, NA), nextssb = c(NA, NA, SSBnext, SSBnext))
#  "Try SSB Y+2 = SSB Y+1"  = list(fval = c(f_final, NA, 0.163, NA), nextssb = c(NA, NA, NA, SSBnext))
)

for(i in seq(scen)){
  set.seed(23456)
  ARGS <- scen[[i]]
  ARGS <- c(ARGS,
            list(fit = fit, catchval.exact=c(NA,NA,NA,NA),
                 ave.years=av.yrs, rec.years=R.yrs, overwriteSelYears=sel.yrs,
                 year.base=final.yr,label=names(scen)[i], 
                 splitLD = TRUE,deterministic=FALSE, processNoiseF=TRUE,
                 useSWmodel=TRUE,nosim=NSIM,savesim=TRUE))  
  FC[[i]] <- do.call(forecast, ARGS)
  
  print(paste0("forecast : ", "'", names(scen)[i], "'", " is complete"))
}



#  Need to use optimiser to hit the SSB targets - the forecast fucntion with nextssb doesn't seem to work with appropriate precision perhpas due to the
# very high biomass & F values required to reach some of these targets
optim_B_fsq <- function (f2target, target, fscale_init, fval_init, catchval_init){
  set.seed(23456)
  f1 <- forecast(fit, fscale=c(NA,NA,NA,1), fval = c(f_final,fval_init,f2target,NA), catchval.exact = c(NA,catchval_init,NA,NA),
                 ave.years=av.yrs, rec.years=R.yrs, overwriteSelYears=sel.yrs,useSWmodel=TRUE,nosim=NSIM)
  ssb <- attr(f1,"shorttab")[3,4]
  out <- (ssb - target)
  return(out)
}


#Blim
val <- uniroot(optim_B_fsq,interval=c(0,10.0), target=Blim, fscale_init=NA, fval_init=f_sq, catchval_init=NA)$root
set.seed(23456)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(NA,NA,NA,1), fval = c(f_final,f_sq,val,NA), catchval.exact=c(NA,NA,NA,NA), ave.years=av.yrs, rec.years=R.yrs, 
                               label="SSBnext=Blim", overwriteSelYears=sel.yrs, splitLD=TRUE,useSWmodel=TRUE,nosim=NSIM,
                               savesim=TRUE)

#  Bpa
val <- uniroot(optim_B_fsq,interval=c(0,10.0), target=Bpa, fscale_init=NA, fval_init=f_sq, catchval_init=NA)$root
set.seed(23456)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(NA,NA,NA,1), fval = c(f_final,f_sq,val,NA), catchval.exact=c(NA,NA,NA,NA), ave.years=av.yrs, rec.years=R.yrs, 
                               label="SSBnext=Bpa", overwriteSelYears=sel.yrs, splitLD=TRUE,useSWmodel=TRUE,nosim=NSIM,
                               savesim=TRUE)

# MSY Btrigger
val <- uniroot(optim_B_fsq,interval=c(0,10.0), target=MSY_Btrigger, fscale_init=NA, fval_init=f_sq, catchval_init=NA)$root
set.seed(23456)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(NA,NA,NA,1), fval = c(f_final,f_sq,val,NA), catchval.exact=c(NA,NA,NA,NA), ave.years=av.yrs, rec.years=R.yrs, 
                               label="SSBnext=MSY Btrigger", overwriteSelYears=sel.yrs, splitLD=TRUE,useSWmodel=TRUE,nosim=NSIM,
                               savesim=TRUE)


Btarget <-attr(FC[[1]],"shorttab")[3,3]
val <- uniroot(optim_B_fsq,interval=c(0,10.0), target=Btarget, fscale_init=NA, fval_init=f_sq, catchval_init=NA)$root
set.seed(23456)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(NA,NA,NA,1), fval = c(f_final,f_sq,val,NA), catchval.exact=c(NA,NA,NA,NA), ave.years=av.yrs, rec.years=R.yrs, 
                               label="SSBnext=SSB", overwriteSelYears=sel.yrs, splitLD=TRUE,useSWmodel=TRUE,nosim=NSIM,
                               savesim=TRUE)




set.seed(23456)
FC[[length(FC)+1]] <- forecast(fit, fscale=c(NA,NA,1,1), fval = c(f_final,f_sq,NA,NA), catchval.exact=c(NA,NA,NA,NA), ave.years=av.yrs, rec.years=R.yrs, 
                               label="F=F2025", overwriteSelYears=sel.yrs, splitLD=TRUE,useSWmodel=TRUE,nosim=NSIM,
                               savesim=TRUE)

nms <-NULL
for (i in 1:length(FC)){
  nms <-c(nms,attr(FC[[i]],"label"))
}

names(FC) <-nms

save(FC, file="model/forecast.RData")

