
load("model/model.RData")


partab <- partable(fit)

# Fs
fatage <- faytable(fit)
fatage <- as.data.frame(fatage)

# Ns
natage <- as.data.frame(ntable(fit))

# Catch - estimates, sops & CATONs
catab <- as.data.frame(catchtable(fit,obs=TRUE))
colnames(catab) <- c("Catch", "Low", "High","Catch.SOP")

cn <-read.ices("data/catch data/cn.dat")
cw <-read.ices("data/catch data/cw.dat")
lw <-read.ices("data/catch data/lw.dat")
dw <-read.ices("data/catch data/dw.dat")
lf <-read.ices("data/catch data/lf.dat")

catab$Land.SOP <-rowSums(cn*lw*lf,na.rm=TRUE)
catab$Disc.SOP <-rowSums(cn*dw*(1-lf),na.rm=TRUE)

caton <-read.csv("data/catch data/IC.caton.csv")



# TSB
tsb <- as.data.frame(tsbtable(fit))
colnames(tsb) <- c("TSB", "Low", "High")

# Summary Table
tab.summary <- cbind(as.data.frame(summary(fit)), tsb)
tab.summary <- cbind(tab.summary, catab)

sag.summary <-tab.summary[,c(2,1,3,5,4,6,8,7,9,11,10,12,14,13,15,16:18)]
names(sag.summary) <-c("Rec.Low","Rec.Est","Rec.High","SSB.Low","SSB.Est","SSB.High",
                       "Fbar.Low","Fbar.Est","Fbar.High","TSB.Low","TSB.Est","TSB.High",
                       "Catch.Low","Catch.Est","Catch.High","Catch.SOP","Land.SOP","Disc.SOP")

sag.summary$Disc.ICES <-sag.summary$Land.ICES <-sag.summary$Catch.ICES <- NA

sag.summary[rownames(sag.summary) %in% caton$Year,"Catch.ICES"] <-caton$catch
sag.summary[rownames(sag.summary) %in% caton$Year,"Disc.ICES"] <-caton$dis+caton$bms
sag.summary[rownames(sag.summary) %in% caton$Year,"Land.ICES"] <-caton$land

sag.summary$Catch.ICES[is.na(sag.summary$Catch.ICES)] <-sag.summary$Catch.SOP[is.na(sag.summary$Catch.ICES)]
sag.summary$Disc.ICES[is.na(sag.summary$Disc.ICES)] <-sag.summary$Disc.SOP[is.na(sag.summary$Disc.ICES)]
sag.summary$Land.ICES[is.na(sag.summary$Land.ICES)] <-sag.summary$Land.SOP[is.na(sag.summary$Land.ICES)]



load("model/forecast.RData")

sag.summary[nrow(sag.summary)+1,"Rec.Low"] <-attributes(FC[[1]])$tab[2,"rec:low"]
sag.summary[nrow(sag.summary),"Rec.Est"] <-attributes(FC[[1]])$tab[2,"rec:median"]
sag.summary[nrow(sag.summary),"Rec.High"] <-attributes(FC[[1]])$tab[2,"rec:high"]
sag.summary[nrow(sag.summary),"SSB.Low"] <-attributes(FC[[1]])$tab[2,"ssb:low"]
sag.summary[nrow(sag.summary),"SSB.Est"] <-attributes(FC[[1]])$tab[2,"ssb:median"]
sag.summary[nrow(sag.summary),"SSB.High"] <-attributes(FC[[1]])$tab[2,"ssb:high"]



mohns_rho <- stockassessment::mohn(ret)
mohns_rho <- as.data.frame(t(mohns_rho))

## Write tables to output directory
write.taf(
  c("partab", "natage", "fatage", "mohns_rho","sag.summary"),row.names=TRUE,
  dir = "output/tables"
)

saveConf(fit$conf,file="output/tables/run.cfg")