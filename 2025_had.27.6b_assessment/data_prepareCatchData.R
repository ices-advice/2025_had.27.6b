##   Script for creating catch data
## 

library(FLCore)
library(stockassessment)
library(mgcv)
library(tidyr)
library(openxlsx)
library(ggplot2)

source("utilities_intercatch WGCSE.R")

outroute.tab <-"data/catch data/"
#inroute.tab.wg <-"catch data/wg2023/"

inroute.tab <-"boot/data/"
ic.dir <-"boot/data/Intercatch/age/"

#set data_yr
last_yr<-2024

ages <-seq(1,8,1)

stock.name <-"had.27.6b"

bms_yrs <-c(2016,2019:2023)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Total landings/discards -by age
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Read in the catch numbers and mean weights at-age from 2023 benchmark
cn <-xtab2taf(read.ices(paste0(inroute.tab,"cn.dat")))
cw <-xtab2taf(read.ices(paste0(inroute.tab,"cw.dat")))
ln <-xtab2taf(read.ices(paste0(inroute.tab,"ln.dat")))
lw <-xtab2taf(read.ices(paste0(inroute.tab,"lw.dat")))
dn <-xtab2taf(read.ices(paste0(inroute.tab,"dn.dat")))
dw <-xtab2taf(read.ices(paste0(inroute.tab,"dw.dat")))
bn <-xtab2taf(read.ices(paste0(inroute.tab,"bms.dat")))
bw <-xtab2taf(read.ices(paste0(inroute.tab,"dw.dat")))


###########################################################
#  Get the new data from IC

#~~~ Catch data from Intercatch
age_range <-0:15
# Catch
infiles <-paste0(ic.dir,"had.27.6b_all_",last_yr,"_ages.zip")
#ic.dat <-read.canum.rsd(infiles,age_range)
ic.dat <-read.canum.rsd(infiles,c(min(age_range),max(age_range)))


catch.no <-ic.dat$canum[,as.character(age_range)]/1000

catch.mean.wt <-ic.dat$catwt[,as.character(age_range)]/1000

catch.wt <-ic.dat$yld

#Landings
infiles <-paste0(ic.dir,"had.27.6b_lan_",last_yr,"_ages.zip")
ic.dat <-read.canum.rsd(infiles,c(min(age_range),max(age_range)))

lan.no <-ic.dat$canum[,as.character(age_range)]/1000
lan.mean.wt <-ic.dat$catwt[,as.character(age_range)]/1000

lan.wt <-ic.dat$yld

#Discards
infiles <-paste0(ic.dir,"had.27.6b_dis_",last_yr,"_ages.zip")
ic.dat <-read.canum.rsd(infiles,c(min(age_range),max(age_range)))

dis.no <-ic.dat$canum[,as.character(0:max(as.numeric(names(ic.dat$canum))))]/1000
dis.mean.wt <-ic.dat$catwt[,as.character(0:max(as.numeric(names(ic.dat$canum))))]/1000

dis.wt <-ic.dat$yld

#BMS 
if (last_yr %in% bms_yrs){
infiles <-paste0(ic.dir,"had.27.6b_bms_",last_yr,"_ages.zip")
ic.dat <-read.canum.rsd(infiles,c(min(age_range),max(age_range)))

#bms.no <-ic.dat$canum[,as.character(0:max(as.numeric(names(ic.dat$canum))))]/1000
#bms.mean.wt <-ic.dat$catwt[,as.character(0:max(as.numeric(names(ic.dat$canum))))]/1000

bms.wt <-ic.dat$yld
}else{
#  bms.no <-rep(0,length(dis.no))
#  names(bms.no) <-names(dis.no)
#  bms.mean.wt <-rep(0,length(dis.no))
#  names(bms.mean.wt) <-names(bms.no)
  bms.wt <-0
}

###.####################################################
#  JOIN OLD and NEW data

plusgp <-as.numeric(names(cn)[ncol(cn)])

ags<-names(catch.no)
catch.no$Year <-last_yr
catch.no <-catch.no[,c("Year",ags)]
catch.mean.wt$Year <-last_yr
catch.mean.wt <-catch.mean.wt[,c("Year",ags)]
M <-which(colnames(catch.no)==plusgp)

cn.new <-catch.no
cw.new <-catch.mean.wt
cw.new[,M] <-rowSums(cw.new[,(M:ncol(cw.new))]*cn.new[,(M:ncol(cn.new))],na.rm=TRUE)/rowSums(cn.new[,(M:ncol(cn.new))])
cn.new[,M] <-rowSums(cn.new[,(M:ncol(cn.new))])

cn.new <-cn.new[,colnames(cn)]
cw.new <-cw.new[,colnames(cw)]

ags<-names(lan.no)
lan.no$Year <-last_yr
lan.no <-lan.no[,c("Year",ags)]
lan.mean.wt$Year <-last_yr
lan.mean.wt <-lan.mean.wt[,c("Year",ags)]
M <-which(colnames(lan.no)==plusgp)

ln.new <-lan.no
lw.new <-lan.mean.wt
lw.new[,M] <-rowSums(lw.new[,(M:ncol(lw.new))]*ln.new[,(M:ncol(ln.new))],na.rm=TRUE)/rowSums(ln.new[,(M:ncol(ln.new))])
ln.new[,M] <-rowSums(ln.new[,(M:ncol(ln.new))])

ln.new <-ln.new[,colnames(ln)]
lw.new <-lw.new[,colnames(lw)]


ags<-names(dis.no)
dis.no$Year <-last_yr
dis.no <-dis.no[,c("Year",ags)]
dis.mean.wt$Year <-last_yr
dis.mean.wt <-dis.mean.wt[,c("Year",ags)]
M <-which(colnames(dis.no)==plusgp)

dn.new <-dis.no
dw.new <-dis.mean.wt
dw.new[,M] <-rowSums(dw.new[,(M:ncol(dw.new))]*dn.new[,(M:ncol(dn.new))],na.rm=TRUE)/rowSums(dn.new[,(M:ncol(dn.new))])
dn.new[,M] <-rowSums(dn.new[,(M:ncol(dn.new))])

dn.new <-dn.new[,colnames(dn)]
dw.new <-dw.new[,colnames(dw)]

cna.n <-rbind(cn,cn.new)
lna.n <-rbind(ln,ln.new)
dna.n <-rbind(dn,dn.new)

cwa.n <-rbind(cw,cw.new)
lwa.n <-rbind(lw,lw.new)
dwa.n <-rbind(dw,dw.new)


rownames(cna.n) <-rownames(lna.n) <-rownames(dna.n) <-rownames(cwa.n) <-rownames(lwa.n) <-rownames(dwa.n) <-cna.n$Year

#rownames(bms.no) <-bms.no$Year


##  Output the data ##############
# Append caton to time series
caton <-read.csv(paste0(inroute.tab,"IC.caton.csv"))
caton[nrow(caton)+1,] <-c(last_yr,catch.wt,lan.wt,dis.wt,bms.wt)
write.csv(caton,file=paste0(outroute.tab,"IC.caton.csv"))


write.ices(as.matrix(cna.n[,-1]),file=paste0(outroute.tab,"cn.dat"))
write.ices(as.matrix(lna.n[,-1]),file=paste0(outroute.tab,"ln.dat"))
write.ices(as.matrix(dna.n[,-1]),file=paste0(outroute.tab,"dn.dat"))
write.ices(as.matrix(cwa.n[,-1]),file=paste0(outroute.tab,"cw.dat"))
write.ices(as.matrix(lwa.n[,-1]),file=paste0(outroute.tab,"lw.dat"))
write.ices(as.matrix(dwa.n[,-1]),file=paste0(outroute.tab,"dw.dat"))


#  Need to sort out the BMS
bna.n <-round(cna.n[,-1] - lna.n[,-1] - dna.n[,-1],4)
bna.n[!(rownames(bna.n) %in% bms_yrs),] <-0
bna.n[bna.n<0] <-0
write.ices(as.matrix(bna.n),file=paste0(outroute.tab,"bms.dat"))

# Add to discards for purpose of assessment
tot.dna <-bna.n+dna.n[,-1]
tot.dna <-cbind(Year=cna.n$Year,tot.dna)

write.ices(as.matrix(tot.dna[,-1]),file=paste0(outroute.tab,"tot.dn.dat"))

lf <-lna.n[,-1]/cna.n[,-1]
write.ices(as.matrix(lf),file=paste0(outroute.tab,"lf.dat"))

#  SOPS ##########################################

landings <-rowSums(lna.n[,-1]*lwa.n[,-1],na.rm=TRUE)
discards <-rowSums(dna.n[,-1]*dwa.n[,-1],na.rm=TRUE)
catch <-rowSums(cna.n[,-1]*cwa.n[,-1],na.rm=TRUE)
bms <-rowSums(bna.n*dwa.n[,-1],na.rm=TRUE)

catch.sop <-data.frame(year=names(landings),landings=landings,discards=discards,bms=bms,catch=catch)
write.taf(catch.sop,file.path(outroute.tab,"catch.sop.csv"))






