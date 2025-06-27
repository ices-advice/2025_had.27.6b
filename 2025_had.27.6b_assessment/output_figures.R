library(ggplot2)
library(tidyverse)
library(viridis)

source("utilities_intercatch WGCSE.R")
source("utilities_plot_functions.R")

load("model/model.RData")

out.dir <-"output/figures/"

################################################
# Intercatch
################################################

ic.dir <-"boot/data/Intercatch/"
stock.name <-"had.27.6b"
#  For the final year
y <-2024

StockOverviewFile <- paste0(ic.dir,"age/StockOverview.txt")
NumbersAtAgeLengthFile <- unz(paste0(ic.dir,"age/Numbers_at_age_and_mean_weights_at_age.zip"),"NumbersAtAgeLength.txt")
Wdata <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile) 
NumbersAtAgeLengthFile <- unz(paste0(ic.dir,"age/Numbers_at_age_and_mean_weights_at_age.zip"),"NumbersAtAgeLength.txt")
Ndata <- readNumbersAtAgeLength(NumbersAtAgeLengthFile) 


par(mfrow=c(1,1))
png(filename = paste0(out.dir,stock.name,"_LandWt_",y,".png"),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotCatchWt(Wdata,plotType="LandWt",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
            byLandBioImp=TRUE,byDisBioImp=FALSE,byDisWtImp=TRUE,markBioImp=TRUE,markDisWtImp=TRUE,
            countryColours=TRUE,set.mar=TRUE,individualTotals=FALSE,allOnOnePlot=TRUE)
dev.off()

png(filename = paste0(out.dir,stock.name,"_LandPercent_",y,".png"),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotCatchWt(Wdata,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
            countryColours=TRUE,set.mar=TRUE,individualTotals=FALSE)
dev.off()

png(filename = paste0(out.dir,stock.name,"_DisWt_",y,".png"),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotCatchWt(Wdata,plotType="DisWt",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
            byLandBioImp=TRUE,byDisBioImp=TRUE,byDisWtImp=TRUE,markBioImp=TRUE,markDisWtImp=TRUE,
            countryColours=TRUE,set.mar=TRUE,individualTotals=FALSE)
dev.off()

png(filename = paste0(out.dir,stock.name,"_DisRate_",y,".png"),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotCatchWt(Wdata,plotType="DisRate",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
            byLandBioImp=TRUE,byDisBioImp=FALSE,byDisWtImp=TRUE,markBioImp=TRUE,markDisWtImp=TRUE,
            countryColours=TRUE,set.mar=TRUE,individualTotals=FALSE)
dev.off()

png(filename = paste0(out.dir,stock.name,"_CatchWt_",y,".png"),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotCatchWt(Wdata,plotType="CatchWt",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
            byLandBioImp=TRUE,byDisBioImp=FALSE,byDisWtImp=TRUE,markBioImp=TRUE,markDisWtImp=TRUE,
            countryColours=TRUE,set.mar=TRUE,individualTotals=FALSE)
dev.off()

#  Explore the post-raising/allocations data (inc LFD)

OutWdata <- readOutputCatchWt(unz(paste0(ic.dir,"age/",stock.name,"_all_",y,"_ages.zip"),"CatchAndSampleDataTables.txt")) 
OutNdata <- readOutputNumbersAtAgeLength(unz(paste0(ic.dir,"age/",stock.name,"_all_",y,"_ages.zip"),"CatchAndSampleDataTables.txt"))


aggWdata <- aggregateCatchWt(OutWdata,byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
                             byLandBioImp=TRUE,byDisBioImp=FALSE,byDisWtImp=TRUE)

par(mfrow=c(1,1))

png(filename = paste(out.dir,stock.name,"_DisRate-post-agg_",y,".png",sep=""),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotCatchWt(OutWdata,plotType="DisRate",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
            byLandBioImp=TRUE,byDisBioImp=FALSE,byDisWtImp=TRUE,markBioImp=TRUE,markDisWtImp=TRUE,
            countryColours=TRUE,set.mar=TRUE,individualTotals=FALSE)
dev.off()

png(filename = paste(out.dir,stock.name,"_CatchWt_agg_",y,".png",sep=""),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotCatchWt(OutWdata,plotType="CatchWt",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,
            byLandBioImp=TRUE,byDisBioImp=FALSE,byDisWtImp=TRUE,markBioImp=TRUE,markDisWtImp=TRUE,
            countryColours=TRUE,set.mar=TRUE,individualTotals=FALSE)
dev.off()


png(filename = paste(out.dir,stock.name,"_totnum-at-age_agg_by_cat",y,".png",sep=""),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotTotalNumbersAtAgeLength.by.category(OutNdata) 
dev.off()

png(filename = paste(out.dir,stock.name,"_totnum-at-age_agg_",y,".png",sep=""),width = 700, height = 480, units = "px", pointsize = 12)  ## width = 480
plotTotalNumbersAtAgeLength.hd(OutNdata) 
dev.off()



##################################################
# Official landings plot
#################################################
pname <-"official.landings.png"

official.land <-read.csv(file.path("boot/data/official.landings.csv"))
names(official.land)[1] <-"Country"
off.land <-gather(official.land,yearcol,landings,X1996:X2024)
off.land$year <-substr(off.land$yearcol,2,5)
off.land <-off.land[,c("Country","year","landings")]
off.land$landings[off.land$landings=="-"] <-0
off.land <-subset(off.land,Country!="Total")
off.land$landings <-as.numeric(off.land$landings)
off.land$landings[is.na(off.land$landings)] <-0
off.land$Country[grep("UK",off.land$Country)] <-"UK"

l.by.yr <-aggregate(landings~year,off.land,sum)


ggplot(off.land,aes(x=as.factor(year),y=landings,fill=Country))+
  geom_bar(stat="identity")+
  xlab("year")+ 
  theme_linedraw()+
  ylab("landings (tonnes)")+
  scale_x_discrete(breaks = c(as.character(seq(1995,2025,5))))+
  scale_y_continuous(expand=c(0,0),limits=c(0,1.05*max(l.by.yr$landings)))+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  theme(legend.text = element_text(size=14),legend.title = element_text(size=16))+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 
ggsave(file.path(out.dir,pname), width=12, height=8)

###################################################
# Total landings/discards time sereis

catch.sop <-read.csv("data/catch data/catch.sop.csv")
tot.cat <-pivot_longer(catch.sop[,c("year","landings","discards","bms")],c("landings","discards","bms"),names_to="type",values_to="data")

pname <-"catch.time.series.png"
yname <-"tonnes"
xname <-"year"

plot.catch(tot.cat,yname,xname)
ggsave(file.path(out.dir,pname), width=12, height=8)
####################################################
# Discard rate
catch.sop$disc.prop <-(catch.sop$discards+catch.sop$bms)/catch.sop$catch
  
pname <-"discard.proportion.time.series.png"
yname <-"proportion"
xname <-"year"

png(filename=paste0(out.dir,pname), width=1000, height=700)
plot(catch.sop$year,catch.sop$disc.prop,type="l",ylab=yname,xlab=xname,yaxs="i",ylim=c(0,1.05*max(catch.sop$disc.prop)),lwd=2)
dev.off()

##############################################
# Weight at age plots
##############################################
lwa <-read.ices("data/catch data/lw.dat")
dwa <-read.ices("data/catch data/dw.dat")
cwa <-read.ices("data/catch data/cw.dat")

pname <-"weights.at.age.png"
yname <-"weight (kg)"
xname <-"year"

wts <-rbind(cbind(year=rownames(lwa),as.data.frame(lwa),type="landings"),
            cbind(year=rownames(dwa),as.data.frame(dwa),type="discards"),
            cbind(year=rownames(cwa),as.data.frame(cwa),type="catch"))
#wts$data[wts$data==0] <-NA
wts[wts<=0] <-NA

wt.long <-pivot_longer(wts,cols=as.character(1:8),names_to="age",values_to="data")


plot.by.age(subset(wt.long,type!="catch"),yname,xname)
ggsave(file.path(out.dir,pname), width=12, height=8)


##############################################
# Catch weight at age plots
##############################################

pname <-"catch.weights.at.age.png"
yname <-"weight (kg)"
xname <-"year"

plot.by.age(subset(wt.long,type=="catch"),yname,xname)
ggsave(file.path(out.dir,pname), width=12, height=12)

###############################################
#Catch weight by type & age separately
###############################################
Cat.Type <-"catch"
pname <-paste0(Cat.Type,".weights.by.age.png")

ggplot(subset(wt.long,type==Cat.Type),aes(as.numeric(year),y=data))+geom_line()+facet_wrap(~age,scales="free")+expand_limits(y=0)+
  xlab("year")+ylab("weight (kg)")+theme_bw()+theme(text=element_text(size=18))
ggsave(file.path(out.dir,pname), width=10, height=6)

wt.long$type <-factor(wt.long$type,levels=c("discards","landings","catch"))

pname <-paste0("catch.disc.lan.weights.by.age.png")
ggplot(wt.long,aes(as.numeric(year),y=data,group=as.factor(type),colour=as.factor(type)))+geom_line(lwd=1.25)+facet_wrap(~age,scales="free")+expand_limits(y=0)+
  xlab("year")+ylab("weight (kg)")+theme_bw()+theme(text=element_text(size=18))+labs(color="")
ggsave(file.path(out.dir,pname), width=10, height=6)


##################################################
# Discards/landings by year and age
##################################################
ln <-read.ices("data/catch data/ln.dat")
dn <-read.ices("data/catch data/dn.dat")
bn <-read.ices("data/catch data/bms.dat")

catch.num <-rbind(cbind(year=rownames(ln),as.data.frame(ln),type="landings"),
                         cbind(year=rownames(dn),as.data.frame(dn),type="discards"),
                         cbind(year=rownames(bn),as.data.frame(bn),type="bms"))
                   
cn.long <-pivot_longer(catch.num,cols=as.character(1:8),names_to="age",values_to="data")

pname <-"catch.at.age.bar.plot.png"

yrs.to.plot <-(max(as.numeric(cn.long$year))-11):max(as.numeric(cn.long$year))

ggplot(subset(cn.long,year %in% yrs.to.plot), aes(fill=type, y=data, x=age)) + 
  geom_bar(position="stack", stat="identity")+facet_wrap(~year)
ggsave(file.path(out.dir,pname), width=12, height=8)



##################################################
# Discard rate at age
##################################################
pname <-"discard.rate.at.age.png"
yname <-"proportion"
xname <-"year"

dis.p <-(as.data.frame(dn)+as.data.frame(bn))/(as.data.frame(dn)+as.data.frame(bn)+as.data.frame(ln))
dis.p$type <-"discard rate"
dis.p$year <-rownames(dis.p)

dp.long <-pivot_longer(dis.p,cols=as.character(1:8),names_to="age",values_to="data")

plot.by.age(dp.long,yname,xname)
ggsave(file.path(out.dir,pname), width=12, height=8)

##############################
#Plots of catch numbers at age

cn <-read.ices("data/catch data/cn.dat")
catch.age <-as.data.frame(cn)
catch.age$year <-as.numeric(rownames(cn))
catch.age <-pivot_longer(catch.age, cols=colnames(cn),names_to="age",values_to="data")
catch.age$age <-as.numeric(catch.age$age)

pname <-"catch.at.age.bubble.plot.png"

catch.at.age.bubble.plot(catch.age,st=FALSE)
ggsave(file.path(out.dir,pname), width=12, height=8)

#~~~~~~~~~

pname <-"catch.at.age.bubble.plot.standardised.png"

all.dat <-catch.age %>% group_by(year) %>% mutate(prop=data/sum(data,na.rm=TRUE))
all.dat <-all.dat %>% group_by(age) %>% mutate(st.prop=(prop-mean(prop,na.rm=TRUE))/sd(prop,na.rm=TRUE))

in.dat <-data.frame(year=all.dat$year,age=all.dat$age,data=all.dat$st.prop)

catch.at.age.bubble.plot(in.dat,st=TRUE)
ggsave(file.path(out.dir,pname), width=12, height=8)


#~~~~~~~~~~~~
#Log CN- by age and year
in.dat <-data.frame(year=catch.age$year,age=catch.age$age,data=log(catch.age$data))
pname <-"log.catch.n.by.year.png"
plot.idx.by.age(in.dat,"log catch",xname="year")
ggsave(file.path(out.dir,pname), width=12, height=8)

# Catch curves
pname <-"commercial.catch.curve.png"
in.dat$cohort <-in.dat$year-in.dat$age
plot.catch.curve(in.dat,"log index","year")
ggsave(file.path(out.dir,pname), width=12, height=8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   Survey exploratory plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################################
# Indices
###################################################
# Old survey
surveys <-stockassessment:::read.surveys(paste0("boot/data/MSS_indices.dat"))
s <-1
s.name <-names(surveys)[s]

tmp <-as.data.frame(surveys[[s]])
ages <-colnames(tmp)
tmp$year <-as.numeric(rownames(tmp))
s.dat <-pivot_longer(tmp,cols=as.character(ages),names_to="age",values_to="data")

s.dat <-s.dat %>% group_by(age) %>% mutate(logidx =log(data/mean(data,na.rm=TRUE)))
s.dat$cohort <-s.dat$year-as.numeric(s.dat$age)

# By year  
in.dat <-data.frame(year=s.dat$year,age=s.dat$age,data=s.dat$logidx)

pname <-paste(s.name,".by.year.png",sep="")
plot.idx.by.age(in.dat,"log mean standardised index",xname="year")

ggsave(file.path(out.dir,pname), width=6, height=4)


# By cohort - although the plotting function expects 'year' variable 
in.dat <-data.frame(year=s.dat$cohort,age=s.dat$age,data=s.dat$logidx)

pname <-paste(s.name,".by.cohort.png",sep="")
plot.idx.by.age(in.dat,"log mean standardised index",xname="cohort")
ggsave(file.path(out.dir,pname), width=6, height=4)



pname <-paste(s.name,".catch.curves.png",sep="")

in.dat <-data.frame(year=s.dat$year,age=s.dat$age,cohort=s.dat$cohort,data=log(s.dat$data))
plot.catch.curve(in.dat,"log index","year")
ggsave(file.path(out.dir,pname), width=6, height=4)


# Modelled Q3 survey
surveys <-stockassessment:::read.surveys(paste0("data/indices/q3index.dat"))

n.surv <-length(surveys)

for (s in 1:n.surv){
  s.name <-names(surveys)[s]
  
  tmp <-as.data.frame(surveys[[s]])
  ages <-colnames(tmp)
  ages <-as.character(1:8)
  tmp$year <-as.numeric(rownames(tmp))
  s.dat <-pivot_longer(tmp,cols=as.character(ages),names_to="age",values_to="data")
  
  s.dat <-s.dat %>% group_by(age) %>% mutate(logidx =log(data/mean(data,na.rm=TRUE)))
  s.dat$cohort <-s.dat$year-as.numeric(s.dat$age)
  
  # By year  
  in.dat <-data.frame(year=s.dat$year,age=s.dat$age,data=s.dat$logidx)
  
  pname <-paste(s.name,".by.year.png",sep="")
  plot.idx.by.age(in.dat,"log mean standardised index",xname="year")
  
  ggsave(file.path(out.dir,pname), width=6, height=4)
  
  # By cohort - although the plotting function expects 'year' variable 
  in.dat <-data.frame(year=s.dat$cohort,age=s.dat$age,data=s.dat$logidx)
  
  pname <-paste(s.name,".by.cohort.png",sep="")
  plot.idx.by.age(in.dat,"log mean standardised index",xname="cohort")
  ggsave(file.path(out.dir,pname), width=6, height=4)
  
  # Scatterplots
  n.dat <-pivot_wider(s.dat[,c("age","cohort","data")],id_cols=cohort,names_from=c(age),values_from=data)
  
  pname <-paste(s.name,".scatterplot.png",sep="")
  png(filename = paste(out.dir,pname),width = 600, height = 600) 
  survey.splom(n.dat[,ages],plusgp=TRUE,s.name)
  dev.off()
  
  pname <-paste(s.name,".catch.curves.png",sep="")

  in.dat <-data.frame(year=s.dat$year,age=s.dat$age,cohort=s.dat$cohort,data=log(s.dat$data))
  plot.catch.curve(in.dat,"log index","year")
  ggsave(file.path(out.dir,pname), width=6, height=4)
}

############# Survey index model ###########################
missing.yrs <-c(2000,2004,2010)

#load("data/Q3models_final8plus.Rdata")
load("boot/data/Q3idx_lastyr.Rdata")
mod.idx <-list()
mod.idx[["Last year"]] <-as.data.frame(idx)
mod.idx[["Last year"]]$year <-rownames(idx)

load("boot/data/Q3idx.Rdata")
#mod.idx[["WGCSE"]] <-as.data.frame(models[[1]]$idx)
#mod.idx[["WGCSE"]]$year <-rownames(models[[1]]$idx)
mod.idx[["WGCSE"]] <-as.data.frame(idx$idx)
mod.idx[["WGCSE"]]$year <-rownames(idx$idx)



tmp <-bind_rows(mod.idx,.id="model")

s.dat <-pivot_longer(tmp,cols=colnames(tmp)[!(colnames(tmp)%in%c("year","model"))],names_to="age",values_to="data")
s.dat$data[s.dat$year %in% missing.yrs] <-NA

s.dat <-s.dat %>% group_by(age,model) %>% mutate(logidx =log(data/mean(data,na.rm=TRUE)))
s.dat$year <-as.numeric(s.dat$year)
s.dat$cohort <-as.numeric(s.dat$year)-as.numeric(s.dat$age)

# Natural scale index
xname <-"year"
yname <-"index"
pname <-paste0("survey comparison.png")
ggplot(s.dat,
       aes(year,y=data,group=as.character(model),colour=model))+
  geom_line(lwd=1.2)+facet_wrap(~age,scale="free")+
  ylab(yname)+
  xlab(xname)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none",
        axis.title = element_text(size=16),
        axis.text = element_text(size=14),
        strip.text=element_text(size=14,angle = 0))
ggsave(file.path(out.dir,pname), width=12, height=8)


#Modelled index plot with CIs
#mod.idx <-lapply(models[[1]][c("idx","lo","up")],function(x)as.data.frame(x))

#names(mod.idx$lo) <-names(mod.idx$up) <-names(mod.idx$idx)

#mod.idx <-lapply(mod.idx,function(x)cbind(x,year=rownames(models[[1]]$idx)))
mod.idx <-lapply(idx,function(x)as.data.frame(cbind(x,year=rownames(idx$idx))))
tmp <-lapply(mod.idx,function(x)pivot_longer(x,cols=colnames(x)[!colnames(x)%in%c("year")],names_prefix="V",names_to="age"))

s.dat <-data.frame(year=tmp$idx$year,age=tmp$idx$age,idx=as.numeric(tmp$idx$value),lo=as.numeric(tmp$lo$value),up=as.numeric(tmp$up$value))

s.dat$idx[s.dat$year %in% missing.yrs] <-NA
s.dat$lo[s.dat$year %in% missing.yrs] <-NA
s.dat$up[s.dat$year %in% missing.yrs] <-NA


#s.dat <-s.dat %>% group_by(age,val) %>% mutate(logidx =log(data/mean(data,na.rm=TRUE)))
s.dat$year <-as.numeric(as.character(s.dat$year))
s.dat$cohort <-s.dat$year-as.numeric(s.dat$age)
s.dat$age <-as.character(s.dat$age)


max.age <-max(s.dat$age)
s.dat$age[s.dat$age==max.age] <-paste0(max.age,"+")

xname <-"year"
yname <-"index"
pname <-"modelled_index.output.png"
ggplot(subset(s.dat,age>0),
       aes(year,y=idx))+
  geom_line()+geom_point()+
  geom_ribbon(aes(ymin=lo, ymax=up, x=year), colour=NA, alpha = 0.3,show.legend=FALSE)+
  facet_wrap(~age,scale="free")+
  ylab(yname)+
  xlab(xname)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="bottom",
        axis.title = element_text(size=16),
        axis.text = element_text(size=14),
        strip.text=element_text(size=14,angle = 0),
        legend.title=element_blank(),
        legend.text=element_text(size=14))
ggsave(file.path(out.dir,pname), width=12, height=8)


###################Assessment output #########################

taf.png(paste0(out.dir,"summary"), width = 1600, height = 2000)
plot(fit)
dev.off()

taf.png(paste0(out.dir,"SSB"))
ssbplot(fit, addCI = TRUE)
dev.off()

taf.png(paste0(out.dir,"Fbar"))
fbarplot(fit, xlab = "", partial = FALSE)
dev.off()

taf.png(paste0(out.dir,"Rec"))
recplot(fit, xlab = "")
dev.off()

taf.png(paste0(out.dir,"Catch"))
catchplot(fit, xlab = "")
dev.off()

taf.png(paste0(out.dir,"surveyqs"))
if(nrow(fit$conf$keyLogFpar)>2){
  qtableplot(qtable(fit))
}
dev.off()

taf.png(paste0(out.dir,"F-at-age"))
fay.tab <- faytable(fit)
plot(as.numeric(row.names(fay.tab)),fay.tab[,dim(fay.tab)[2]], type='l', ylim=c(0,max(fay.tab[,dim(fay.tab)[2]])),
     ylab="F at age",xlab="")
for(i in 1:(dim(fay.tab)[2])) { lines(as.numeric(row.names(fay.tab)),fay.tab[,i],col=i) }
legend("topright",inset=0.02,legend=paste("Age",colnames(fay.tab)),col=1:ncol(fay.tab),lwd=2,lty=1,cex=0.7,ncol=2)
dev.off()


# Selectivity
taf.png(paste0(out.dir,"F-selectivity"))

Fsel <- faytable(fit)/rowSums(faytable(fit))
plot(rownames(Fsel),Fsel[,1],xlab="Year", main = "Selectivity in F",type="n",ylim=range(Fsel),ylab="")
for(i in 1:ncol(Fsel)){
  lines(rownames(Fsel),Fsel[,i],col=i,lwd=2)
}
legend("topleft",inset=0.02,legend=paste("Age",colnames(Fsel)),col=1:ncol(Fsel),lwd=2,lty=1,cex=0.7,ncol=2)
dev.off()

taf.png(paste0(out.dir,"F-selectivity.by_decade"),width=2000,height=1600)

# by decade
Fsel <- faytable(fit)/rowSums(faytable(fit))

decade <- paste0(substr((rownames(Fsel)),start=1,stop = 3),"0")
dec_yr <- substr(rownames(Fsel),start=4,stop = 4)

par(mfrow=c(2,3))
for (d in unique(decade)){
  # get years
  idx <- which(decade %in% d)
  toplot <- Fsel[idx,]
  
  plot(colnames(toplot),toplot[1,],xlab="Year", main = "Selectivity in F",type="n",ylim=range(Fsel),ylab="")
  for(i in 1:nrow(toplot)){
    lines(colnames(toplot),toplot[i,],col=as.numeric(dec_yr[idx[i]]),lwd=2)
  }
  legend("bottomright",inset=0.02,legend=paste(rownames(toplot)),col=1:nrow(toplot),lwd=2,lty=1,cex=0.7,ncol=2)
  
}
dev.off()


taf.png(paste0(out.dir,"number.by.age"),width=2000,height=1600)
natage <- as.data.frame(ntable(fit))
natage <-rownames_to_column(natage,var="Year")
dat <-pivot_longer(natage,cols=as.character(1:8),names_to="age",values_to="numbers")
p1 <-ggplot(dat,aes(fill=age,y=numbers,x=Year))+geom_bar(position="fill",stat="identity")+scale_fill_viridis(discrete = T,direction=-1)+
  scale_x_discrete(breaks = as.character(unique(dat$Year)[c(TRUE,FALSE)]))+theme(text=element_text(size=16))+ylab("proportion by number")
print(p1)
dev.off()  

taf.png(paste0(out.dir,"bio.by.age"),width=2000,height=1600)
miny <-min(fit$data$years)
maxy <-max(fit$data$years)
swa <- exp(fit$pl$logSW)[1:(maxy-miny+1),]
batage <- as.data.frame(ntable(fit)*swa)

batage <-rownames_to_column(batage,var="Year")
dat <-pivot_longer(batage,cols=as.character(1:8),names_to="age",names_prefix="age",values_to="biomass")
p1 <-ggplot(dat,aes(fill=age,y=biomass,x=Year))+geom_bar(position="fill",stat="identity")+scale_fill_viridis(discrete = T,direction=-1)+
  scale_x_discrete(breaks = as.character(unique(dat$Year)[c(TRUE,FALSE)]))+theme(text=element_text(size=16))+ylab("biomass proportion")
print(p1)
dev.off()  



taf.png(paste0(out.dir,"retrospective"), width = 1600, height = 2000)
plot(ret)
dev.off()

#taf.png(paste0(out.dir,"retro.4plot"), width = 2000, height = 2000)
taf.png(paste0(out.dir,"retro_4plot"), width = 2000, height = 2000)
par(mfrow=c(2,2))
retro <-ret
if(!is.null(retro)){
  m0<-stockassessment::mohn(retro)
  m1<-stockassessment::mohn(retro, lag=1)
  recplot(retro); legend("topright", legend=bquote(rho[Mohn] == .(round(m0[1]*100))*"%"), bty="n") 
  ssbplot(retro); legend("topright", legend=bquote(rho[Mohn] == .(round(m0[2]*100))*"%"), bty="n") 
  fbarplot(retro, partial=FALSE); legend("topright", legend=bquote(rho[Mohn] == .(round(m0[3]*100))*"%"), bty="n") 
  catchplot(retro)
  
  #parplot(retro)
#  title(name, outer=TRUE, line=-2)
}
dev.off()


if(!is.null(res)){
  taf.png(paste0(out.dir,"one.step.res"), width = 1600, height = 2000)
  par(mfrow=c(1,1))
  plot(res)
#  title(name, outer=TRUE, line=-2)
  dev.off()
}

if(!is.null(perr)){
  taf.png(paste0(out.dir,"proc.res"), width = 1600, height = 2000)
  plot(perr)
#  title(name, outer=TRUE, line=-2)
#  par(mfrow=c(1,1))
  dev.off()
}

for (f in 1:fit$data$noFleets){
  taf.png(paste0(out.dir,"fit_fleet_",f), width = 1600, height = 2000)
  fitplot(fit,fleets=f)
  #  title(name, outer=TRUE, line=-2)
  dev.off()
}

taf.png(paste0(out.dir,"fit_stock.weights"), width = 2000, height = 2000)
bioplot(fit)
dev.off()

taf.png(paste0(out.dir,"fit_sim"), width = 2000, height = 2000)
plot(sim)
dev.off()

# plot
pdf(paste0(out.dir,"all_figures.pdf"), onefile=TRUE, width=11.7, height=8.3)
plotfit_extra(fit, retro=ret, residual=res)
dev.off()



#Comparison with last year
fit.cur <-fit

load("boot/data/model_lastyr.RData")

taf.png(paste0(out.dir,"SSB_compare"))
ssbplot(fit.cur, addCI = TRUE)
ssbplot(fit,add=TRUE,ci=FALSE,col="red")
dev.off()

taf.png(paste0(out.dir,"Fbar_compare"))
fbarplot(fit.cur, xlab = "", partial = FALSE)
fbarplot(fit,partial=FALSE,add=TRUE,ci=FALSE,col="red")
dev.off()

taf.png(paste0(out.dir,"Rec_compare"))
recplot(fit.cur, xlab = "")
recplot(fit,add=TRUE,ci=FALSE,col="red")
dev.off()

taf.png(paste0(out.dir,"Catch_compare"))
catchplot(fit.cur, xlab = "")
catchplot(fit,add=TRUE,ci=FALSE,col="red")
dev.off()


