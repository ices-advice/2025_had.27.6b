################################## readStockOverview ###########################

readStockOverview <- function(StockOverviewFile,NumbersAtAgeLengthFile){

Wdata <- read.table(StockOverviewFile,header=TRUE,sep="\t")

names(Wdata)[3] <- "SeasonType"
names(Wdata)[7] <- "Fleet"
names(Wdata)[10] <- "CatchWt"
names(Wdata)[11] <- "CatchCat"
names(Wdata)[12] <- "ReportCat"
Wdata$CatchCat <- substr(Wdata$CatchCat,1,2)
# 
Wdata <-subset(Wdata,Discards.Imported.Or.Raised=="Imported")
#
Wdata <- Wdata[,-ncol(Wdata)]

Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
names(Ndata)[7] <- "CatchCat"
names(Ndata)[9] <- "Fleet"
#  Fix to match 1st 2 letters of CatchCat in Wdata
levels(Ndata$CatchCat)[levels(Ndata$CatchCat)=="L"] <- "La"
levels(Ndata$CatchCat)[levels(Ndata$CatchCat)=="D"] <- "Di"


Wtdata <- merge(Wdata,Ndata[,c(1,2,3,4,5,7,9,11)],by=c("Year","Stock","Area","Season","Fleet","Country","CatchCat"),all.x=TRUE)
Wtdata$Sampled <- ifelse(is.na(Wtdata$SampledCatch),FALSE,TRUE)
#Wtdata$SampledCatch <- NULL

return(Wtdata)
}

################################## readNumbersAtAgeLength ######################

readNumbersAtAgeLength <- function(NumbersAtAgeLengthFile){

Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
names(Ndata)[7] <- "CatchCat"
names(Ndata)[8] <- "ReportCat"
names(Ndata)[9] <- "Fleet"
Ndata <- Ndata[,-ncol(Ndata)]
ageNames <- names(Ndata)[16:ncol(Ndata)]

#  Fix to match 1st 2 letters of CatchCat in Wdata
levels(Ndata$CatchCat)[levels(Ndata$CatchCat)=="L"] <- "La"
levels(Ndata$CatchCat)[levels(Ndata$CatchCat)=="D"] <- "Di"

#line was changed to make it work by Rui 12/05/2015
ages <- as.numeric(substr(ageNames,(nchar(ageNames)),nchar(ageNames)))#ages <- as.numeric(substr(ageNames,4,nchar(ageNames)))
allAges <- min(ages):max(ages)
missingAges <- allAges[allAges %in% ages]

return(Ndata)
}

############################## readOutputCatchWt ###############################

readOutputCatchWt <- function(CatchAndSampleDataTablesFile){

dataFile <- CatchAndSampleDataTablesFile
datText <- readLines(dataFile)
table1Start <- which(datText=="TABLE 1.")
table2Start <- which(datText=="TABLE 2.")
nRows <- table2Start - 3 - table1Start - 6

datNames <- scan(dataFile,sep="\t",skip=table1Start+3,nlines=1,what=character(),quiet=TRUE)
tab1 <- read.table(dataFile,sep="\t",skip=table1Start+5,nrows=nRows,header=FALSE,as.is=TRUE)
names(tab1) <- datNames
names(tab1)[4] <- "CatchCat"
names(tab1)[6] <- "RsdOrImp"
names(tab1)[12] <- "CatchWt"
names(tab1)[15] <- "OffLand"
names(tab1)[16] <- "Sampled"
names(tab1)[18] <- "NumLenSamp"
names(tab1)[19] <- "NumLenMeas"
names(tab1)[20] <- "NumAgeSamp"
names(tab1)[21] <- "NumAgeMeas"
tab1$Area <- gsub(" ","",tab1$Area)

Wdata <- tab1
Wdata$CatchCat <- substr(Wdata$CatchCat,1,2)
Wdata$RsdOrImp <- ifelse(substr(Wdata$RsdOrImp,1,1)=="R","Rsd","Imp")
Wdata$Sampled <- ifelse(substr(Wdata$Sampled,1,1)=="S",TRUE,FALSE)

return(Wdata)
}

########################## readOutputNumbersAtAgeLength ########################

readOutputNumbersAtAgeLength <- function(CatchAndSampleDataTablesFile){

dataFile <- CatchAndSampleDataTablesFile
datText <- readLines(dataFile)
table2Start <- which(datText=="TABLE 2.")

datNames <- scan(dataFile,sep="\t",skip=table2Start+3,nlines=1,what=character(),quiet=TRUE)
tab2 <- read.table(dataFile,sep="\t",skip=table2Start+6,header=FALSE,as.is=TRUE)
names(tab2) <- datNames
tab2$Area <- gsub(" ","",tab2$Area)
#tab2$AgeOrLength <- factor(tab2$AgeOrLength)
#tab2$ID <- paste(tab2$Area,tab2$Fleet,tab2$CatchCategory,tab2$Country,tab2$Season)

names(tab2)[4] <- "CatchCat"
names(tab2)[6] <- "RsdOrImp"
names(tab2)[12] <- "CatchWt"
names(tab2)[15] <- "OffLand"
names(tab2)[16] <- "Sampled"
names(tab2)[24] <- "NumLenSamp"
names(tab2)[25] <- "NumLenMeas"
names(tab2)[26] <- "NumAgeSamp"
names(tab2)[27] <- "NumAgeMeas"
tab2$Area <- gsub(" ","",tab2$Area)

Ndata <- tab2
Ndata$CatchCat <- substr(Ndata$CatchCat,1,2)
Ndata$RsdOrImp <- ifelse(substr(Ndata$RsdOrImp,1,1)=="R","Rsd","Imp")
Ndata$Sampled <- ifelse(substr(Ndata$Sampled,1,1)=="S",TRUE,FALSE)

return(Ndata)
}

############################ aggregateStockOverview ############################

aggregateCatchWt <- function(dat,byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,byLandBioImp=TRUE,byDisBioImp=TRUE,byDisWtImp=TRUE) {

if (is.null(dat$RsdOrImp)) dat$RsdOrImp <- "Imp"

summaryNames <- NULL
if (byFleet) 
  summaryNames <- c(summaryNames,"Fleet")
if(byCountry) 
  summaryNames <- c(summaryNames,"Country")
if(bySeason)
  summaryNames <- c(summaryNames,"Season")
if (byArea) 
  summaryNames <- c(summaryNames,"Area")
if (byLandBioImp) 
  summaryNames <- c(summaryNames,"LandBioImp")
if (byDisBioImp) 
  summaryNames <- c(summaryNames,"DisBioImp")
if (byDisWtImp) 
  summaryNames <- c(summaryNames,"DisWtImp")

if (any(dat$SeasonType=="Year") & any(dat$SeasonType=="Quarter")) {
    id <- paste(dat$Fleet,dat$Country,dat$Area)    
    Yid <- id[dat$SeasonType=="Year"]
    year <- dat[dat$SeasonType=="Year",][1,"Season"]
    Ydat <- dat[(id %in% Yid),]
    aggYdat <- aggregate(CatchWt~Fleet+Country+Area+Sampled+RsdOrImp+CatchCat,data=Ydat,sum)
    Yland <- aggYdat[aggYdat$CatchCat=="La",]
    Ydis <- aggYdat[aggYdat$CatchCat=="Di",]
    catch <- merge(Yland,Ydis,by=c("Fleet","Country","Area"),all=TRUE)
    
    catch$Season <- year
    
    catch1 <- catch

    id <- paste(dat$Fleet,dat$Country,dat$Area)
    Yid <- paste(Ydat$Fleet,Ydat$Country,Ydat$Area)
    Qdat <- dat[!(id %in% Yid),]
    Qland <- Qdat[Qdat$CatchCat=="La",]
    Qdis <- Qdat[Qdat$CatchCat=="Di",]
    catch <- merge(Qland,Qdis,by=c("Fleet","Country","Area","Season"),all=TRUE)
    catch2 <- catch

    catch <- merge(catch1,catch2,all=TRUE)
} else {
    land <- dat[dat$CatchCat=="La",-which(names(dat)%in% "CatchCat")]
    dis <- dat[dat$CatchCat=="Di",-which(names(dat)%in% "CatchCat")]
    catch <- merge(land,dis,by=c("Year","Stock","Area","Season","Fleet","Country","Effort","SeasonType"),all=TRUE)
} 

names(catch)[names(catch)=="Sampled.x"] <- "LandBioImp"
names(catch)[names(catch)=="Sampled.y"] <- "DisBioImp"
names(catch)[names(catch)=="CatchWt.x"] <- "LandWt"
names(catch)[names(catch)=="CatchWt.y"] <- "DisWt"
names(catch)[names(catch)=="RsdOrImp.y"] <- "DisWtImp"

catch$LandBioImp[is.na(catch$LandBioImp)] <- FALSE
catch$DisBioImp[is.na(catch$DisBioImp)] <- FALSE
catch$DisWtImp[catch$DisWtImp=="Imp"] <- TRUE
catch$DisWtImp[catch$DisWtImp=="Rsd"] <- FALSE
catch$DisWtImp[is.na(catch$DisWtImp)] <- FALSE
catch$DisWtImp <- as.logical(catch$DisWtImp)

sum.na <- function(x){sum(x,na.rm=TRUE)}
stratumSummary <- eval(parse(text=paste("aggregate(cbind(LandWt,DisWt)~",paste(summaryNames,collapse="+"),",data=catch,sum.na,na.action=na.pass)",sep="")))

if (byLandBioImp) {
  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandBioImp,stratumSummary$LandWt,na.last=FALSE)),]
  if (byDisBioImp) {
  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandBioImp,stratumSummary$LandBioImp,stratumSummary$LandWt,na.last=FALSE)),]
  }
} else {
  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt,na.last=FALSE)),]
}
return(stratumSummary)                                                        
}

############################ plotAgeDistribution ###############################

plotAgeDistribution <- function(dat,plotType="perCent",ageCols=NULL) {

plotTypes <- c("perCent","frequency")

if (!(plotType %in% plotTypes)) 
  stop(paste("plotType needs to be one of the following:", paste(plotTypes,collapse=", ")))


if (is.null(ageCols)) ageCols <- 16:ncol(dat)

frog <- is.na(dat[,ageCols])
dat[,ageCols][frog] <- 0
sampAge <- dat

if (TRUE) {
par(mar=c(4,2,1,1)+0.1)
par(mfcol=c(4,2))

year <- sampAge$Year[1]

sampAge$AFC <- paste(sampAge$Area,sampAge$Fleet,sampAge$Country)
AFClist <- sort(unique(sampAge$AFC))
nAFC <- length(AFClist)

overallAgeDist <- list()
overallAgeDist[["La"]] <- colSums(sampAge[sampAge$CatchCat=="La" ,ageCols],na.rm=TRUE)/sum(sampAge[sampAge$CatchCat=="La" ,ageCols],na.rm=TRUE)
if (sum(sampAge$CatchCat=="Di")>0) {
overallAgeDist[["Di"]] <- colSums(sampAge[sampAge$CatchCat=="Di" ,ageCols],na.rm=TRUE)/sum(sampAge[sampAge$CatchCat=="Di" ,ageCols],na.rm=TRUE)
} else {
overallAgeDist[["Di"]] <- 0
}

ymax <- max(sampAge[sampAge$Country!="UK(Scotland)",ageCols],na.rm=TRUE)
for (i  in 1:nAFC) {
  for (catch in c("La","Di")) {
    for (season in 1:4) {
      if (sampAge[sampAge$AFC==AFClist[i],"Country"][1]=="UK(Scotland)" & catch=="Di" & season==1) {
        season <- year
      }
      indx <- sampAge$AFC==AFClist[i] & sampAge$CatchCat==catch & sampAge$Season==season
      dat <- sampAge[indx,]
      if (nrow(dat)==0) {
        plot(0,0,type="n",xlab="",ylab="")
      } else {
        ageDist <- as.matrix(dat[,ageCols],nrow=nrow(dat),ncol=length(ageCols))
        if (sum(ageDist)==0) {
          plot(0,0,type="n",xlab="",ylab="")
        } else {
          if (plotType=="perCent") ageDist <- ageDist/rowSums(ageDist)
          newYmax <- max(sampAge[sampAge$AFC==AFClist[i],ageCols],na.rm=TRUE)
          if (plotType=="perCent") newYmax <- max(ageDist)
          newYmax <- 1.05*newYmax
          #newYmax <- max(ageDist,na.rm=TRUE)
          barplot(ageDist,ylim=c(0,newYmax),las=2)
          box()
        }
      }
      title(paste(AFClist[i],catch,season),cex.main=0.7,line=0.5)
     }
  }
}
}

}

######################## plotTotalNumbersAtAgeLength ###########################

plotTotalNumbersAtAgeLength <- function(OutNdata) {
  N1 <- tapply(OutNdata$CANUM,list(OutNdata$AgeOrLength,OutNdata$CatchCat,OutNdata$RsdOrImp,OutNdata$Sampled),sum)/1e6
  
# Ffix for Logbook recorded discard numbers at age  
#  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),ncol=12,byrow=F,dimnames=list(rownames(N1),
#                  c("DisImpFill","LanImpFill","LogImpFill","DisRsdFill","LanRsdFill","LogRsdFill","DisImpSam","LanImpSam","LogImpSam","DisRsdSam","LanRsdSam","LogRsdSam"))))
  
#  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),ncol=8,byrow=F,dimnames=list(rownames(N1),c("DisImpFill","LanImpFill","DisRsdFill","LanRsdFill","DisImpSam","LanImpSam","DisRsdSam","LanRsdSam"))))
 
  #Fix for BMS landings at age  
  
  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),ncol=12,byrow=F,dimnames=list(rownames(N1),c("BMSImpFill","DisImpFill","LanImpFill",
                                                                                           "BMSRsdFill","DisRsdFill","LanRsdFill",
                                                                                           "BMSImpSam","DisImpSam","LanImpSam",
                                                                                           "BMSRsdSam","DisRsdSam","LanRsdSam"))))

    N2[is.na(N2)] <- 0
  N2 <- N2[,c("LanImpSam","LanImpFill","DisImpSam","DisImpFill","DisRsdFill")]    
  collist <- c("grey","blue","black","yellow","red")
  ymax <- 1.05*max(rowSums(N2))
  barplot(t(as.matrix(N2,nrow=13,ncol=4)),ylim=c(0,ymax),col=collist,xlab="age",ylab="numbers-at-age (millions)")
  box()
  legendtxt <- c("Sampled Landings","Unsampled Landings","Sampled Discards","Imported Unsampled Discards","Raised Unsampled Discards")                                            
  legend("topright",legend=legendtxt,col=collist,pch=19)  
  title("Total Catch Numbers At Age")  
}

#  New function to account for possible BMS and logbook discards
plotTotalNumbersAtAgeLength.hd <-function(OutNdata) {
  N1 <- tapply(OutNdata$CANUM,list(OutNdata$AgeOrLength,OutNdata$CatchCat,OutNdata$RsdOrImp,OutNdata$Sampled),sum)/1e6
  
  # Fix for Logbook recorded discard numbers at age  
  NC <-length(unique(OutNdata$CatchCat))*4
  
  cnames <-paste(rep(unique(OutNdata$CatchCat),4),rep(c("ImpFill","RsdFill","ImpSam","RsdSam"),each=length(unique(OutNdata$CatchCat))),sep="")
  
  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),ncol=NC,byrow=F,dimnames=list(rownames(N1),cnames)))
#                                                                            c("BMSImpFill","DisImpFill","LanImpFill","LogImpFill","BMSRsdFill","DisRsdFill","LanRsdFill","LogRsdFill","BMSImpSam","DisImpSam","LanImpSam","LogImpSam","BMSRsdSam","DisRsdSam","LanRsdSam","LogRsdSam"))))
  
  #  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),ncol=8,byrow=F,dimnames=list(rownames(N1),c("DisImpFill","LanImpFill","DisRsdFill","LanRsdFill","DisImpSam","LanImpSam","DisRsdSam","LanRsdSam"))))
  N2[is.na(N2)] <- 0
  N2 <- N2[,c("LaImpSam","LaImpFill","DiImpSam","DiImpFill","DiRsdFill")]    
  collist <- c("grey","blue","black","yellow","red")
  ymax <- 1.05*max(rowSums(N2))
  barplot(t(as.matrix(N2,nrow=13,ncol=4)),ylim=c(0,ymax),col=collist,xlab="age",ylab="numbers-at-age (millions)")
  box()
  legendtxt <- c("Sampled Landings","Unsampled Landings","Sampled Discards","Imported Unsampled Discards","Raised Unsampled Discards")                                            
  legend("topright",legend=legendtxt,col=collist,pch=19) 
  if (unique(OutNdata$AgeOrLengthType)=="Lngt"){
    title("Total Catch Numbers At Length")
  }else{
    title("Total Catch Numbers At Age")
  }
}

#  New function to account for possible BMS and logbook discards
plotTotalNumbersAtAgeLength.by.category <-function(OutNdata) {
  N1 <- tapply(OutNdata$CANUM,list(OutNdata$AgeOrLength,OutNdata$CatchCat),sum)/1e6

  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1)))
  names(N2) <-colnames(N1)
  N2[is.na(N2)] <- 0
  
  tmpN <-N2[,-which("La"==names(N2)),drop=FALSE]
  
  N3 <-data.frame(Landings=N2$La,Discards=rowSums(tmpN))
  rownames(N3) <-rownames(N1)
#  ,nrow=nrow(N1),ncol=16,byrow=F,dimnames=list(rownames(N1),
#                                                                            c("BMSImpFill","DisImpFill","LanImpFill","LogImpFill","BMSRsdFill","DisRsdFill","LanRsdFill","LogRsdFill","BMSImpSam","DisImpSam","LanImpSam","LogImpSam","BMSRsdSam","DisRsdSam","LanRsdSam","LogRsdSam"))))
  
  #  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),ncol=8,byrow=F,dimnames=list(rownames(N1),c("DisImpFill","LanImpFill","DisRsdFill","LanRsdFill","DisImpSam","LanImpSam","DisRsdSam","LanRsdSam"))))
 
#  N2 <- N2[,c("LanImpSam","LanImpFill","DisImpSam","DisImpFill","DisRsdFill")]    
  collist <- c("red","blue")
  ymax <- 1.05*max(rowSums(N3))
  if (unique(OutNdata$AgeOrLengthType)=="Lngt"){
    ytitle <-"numbers-at-length (millions)"
    mtitle <-paste0("Total Catch Numbers At Length: ",unique(OutNdata$Year))
    xtitle <-"length (mm)"
  }else{
    mtitle <-paste0("Total Catch Numbers At Age: ",unique(OutNdata$Year))
    ytitle <-"numbers-at-age (millions)"
    xtitle <-"age"
  }
  barplot(t(N3),ylim=c(0,ymax),col=collist,xlab=xtitle,ylab=ytitle)
  box()
  legendtxt <- c("Landings","Discards")                                            
  legend("topright",legend=legendtxt,col=collist,pch=19) 
  title(mtitle)
}


###################### aggTotalStockCatchWtByFleetGroup ########################

aggTotalCatchWtByFleetGroup <- function(OutWtData){

OWF <- OutWtData
OWF$Fleet <- substr(OWF$Fleet,1,7)
#OWF$Fleet[OWF$Fleet=="SSC_DEF"] <- "MIS_MIS"                                            
#OWF$Fleet[OWF$Fleet=="LLS_FIF"] <- "MIS_MIS"                                            
OWF$Fleet[OWF$Fleet=="FPO_CRU"] <- "MIS_MIS"                                            
OWF$Fleet[OWF$Fleet=="OTB_SPF"] <- "MIS_MIS"                                            
#OWF$Fleet[OWF$Fleet=="GTR_DEF"] <- "MIS_MIS"                                            
OWF$Fleet[OWF$Fleet=="PTB_all"] <- "MIS_MIS"                                            
OWF$Fleet <- factor(OWF$Fleet,c("OTB_DEF","OTB_CRU","SDN_DEF","GNS_DEF","TBB_DEF","SSC_DEF","LLS_FIF","GTR_DEF","MIS_MIS"))

WtInfo <- tapply(OWF$CatchWt,list(OWF$Fleet,OWF$RsdOrImp,OWF$CatchCat),sum,na.rm=T)/1000                        
WtInfo[is.na(WtInfo)] <- 0                    
WtSumm <- as.data.frame(matrix(WtInfo,nrow=nrow(WtInfo),ncol=4,byrow=F,dimnames=list(rownames(WtInfo),c("DisImp","DisRsd","LanImp","LanRsd"))))
WtSumm <- WtSumm[,c("LanImp","DisImp","DisRsd")]
#WtSumm <- WtSumm[rev(order(WtSumm[,"LanImp"])),]
WtPC <- 100*WtSumm/sum(WtSumm)
WtBoth <- cbind(WtSumm,WtPC)
names(WtBoth) <- c("LanTon","DisTonImp","DisTonRsd","LanPC","DisImpPC","DidRsdPC")
WtBoth$DisRate <- 100*rowSums(WtBoth[,2:3])/rowSums(WtBoth[,1:3])

return(WtBoth)
}

######################## aggTotalStockCatchWtByCountry #########################

aggTotalCatchWtByCountry <- function(OutWtData){

OWF <- OutWtData
WtInfo <- tapply(OWF$CatchWt,list(OWF$Country,OWF$RsdOrImp,OWF$CatchCat),sum,na.rm=T)/1000                        
WtInfo[is.na(WtInfo)] <- 0   
WtSumm <- as.data.frame(matrix(WtInfo,nrow=nrow(WtInfo),ncol=4,byrow=F,dimnames=list(rownames(WtInfo),c("DisImp","DisRsd","LanImp","LanRsd"))))
WtSumm <- WtSumm[rev(order(WtSumm[,"LanImp"])),c("LanImp","DisImp","DisRsd")]
WtPC <- 100*WtSumm/sum(WtSumm)

WtBoth <- cbind(WtSumm,WtPC)
names(WtBoth) <- c("LanTon","DisTonImp","DisTonRsd","LanPC","DisImpPC","DidRsdPC")
WtBoth$DisRate <- 100*rowSums(WtBoth[,2:3])/rowSums(WtBoth[,1:3])
                                                                                        
return(WtBoth)
}
    
############################ aggTotalStockCatchWt ##############################

aggTotalStockCatchWt <- function(OutWtData){

OWF <- OutWtData
WtInfo <- tapply(OWF$CatchWt,list(OWF$Sampled,OWF$RsdOrImp,OWF$CatchCat),sum,na.rm=T)/1000                        
WtInfo[is.na(WtInfo)] <- 0                    
WtSumm <- as.vector(unlist(WtInfo))
names(WtSumm) <- c("DisImpFill","DisImpSam","DisRsdFill","DisRsdSam","LanImpFill","LanImpSam","LanRsdFill","LanRsdSam")
WtSumm <- WtSumm[c("LanImpSam","LanImpFill","DisImpSam","DisImpFill","DisRsdFill")]

WtPC <- 100*WtSumm/sum(WtSumm)

WtBoth <- c(WtSumm,WtPC)
WtBoth <- as.data.frame(matrix(WtBoth,nrow=1,ncol=length(WtBoth)))
names(WtBoth) <- paste(c("LanImpSam","LanImpFill","DisImpSam","DisImpFill","DisRsdFill"),rep(c("Ton","PC"),rep(5,2)),sep="")

WtBoth$DisRate <- 100*sum(WtBoth[,2:3])/sum(WtBoth[,1:3])

return(WtBoth)
}        
    
###################################### plotCatchWt #############################

plotCatchWt <- function(dat,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,byLandBioImp=FALSE,byDisBioImp=FALSE,byDisWtImp=FALSE,
markBioImp=TRUE,markDisWtImp=TRUE,countryColours=NULL,set.mar=TRUE,individualTotals=TRUE,ymax=NULL,allOnOnePlot=FALSE){

plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRate","CatchRatio")
if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:",paste(plotTypes)))

stock <- dat$Stock[1]

impLand <- dat[dat$CatchCat=="La",]
impDis <- dat[dat$CatchCat=="Di",]

nArea <- nSeason <- nCountry <- nFleet <- 1

SeasonNames <- sort(unique(impLand$Season))
AreaNames <- sort(unique(impLand$Area))

countryLegend <- FALSE

if (byFleet) nFleet <- length(unique(impLand$Fleet))
if (byCountry) { 
  nCountry <- length(unique(impLand$Country))
  countryLegend <- TRUE
}    
if (byArea) nArea <- length(AreaNames)
if (bySeason) nSeason <- length(SeasonNames)
if (!byLandBioImp) markLandBio <- FALSE

if (allOnOnePlot) nArea <- 1
if (allOnOnePlot) nSeason <- 1

if (length(countryColours)==1 &&countryColours){
  countryColours <- data.frame(
  "Country"=c("Belgium","Denmark","France","Germany","Ireland","Netherlands","Norway","Poland","Spain","UK (England)","UK(Northern Ireland)","UK(Scotland)"),
  "Colour"=c("yellow3", "red", "darkblue", "black","green","orange","turquoise" ,"purple","yellow","magenta","magenta3","blue")
  , stringsAsFactors=FALSE)
  countryColours <- subset(countryColours,Country %in% unique(dat$Country))
}
if (length(countryColours)==1 && countryColours==FALSE){
  countryLegend <- FALSE
  countryColours <- data.frame(
  "Country"=c("Belgium","Denmark","France","Germany","Ireland","Norway","Netherlands","Poland","Spain","UK (England)","UK(Northern Ireland)","UK(Scotland)")
   , stringsAsFactors=FALSE)
  countryColours$Colour <- rep("grey",length(countryColours$Country))
}

stratumSummary <- aggregateCatchWt(dat,byFleet=byFleet,byCountry=byCountry,byArea=byArea,bySeason=bySeason,byLandBioImp=byLandBioImp,byDisBioImp=byDisBioImp,byDisWtImp=byDisWtImp)

if (byDisWtImp) {
  catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt,stratumSummary$DisWt),byrow=FALSE,ncol=3)
  catchData[stratumSummary$DisWtImp==FALSE,2] <- 0
  catchData[stratumSummary$DisWtImp==TRUE,3] <- 0
} else {
  catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt),byrow=FALSE,ncol=2)
}

if (set.mar) par(mar=c(8.5,4.5,1.5,1)+0.1) 
                                       
for (a in 1:nArea) {
  if (bySeason & !(byCountry | byFleet)) nSeason <- 1
  for (s in 1:nSeason) {
    area <- AreaNames[a]
    if (nArea==1) area <- AreaNames
    season <- SeasonNames[s]
    if (nSeason==1) season <- SeasonNames

    indx <- rep(TRUE,nrow(stratumSummary))
    if (bySeason & !byArea & (byCountry | byFleet)) indx <- stratumSummary$Season %in% season 
    if (!bySeason & byArea) indx <- stratumSummary$Area %in% area 
    if (bySeason & byArea & (byCountry | byFleet | byLandBioImp)) 
      indx <- stratumSummary$Area %in% area & stratumSummary$Season %in% season

    if (individualTotals) {
      sumLandWt <- sum(stratumSummary$LandWt[indx],na.rm=TRUE)
    } else {
      sumLandWt <- sum(stratumSummary$LandWt,na.rm=TRUE)
    }  
    if(byCountry) {
      print(names(stratumSummary))
      print(countryColours)
      colVec <- countryColours$Colour[match(stratumSummary$Country[indx],countryColours$Country)]
    } else {
      colVec <- "grey"
    }  
        
    if (plotType=="LandWt") yvals <- stratumSummary$LandWt[indx]
    if (plotType=="LandPercent") yvals <- 100*stratumSummary$LandWt[indx]/sumLandWt    
    if (plotType=="DisWt") yvals <- stratumSummary$DisWt[indx]
    if (plotType=="CatchWt") yvals <- t(catchData[indx,])
    if (plotType=="DisRatio") {
      yvals <- stratumSummary$DisWt[indx]/stratumSummary$LandWt[indx]
      yvals[yvals==Inf] <- NA
    }
    if (plotType=="DisRate") {
      print(stratumSummary[indx,])
      yvals <- 100*stratumSummary$DisWt[indx]/(stratumSummary$DisWt[indx] + stratumSummary$LandWt[indx])
      yvals[yvals==Inf] <- NA
    }
    if (plotType=="CatchRatio") {
      yvals <- 100*t(catchData[indx,]/matrix(rowSums(catchData[indx,]),byrow=F,nrow=sum(indx),ncol=ncol(catchData)))
      print(yvals)
    }
    if (!is.null(ymax)) newYmax <- ymax
    if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
    if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
    if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE)
    if (plotType %in% c("CatchWt","CatchRatio") &!byDisWtImp) colVec <- c("grey","black")
    if (plotType %in% c("CatchWt","CatchRatio") & byDisWtImp) colVec <- c("grey","black","red")
    if (plotType=="CatchWt") countryLegend <- FALSE
    if (plotType=="DisRate") newYmax <- 100 ## steveH
    if (markBioImp) newYmax <- 1.06*newYmax
    
    namesVec <- 1:sum(indx)
    if (byFleet) {
      namesVec=stratumSummary$Fleet[indx]
      if (byArea & nArea==1) namesVec <- paste(namesVec,stratumSummary$Area[indx])
      if (bySeason & nSeason==1) namesVec <- paste(namesVec,stratumSummary$Season[indx])
    }  
    if (!byFleet) {
      namesVec <- rep("",sum(indx)) 
      if (byCountry) namesVec <- paste(namesVec,stratumSummary$Country[indx])
      if (byArea & nArea==1) namesVec <- paste(namesVec,stratumSummary$Area[indx])
      if (bySeason & nSeason==1) namesVec <- paste(namesVec,stratumSummary$Season[indx])
    }
    
    cumulativeY <- cumsum(yvals)
    yvals[yvals>newYmax] <- newYmax

    if ((newYmax==-Inf)) {
      plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
      box()
    } else {
      b <- barplot(yvals,names=namesVec,las=2,cex.names=0.7,col=colVec,ylim=c(0,newYmax),yaxs="i")   ## cex.names=0.7
      box()
    }
    
    if (!bySeason & !byArea) title.txt <- paste(stock)
    if (!bySeason & byArea & nArea==1) title.txt <- paste(stock)
    if (!bySeason & byArea & nArea!=1) title.txt <- paste(stock,area)
    if (bySeason & !byArea & !(byCountry | byFleet | byLandBioImp)) title.txt <- paste(stock)
    if (bySeason & !byArea & (byCountry | byFleet | byLandBioImp)) title.txt <- paste(stock,season)
    if (bySeason & nSeason==1 & byArea & nArea==1) title.txt <- paste(stock)
    if (bySeason & nSeason==1 & byArea & nArea!=1) title.txt <- paste(stock,area)
    if (bySeason & nSeason!=1 & byArea & nArea==1) title.txt <- paste(stock,season)
    if (bySeason & nSeason!=1 & byArea & nArea!=1) title.txt <- paste(stock,area,season)
    title.txt <- paste(title.txt,plotType)
    title(title.txt)

    if (newYmax!=-Inf) {
 #   if (markBioImp) {
      nSampled <- sum(stratumSummary$LandBioImp[indx],na.rm=TRUE)
      if (length(b)>1 & nSampled>0) {
        arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
        arrows(b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
        text((b[nSampled]+b[1])/2,newYmax*104/106,"landings bio data",cex=0.8)
        text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"no landings bio data",cex=0.8)
      }
      if (plotType %in% c("DisWt","CatchWt","DisRatio","CatchRatio")) {
        if (length(b)>1) {
          Dindx <- stratumSummary$DisBioImp[indx]=="TRUE"
          if (sum(indx)>0) {
            points(b[Dindx],rep(0.01*newYmax,sum(Dindx)),pch=1)
          }
        }
      }
    } 
#    if (markDisImp & byDisImp) {
#      Iindx <- stratumSummary$DisImported[indx]
#      points(b[Iindx],rep(0.05*newYmax,sum(Iindx)),pch="+")
#    }  
#}
#    if (bySampled & byDisImp & markSampled) {
#      nLBDB <- sum(stratumSummary$LandSampled[indx] & stratumSummary$DisSampled[indx],na.rm=TRUE)
#      nLBDW <- sum(stratumSummary$LandSampled[indx] & !stratumSummary$DisSampled[indx] & stratumSummary$DisImported[indx],na.rm=TRUE)
#      nLBDN <- sum(stratumSummary$LandSampled[indx] & !stratumSummary$DisSampled[indx] & !stratumSummary$DisImported[indx],na.rm=TRUE)
#      nLNDB <- sum(!stratumSummary$LandSampled[indx] & stratumSummary$DisSampled[indx],na.rm=TRUE)
#      nLNDW <- sum(!stratumSummary$LandSampled[indx] & !stratumSummary$DisSampled[indx] & stratumSummary$DisImported[indx],na.rm=TRUE)
#      nLNDN <- sum(!stratumSummary$LandSampled[indx] & !stratumSummary$DisSampled[indx] & !stratumSummary$DisImported[indx],na.rm=TRUE)
#      if (length(b)>1) {
#        if (nLBDB>0) {
#          arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nLBDB]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
#          text((b[nLBDB]+b[1])/2,newYmax*104/106,"L bio D bio",cex=0.8)
#        }
#        if (nLBDW>0) {
#          arrows(b[nLBDB]+(b[2]-b[1])/2,newYmax*102/106,b[nLBDB+nLBDW]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
#          text((b[nLBDB]+b[nLBDB+nLBDW])/2+(b[2]-b[1])/2,newYmax*104/106,"L bio D wt",cex=0.8)
#        }
#        if (nLBDN>0) {
#          if(nLBDB+nLBDW==0) { 
#            arrows(b[1]+(b[2]-b[1])/2,newYmax*102/106,b[nLBDB+nLBDW+nLBDN]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
#            text((b[1]+b[nLBDB+nLBDW+nLBDN])/2+(b[2]-b[1])/2,newYmax*104/106,"L bio D not",cex=0.8)
#          } else {
#            arrows(b[nLBDB+nLBDW]+(b[2]-b[1])/2,newYmax*102/106,b[nLBDB+nLBDW+nLBDN]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
#            text((b[nLBDB+nLBDW]+b[nLBDB+nLBDW+nLBDN])/2+(b[2]-b[1])/2,newYmax*104/106,"L bio D not",cex=0.8)
#          }          
#        }
#        if (nLNDB>0) {
#          arrows(b[nLBDB+nLBDW+nLBDN]+(b[2]-b[1])/2,newYmax*102/106,b[nLBDB+nLBDW+nLBDN+nLNDB]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
#          text((b[nLBDB+nLBDW+nLBDN]+b[nLBDB+nLBDW+nLBDN+nLNDB])/2+(b[2]-b[1])/2,newYmax*104/106,"L wt D bio",cex=0.8)
#        }
#        if (nLNDW>0) {
#          arrows(b[nLBDB+nLBDW+nLBDN+nLNDB]+(b[2]-b[1])/2,newYmax*102/106,b[nLBDB+nLBDW+nLBDN+nLNDB+nLNDW]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
#          text((b[nLBDB+nLBDW+nLBDN+nLNDB]+b[nLBDB+nLBDW+nLBDN+nLNDB+nLNDW])/2+(b[2]-b[1])/2,newYmax*104/106,"L wt D wt",cex=0.8)
#        }
#        if (nLNDN>0) {
#          arrows(b[nLBDB+nLBDW+nLBDN+nLNDB+nLNDW]+(b[2]-b[1])/2,newYmax*102/106,b[nLBDB+nLBDW+nLBDN+nLNDB+nLNDW+nLNDN]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
#          text((b[nLBDB+nLBDW+nLBDN+nLNDB+nLNDW]+b[length(b)])/2+(b[2]-b[1])/2,newYmax*104/106,"L wt D not",cex=0.8)        
#        }
#      }
#    }
#      if (plotType %in% c("DisWt","CatchWt","DisRatio","CatchRatio")) {
#        if (length(b)>1) {
#          Dindx <- stratumSummary$DisBioImp[indx]=="TRUE"
#          if (sum(indx)>0) {
#            points(b[Dindx],rep(0.01*newYmax,sum(Dindx)),pch=1)
#          }
#        }
#      }
#    } 
    if (countryLegend) 
      {
        if (plotType=="DisRate") ## steveH
        {
          legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15,ncol=2)
        } else
        {
          legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
        }
        
      }
    if (plotType=="LandPercent" & length(b)>1) lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
    if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
    if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
    if (plotType=="LandPercent") abline(h=100)
    }
}   

}

################################################################################

plotTotalNumbersAtAgeLength.byFleet <-function(OutNdata) {
  OutNdata$Fishery <-"OTH"
  OutNdata$Fishery[grep("CRU",OutNdata$Fleet)] <-"CRU"
  OutNdata$Fishery[grep("DEF",OutNdata$Fleet)] <-"DEF"
  N1 <- tapply(OutNdata$CANUM,list(OutNdata$AgeOrLength,OutNdata$CatchCat,OutNdata$Fishery),sum)/1e6
  
  # Fix for Logbook recorded discard numbers at age  
  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),byrow=F,dimnames=list(rownames(N1),
                                    c(paste(rep(sort(unique(OutNdata$Fishery)),each=length(unique(OutNdata$CatchCat))),
                                          rep(sort(unique(OutNdata$CatchCat)),length(unique(OutNdata$Fishery))),sep="_")))))
  
  #  N2 <- as.data.frame(matrix(N1,nrow=nrow(N1),ncol=8,byrow=F,dimnames=list(rownames(N1),c("DisImpFill","LanImpFill","DisRsdFill","LanRsdFill","DisImpSam","LanImpSam","DisRsdSam","LanRsdSam"))))
  N2[is.na(N2)] <- 0
  N2 <- N2[,c("CRU_Di","CRU_La","DEF_Di","DEF_La","OTH_Di","OTH_La")]    
  collist <- c("grey","blue","black","yellow","red","green")
  ymax <- 1.05*max(rowSums(N2))
  barplot(t(as.matrix(N2,nrow=13,ncol=6)),ylim=c(0,ymax),col=collist,xlab="age",ylab="numbers-at-age (millions)")
  box()
  legendtxt <- names(N2)                                            
  legend("topright",legend=legendtxt,col=collist,pch=19)  
  title("Total Catch Numbers At Age")  
}

read.canum.rsd <- function(files,age.rng=c(0,7)){
  #  canum <- NULL
  
  canum <-data.frame(matrix(ncol=(age.rng[2]-age.rng[1]+1),nrow=0))
  c.names <-c(as.character(age.rng[1]:age.rng[2]))
  colnames(canum) <-c.names
  #  canum <-data.frame()
  
  catwt <-canum
  
  yld <-NULL
  
  for(f in files){
    cat(f,'\n')
    
    ages <-scan(unz(f,'canum.txt'),skip=4,nlines=1)
    num0 <- as.numeric(gsub(",","",read.table(unz(f,'canum.txt'),skip=7,colClasses="character")))
    names(num0) <-as.character(ages[1]:ages[2])
    
    ages <-scan(unz(f,'weca.txt'),skip=4,nlines=1)
    wgt0 <- as.numeric(gsub(",","",read.table(unz(f,'weca.txt'),skip=7,colClasses="character")))
    names(wgt0) <-as.character(ages[1]:ages[2])
    
    yld0 <- read.table(unz(f,'caton.txt'),skip=7,colClasses="character")
    
    
    canum <- dplyr::bind_rows(canum,num0)
    catwt <- dplyr::bind_rows(catwt,wgt0)
    yld <-rbind(yld,as.numeric(gsub(",","",yld0)))
    
  }
  canum[is.na(canum)] <-0
  yld[is.na(yld)] <-0
  
  return(outdat=list(canum=canum,catwt=catwt,yld=yld))
}

