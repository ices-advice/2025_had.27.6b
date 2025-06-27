##########################################
#' Process the forecast outputs
#' 
##########################################
#
#  

library(TMB)
library(icesAdvice)
library(ggplot2)
library(viridis)

# Load the final assessment & forecast
load("model/model.RData")
load("model/forecast.RData")

# ~~~~~~~~~~~~Annual inputs to be checked~~~~~~~~~~~~~~~~~~~~~~~~~
prev.TAC <-10195 # TAC for 2023 & 2024
prev.advice <-31565

final.yr <-2024 # final assessment year
int.yr <-2 # If the assessment includes the int year (and this is the 1st year in forecast then set int.yr=1)

#av.yrs <-10 # No of years for averageing mean weights
av.yrs <-5 # No of years for averageing mean weights - changed at WGCSE 2025

ages <-fit$conf$minAge:fit$conf$maxAge


#  ~~~~~~~~~~~~~Reference points~~~~~~~~~~~~~~~~~~~~~
Blim <-8542
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ac<-as.character


Table2 <- NULL
Table3 <- NULL

for (i in 1:length(FC))
{
#  Forecast inputs  
  Table2 <- rbind(Table2, data.frame(Basis=attributes(FC[[i]])$label,
                                     Ftot=attributes(FC[[i]])$shorttab[1,int.yr],
                                     SSB =attributes(FC[[i]])$shorttab[3,int.yr+1],
                                     R1 =  attributes(FC[[i]])$shorttab[2,int.yr],
                                     R2 =  attributes(FC[[i]])$shorttab[2,int.yr+1],
                                     TotCatch=attributes(FC[[i]])$shorttab[7,int.yr]+attributes(FC[[i]])$shorttab[8,int.yr],
                                     Landings=attributes(FC[[i]])$shorttab[7,int.yr],
                                     Discards=attributes(FC[[i]])$shorttab[8,int.yr]
                                     
  ) )
  
# Changed table 3 to get the Fbar with more decimals & then apply ICES rounding
    Table3 <- rbind(Table3, data.frame(Basis=attributes(FC[[i]])$label,
                                     TotCatch=attributes(FC[[i]])$shorttab[7,int.yr+1]+attributes(FC[[i]])$shorttab[8,int.yr+1],
                                     WCatch=attributes(FC[[i]])$shorttab[7,int.yr+1],
                                     UCatch =attributes(FC[[i]])$shorttab[8,int.yr+1],
#                                     Ftot=attributes(FC[[i]])$shorttab[1,int.yr+1],
#                                     Fw=attributes(FC[[i]])$shorttab[5,int.yr+1],
#                                     Fu=attributes(FC[[i]])$shorttab[6,int.yr+1],
                                     Ftot = median(FC[[i]][[int.yr+1]]$fbar),
                                     Fw = median(FC[[i]][[int.yr+1]]$fbarL),
                                     Fu = median(FC[[i]][[int.yr+1]]$fbar-FC[[i]][[int.yr+1]]$fbarL),
                                     SSB = attributes(FC[[i]])$shorttab[3,int.yr+2],
                                     SSBpc =NA,
                                     TACpc=NA,
                                     Advicepc = NA,
                                     Prob.less.Blim=NA
  ) )
}

Table2 
write.csv(Table2,file=paste0("output/tables/forecast_Table2.csv"))
#ccheck rows are all the same

Table3

Table3$SSBpc <- 100 * (Table3$SSB/Table2$SSB -1)
Table3$TACpc <- 100 * (Table3$TotCatch/prev.TAC - 1)
Table3$Advicepc <- 100 * (Table3$TotCatch/prev.advice - 1)

#  Stock weights are modelled outputs
swa <- exp(fit$pl$logSW)[length(fit$data$years)+int.yr+1,]

perc <-rep(NA,length(FC))
for (i in 1:length(FC)){
  perc[i] <-100*sum(rowSums(sweep(exp(FC[[i]][[4]]$sim[,ages]), MARGIN = 2, FUN = "*", STATS = (swa * tail(fit$data$propMat,1))))>Blim)/dim(FC[[i]][[4]]$sim)[1]
}
Table3$Prob.less.Blim <-100-perc

# Rounding
Tab3 <-Table3
Tab3$Ftot <-icesRound(Table3$Ftot)
Tab3$Fw <-icesRound(Table3$Fw)
Tab3$Fu <-icesRound(Table3$Fu)
Tab3$TotCatch <-round(Table3$TotCatch)
Tab3$WCatch <-round(Table3$WCatch)
Tab3$UCatch <-round(Table3$UCatch)
Tab3$SSB <-round(Table3$SSB)
Tab3$SSBpc <-icesRound(Table3$SSBpc,percent=TRUE)
Tab3$TACpc <-icesRound(Table3$TACpc,percent=TRUE)
Tab3$Advicepc <-icesRound(Table3$Advicepc,percent=TRUE)
Tab3$Prob.less.Blim <-icesRound(Table3$Prob.less.Blim)

write.csv(Tab3,file="output/tables/forecast_Table3_rounded.csv")
write.csv(Table3,file="output/tables/forecast_Table3_unrounded.csv")

i <-1
jpeg(paste0("output/figures/forecast_",i,".jpeg"), height = 210, width = 210, units = "mm", res = 350)
plot(FC[[i]])
title(attr(FC[[i]],"label"), outer=TRUE, line=-1)
dev.off()

i <-length(FC)
jpeg(paste0("output/figures/forecast_",i,".jpeg"), height = 210, width = 210, units = "mm", res = 350)
plot(FC[[i]])
title(attr(FC[[i]],"label"), outer=TRUE, line=-1)
dev.off()


i <-1
ssb_sim <-sweep(exp(FC[[i]][[4]]$sim[,ages]), MARGIN = 2, FUN = "*", STATS = (swa * tail(fit$data$propMat,1)))
# Another option that gives the simulation closest to median of totals
#ssb <- ssb_sim[which.min(abs(rowSums(ssb_sim) - median(rowSums(ssb_sim)))),]
# Provides the median of each of the age classes
ssb <-apply(ssb_sim,MARGIN=2,FUN=median)

#C by age Intplus1
catch_sim <- sweep(FC[[i]][[3]]$catchatage, MARGIN = 1, FUN = "*", STATS = colMeans(tail(fit$data$catchMeanWeight,av.yrs)))
# As for SSB
#catch <-  catch_sim[,which.min(abs(colSums(catch_sim) - median(colSums(catch_sim))))] 
catch <-apply(catch_sim,MARGIN=1,FUN=median)
sum(catch)

int.yr <- final.yr+1

plot.data <- rbind(
  data.frame(recruitment=int.yr + 1:-6, val=catch, type=paste(int.yr+1,"Catch")),
  data.frame(recruitment=int.yr + 2:-5, val=ssb, type=paste(int.yr+2,"SSB"))
)
# Modify this because of assumptioi in int year
#plot.data$est <- plot.data$recruitment >= int.yr+1
plot.data$est <-plot.data$recruitment >=int.yr

ggplot(plot.data, aes(x=factor(recruitment), y=val, fill=est)) + facet_wrap(~type, scales="free_y") + geom_col() + 
  xlab("Recruitment year") +ylab("Contribution (tonnes)") +
  scale_fill_viridis_d()+theme(legend.position="none")

ggsave(paste0("output/figures/Forecast contribution plot.png"), 
       width = 20, height = 15, units = "cm", dpi = 300, type = "cairo-png")

write.csv(plot.data,file="output/tables/forecast_contribution.csv")


plot.data <-plot.data %>% group_by(type) %>% mutate(prop=val/sum(val))


ggplot(plot.data, aes(x=factor(recruitment), y=prop, fill=est)) + facet_wrap(~type, scales="free_y") + geom_col() + 
  xlab("Recruitment year") +ylab("Proportion") +
  scale_fill_viridis_d()+theme(legend.position="none")

ggsave(paste0("output/figures/Forecast contribution prop plot.png"), 
       width = 20, height = 15, units = "cm", dpi = 300, type = "cairo-png")


#ompare stock and catch mean weights in forecast
swa <- as.data.frame(exp(fit$pl$logSW)[(length(fit$data$years)-1):(length(fit$data$years)+2),])
colnames(swa) <-ac(ages)
swa$Year <-ac((max(fit$data$years)-1):(max(fit$data$years)+2))

df_swa <-pivot_longer(swa,cols=ac(ages),names_to="Age",values_to="Wt")
df_swa$Type <-"Stock"

cwa <-as.data.frame(fit$data$catchMeanWeight[(length(fit$data$years)-1):(length(fit$data$years)),,])
cwa <-rbind(cwa,t(colMeans(tail(fit$data$catchMeanWeight,av.yrs))))
cwa <-rbind(cwa,t(colMeans(tail(fit$data$catchMeanWeight,av.yrs))))
colnames(cwa) <-ac(ages)
cwa$Year <-ac((max(fit$data$years)-1):(max(fit$data$years)+2))

df_cwa <-pivot_longer(cwa,cols=ac(ages),names_to="Age",values_to="Wt")
df_cwa$Type <-"Catch"

dat <-rbind(df_swa,df_cwa)


taf.png(paste0(out.dir,"Compare forecast weights-at-age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=as.factor(Age),y=Wt,group=Type,colour=Type,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Weight (kg)",shape="")+ xlab("Age") +
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))
print(p1)
dev.off()






