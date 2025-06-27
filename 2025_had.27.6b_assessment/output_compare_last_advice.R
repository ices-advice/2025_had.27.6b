# Extra stuff for ADGs to help explain the change in advice from last year
# 
library(reshape2)

out.dir <-"output/figures/"

wg <-"WGCSE"

ay <-2025 # assessment year

start.yr <-1991
data_yrs <- start.yr:(ay-1)

## Forecast parameters:
Ay_this_yr <-(ay-5):(ay-1)
Ay_last_yr <- (ay-11):(ay-2) # for mean wts - 10 year average
Sy <- (ay-3):(ay-1)  # for sel

ac<-as.character

load('model/model.RData')
load("model/forecast.RData")
SAM_fit_this_yr <-fit
Fmsy_forecast_this_yr <-FC[[1]]

load("boot/data/model_lastyr.RData")
load("boot/data/forecast_lastyr.RData")


SAM_fit_last_yr<-fit
Fmsy_forecast_last_yr<-FC[[1]]

################################################################################
# Compare the numbers as estimated and forecast of this years WG and last year's
################################################################################

n_this_year<- rbind(ntable(SAM_fit_this_yr),apply(exp(Fmsy_forecast_this_yr[[2]]$sim[,1:8]),2,median),
                    apply(exp(Fmsy_forecast_this_yr[[3]]$sim[,1:8]),2,median))

rownames(n_this_year) <- as.character(start.yr:(ay+1))

n_last_year <- rbind(ntable(SAM_fit_last_yr),
                     apply(exp(Fmsy_forecast_last_yr[[2]]$sim[,1:8]),2,median),
                     apply(exp(Fmsy_forecast_last_yr[[3]]$sim[,1:8]),2,median),
                     apply(exp(Fmsy_forecast_last_yr[[4]]$sim[,1:8]),2,median))
rownames(n_last_year) <- rownames(n_this_year)

df_n_this_year <- melt(t(n_this_year))
colnames(df_n_this_year) <- c("Age","Year","N")
df_n_this_year$"WG"<-paste(wg,ac(ay))
df_n_this_year$"Type"<-"Data"
df_n_this_year[df_n_this_year$Year==ay,]$"Type"<-"Intermediate year"
df_n_this_year[df_n_this_year$Year==(ay+1),]$"Type"<-"Advice year"

head(df_n_this_year)
table(df_n_this_year$Year,df_n_this_year$Type)

df_n_last_year <- melt(t(n_last_year[ac(start.yr:ay),]))
colnames(df_n_last_year) <- c("Age","Year","N")
df_n_last_year$"WG"<-paste(wg,ac(ay-1))
df_n_last_year$"Type"<-"Data"
df_n_last_year[df_n_last_year$Year==(ay-1),]$"Type"<-"Intermediate year"
df_n_last_year[df_n_last_year$Year==ay,]$"Type"<-"Advice year"

head(df_n_last_year)
table(df_n_last_year$Year,df_n_last_year$Type)

N_at_age_dat<-rbind(df_n_last_year,df_n_this_year)
dat<-N_at_age_dat[N_at_age_dat$Year>(ay-3),]
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))

taf.png(paste0(out.dir,"Compare stock numbers-at-age.png"),width = 10, height = 6, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=as.factor(Age),y=N,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=5)+labs(colour="",y="Abundance (thousands)",shape="") + xlab("Age") +
  facet_wrap(~Year,nrow=2)+theme_bw()+theme(text=element_text(size=20))+scale_shape_manual(values=c(16, 2, 0))
print(p1)
dev.off()

ncomp <- n_this_year/n_last_year
ncomp <- t(ncomp[ac((ay-15):(ay+1)),])
write.taf(ncomp, dir = "output/tables")

################################################################################
# Compare the biomass as estimated and forecast of this years WG and last year's
################################################################################

swa_this_yr <- exp(SAM_fit_this_yr$pl$logSW)
rownames(swa_this_yr) <-ac(start.yr:(ay+9))
swa_last_yr <- exp(SAM_fit_last_yr$pl$logSW)
rownames(swa_last_yr) <-ac(start.yr:(ay+8))


b_by_age_sims_1 <-sweep(exp(Fmsy_forecast_this_yr[[2]]$sim[,1:8]), MARGIN = 2, FUN = "*", STATS = swa_this_yr[length(SAM_fit_this_yr$data$years)+1,])
b_by_age_1 <-apply(b_by_age_sims_1,2,median)

sum(b_by_age_1)

b_by_age_sims_2 <-sweep(exp(Fmsy_forecast_this_yr[[3]]$sim[,1:8]), MARGIN = 2, FUN = "*", STATS = swa_this_yr[length(SAM_fit_this_yr$data$years)+2,])
b_by_age_2 <-apply(b_by_age_sims_2,2,median)

sum(b_by_age_2)

b_this_year <- rbind(ntable(SAM_fit_this_yr) * swa_this_yr[1:length(SAM_fit_this_yr$data$years),], b_by_age_1, b_by_age_2)

rownames(b_this_year) <- as.character(start.yr:(ay+1))


b_by_age_sims_1 <-sweep(exp(Fmsy_forecast_last_yr[[2]]$sim[,1:8]), MARGIN = 2, FUN = "*", STATS = swa_last_yr[length(SAM_fit_last_yr$data$years)+1,])
b_by_age_1 <-apply(b_by_age_sims_1,2,median)
sum(b_by_age_1)

b_by_age_sims_2 <-sweep(exp(Fmsy_forecast_last_yr[[3]]$sim[,1:8]), MARGIN = 2, FUN = "*", STATS = swa_last_yr[length(SAM_fit_last_yr$data$years)+2,])
b_by_age_2 <-apply(b_by_age_sims_2,2,median)

sum(b_by_age_2)

b_by_age_sims_3 <-sweep(exp(Fmsy_forecast_last_yr[[4]]$sim[,1:8]), MARGIN = 2, FUN = "*", STATS = swa_last_yr[length(SAM_fit_last_yr$data$years)+3,])
b_by_age_3<-b_by_age_sims_3[which.min(abs(rowSums(b_by_age_sims_3) - median(rowSums(b_by_age_sims_3)))),]
b_by_age_3 <-apply(b_by_age_sims_3,2,median)

sum(b_by_age_3)

b_last_year <- rbind(ntable(SAM_fit_last_yr) * swa_last_yr[1:length(SAM_fit_last_yr$data$years),],b_by_age_1,b_by_age_2,b_by_age_3)
rownames(b_last_year) <- rownames(b_this_year)

df_b_this_year <- melt(t(b_this_year[ac(start.yr:(ay+1)),]))
colnames(df_b_this_year) <- c("Age","Year","B")
df_b_this_year$"WG"<-paste(wg,ac(ay))
df_b_this_year$"Type"<-"Data"
df_b_this_year[df_b_this_year$Year==ay,]$"Type"<-"Intermediate year"
df_b_this_year[df_b_this_year$Year==(ay+1),]$"Type"<-"Advice year"

head(df_b_this_year)
table(df_b_this_year$Year,df_b_this_year$Type)

df_b_last_year <- melt(t(b_last_year[ac(start.yr:ay),]))
colnames(df_b_last_year) <- c("Age","Year","B")
df_b_last_year$"WG"<-paste(wg,ac(ay-1))
df_b_last_year$"Type"<-"Data"
df_b_last_year[df_b_last_year$Year==(ay-1),]$"Type"<-"Intermediate year"
df_b_last_year[df_b_last_year$Year==ay,]$"Type"<-"Advice year"

head(df_b_last_year)
table(df_b_last_year$Year,df_b_last_year$Type)

B_at_age_dat<-rbind(df_b_last_year,df_b_this_year)
dat<-B_at_age_dat[B_at_age_dat$Year>(ay-3),]
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))

taf.png(paste0(out.dir,"Compare stock biomass-at-age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=as.factor(Age),y=B,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+ xlab("Age") +
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))
print(p1)
dev.off()

bcomp <- b_this_year/b_last_year
bcomp <- t(bcomp[ac((ay-15):(ay+1)),])
write.taf(bcomp, dir = "output/tables")

################################################################################
# Compare the stock weights as estimatined in this years WG and last year's
################################################################################

swa_this_yr <- exp(SAM_fit_this_yr$pl$logSW)
rownames(swa_this_yr) <-ac(start.yr:(ay+9))
swa_last_yr <- exp(SAM_fit_last_yr$pl$logSW)
rownames(swa_last_yr) <-ac(start.yr:(ay+8))

sw_this_year <-swa_this_yr[(length(SAM_fit_this_yr$data$years)-1):(length(SAM_fit_this_yr$data$years)+2),]


sw_last_year <-swa_last_yr[(length(SAM_fit_last_yr$data$years)):(length(SAM_fit_last_yr$data$years)+3),]

df_sw_this_year <- melt(t(sw_this_year))
colnames(df_sw_this_year) <- c("Age","Year","SW")
df_sw_this_year$"WG"<-paste(wg,ac(ay))
df_sw_this_year$"Type"<-"Data"
df_sw_this_year[df_sw_this_year$Year==ay,]$"Type"<-"Intermediate year"
df_sw_this_year[df_sw_this_year$Year==(ay+1),]$"Type"<-"Advice year"


df_sw_last_year <- melt(t(sw_last_year))
colnames(df_sw_last_year) <- c("Age","Year","SW")
df_sw_last_year$"WG"<-paste(wg,ac(ay-1))
df_sw_last_year$"Type"<-"Data"
df_sw_last_year[df_sw_last_year$Year==(ay-1),]$"Type"<-"Intermediate year"
df_sw_last_year[df_sw_last_year$Year==ay,]$"Type"<-"Advice year"
df_sw_last_year[df_sw_last_year$Year==(ay+1),]$"Type"<-"Advice year+1"


SW_at_age_dat<-rbind(df_sw_last_year,df_sw_this_year)
dat<-SW_at_age_dat[SW_at_age_dat$Year>(ay-3),]
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year","Advice year+1"))

taf.png(paste0(out.dir,"Compare stock weights-at-age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(subset(dat,Type!="Advice year+1"),aes(x=as.factor(Age),y=SW,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Weight (kg)",shape="")+ xlab("Age") +
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))
print(p1)
dev.off()



######################################################
#  Compare catch numbers at age
###################################################

#catch_sim <- sweep(FC[[i]][[3]]$catchatage, MARGIN = 1, FUN = "*", STATS = colMeans(tail(fit$data$catchMeanWeight,av.yrs)))
cn_this_year <- rbind(caytable(SAM_fit_this_yr),
                      apply(Fmsy_forecast_this_yr[[2]]$catchatage,1,median),
                      apply(Fmsy_forecast_this_yr[[3]]$catchatage,1,median))
#,
#                      apply(Fmsy_forecast_this_yr[[4]]$catchatage,1,median))

rownames(cn_this_year) <- ac(start.yr:(ay+1))

cn_last_year <- rbind(caytable(SAM_fit_last_yr),
                      apply(Fmsy_forecast_last_yr[[2]]$catchatage,1,median),
                      apply(Fmsy_forecast_last_yr[[3]]$catchatage,1,median),
                      apply(Fmsy_forecast_last_yr[[4]]$catchatage,1,median))
rownames(cn_last_year) <- rownames(cn_this_year)

df_cn_this_year <- melt(t(cn_this_year[ac(start.yr:(ay+1)),]))
colnames(df_cn_this_year) <- c("Age","Year","C")
df_cn_this_year$"WG"<-paste(wg,ay)
df_cn_this_year$"Type"<-"Data"
df_cn_this_year[df_cn_this_year$Year==ay,]$"Type"<-"Intermediate year"
df_cn_this_year[df_cn_this_year$Year==(ay+1),]$"Type"<-"Advice year"

head(df_cn_this_year)
table(df_cn_this_year$Year,df_cn_this_year$Type)

df_cn_last_year <- melt(t(cn_last_year[ac(start.yr:ay),]))
colnames(df_cn_last_year) <- c("Age","Year","C")
df_cn_last_year$"WG"<-paste(wg,(ay-1))
df_cn_last_year$"Type"<-"Data"
df_cn_last_year[df_cn_last_year$Year==(ay-1),]$"Type"<-"Intermediate year"
df_cn_last_year[df_cn_last_year$Year==ay,]$"Type"<-"Advice year"

head(df_cn_last_year)
table(df_cn_last_year$Year,df_cn_last_year$Type)

C_at_age_dat<-rbind(df_cn_last_year,df_cn_this_year)
dat<-C_at_age_dat[C_at_age_dat$Year>(ay-3),]
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))


taf.png(paste0(out.dir,"Compare catch numbers-at-age.png"),width = 10, height = 6, units = "in", res = 600)
p1 <- ggplot(dat,aes(x=as.factor(Age),y=C,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=5)+labs(colour="",y="Catch",shape="") + xlab("Age") +
  facet_wrap(~Year,nrow=2)+theme_bw()+theme(text=element_text(size=20))+scale_shape_manual(values=c(16, 2, 0))
print(p1)
dev.off()


# compare forecast assumptions/summary outputs----------------------------------------------####
#  -----------------------------------------------####

# this year
tab1 <- data.frame(WG =paste(wg,ay),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-1):(ay+1),4), Type=NA, Value= NA, Source = "Forecast")

tab1$Type[tab1$Year %in% (ay-1)] <- "Data year"
tab1$Type[tab1$Year %in% (ay)] <- "Intermediate year"
tab1$Type[tab1$Year %in% (ay+1)] <- "Advice year"

tab1$Source[tab1$Year %in% (ay-1)] <- "Assessment"

astab <- as.data.frame(summary(SAM_fit_this_yr))
astab$Year <- rownames(astab)
tmp_tab <- catchtable(SAM_fit_this_yr)
fctab <- attr(Fmsy_forecast_this_yr,"tab")

# data year values
idx <- which(astab$Year %in% c(ay-1)) # data yr
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Recruitment"] <- astab$"R(age 1)"[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "SSB"] <- astab$SSB[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Fbar"] <- astab$"Fbar(2-5)"[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Total catch"] <- catchtable(SAM_fit_this_yr)[as.character(ay-1),"Estimate"] # data year

# int year values
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay),"ssb:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay),"fbar:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay),"catch:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay),"rec:median"]

# advice year values
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay+1),"ssb:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay+1),"fbar:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay+1),"catch:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay+1),"rec:median"]


# last year
tab2 <- data.frame(WG =paste(wg,ay-1),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-2):(ay),4), Type=NA, Value= NA, Source = "Forecast")

tab2$Type[tab2$Year %in% (ay-2)] <- "Data year"
tab2$Type[tab2$Year %in% (ay-1)] <- "Intermediate year"
tab2$Type[tab2$Year %in% (ay)] <- "Advice year"

tab2$Source[tab2$Year %in% (ay-2)] <- "Assessment"

#astab <- getSAG(stock = "had.27.46a20",year=(ay-1),purpose="Advice")
astab <- as.data.frame(summary(SAM_fit_last_yr))
astab$Year <- rownames(astab)
fctab <- attr(Fmsy_forecast_last_yr,"tab")

# data year values
idx <- which(astab$Year %in% c(ay-2)) # data yr
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Recruitment"] <- astab$"R(age 1)"[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "SSB"] <- astab$SSB[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Fbar"] <- astab$"Fbar(2-5)"[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Total catch"] <- catchtable(SAM_fit_this_yr)[as.character(ay-1),"Estimate"] # data year

# int year values
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "SSB"] <- fctab[as.character(ay-1),"ssb:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Fbar"] <- fctab[as.character(ay-1),"fbar:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Total catch"] <- fctab[as.character(ay-1),"catch:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Recruitment"] <- fctab[as.character(ay-1),"rec:median"]

# advice year values
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "SSB"] <- fctab[as.character(ay),"ssb:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Fbar"] <- fctab[as.character(ay),"fbar:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Total catch"] <- fctab[as.character(ay),"catch:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Recruitment"] <- fctab[as.character(ay),"rec:median"]


# combine tables and export
tab <- rbind(tab1,tab2)

write.csv(tab,paste0(out.dir,"Compare_forecast_outputs.csv"),row.names=F)

#  Plot the comparisons

dat <- tab
dat$Type <- factor(dat$Type,levels=c("Data year","Intermediate year","Advice year"))

taf.png(paste0(out.dir,"Compare_forecast_outputs.png"),width = 10, height = 6, units = "in", res = 600)
p1 <- ggplot(subset(dat,Year>(ay-2)),aes(x=as.factor(Year),y=Value))+
#  geom_line(aes(colour=WG))+
  geom_point(aes(colour=WG,shape=Type),size=4,fill='white')+
  scale_shape_manual(values=c(3,21,16)) +
  facet_wrap(~Variable,scales="free_y")+
  theme_bw()+theme(text=element_text(size=20))+labs(x="",y="",colour="",shape="")
print(p1)
dev.off()



# Forecast_selectivit---------------------------------------------------####

# Fbar range
fromto <- SAM_fit_this_yr$conf$fbarRange - (SAM_fit_this_yr$conf$minAge - 1)

Ftab_this_yr <-faytable(SAM_fit_this_yr)
Ftab_last_yr <-faytable(SAM_fit_last_yr)

sel_this_yr <-colMeans(Ftab_this_yr[as.integer(rownames(Ftab_this_yr)) %in% ac(Sy),,drop=FALSE])
sel_last_yr <-colMeans(Ftab_last_yr[as.integer(rownames(Ftab_last_yr)) %in% ac(Sy-1),,drop=FALSE])
sel_this_yr <- sel_this_yr/mean(sel_this_yr[fromto[1]:fromto[2]])
sel_last_yr <- sel_last_yr/mean(sel_last_yr[fromto[1]:fromto[2]])


sel_this_yr <- as.data.frame(sel_this_yr)
sel_last_yr <- as.data.frame(sel_last_yr)

sel_this_yr$label <-paste(wg,ay)
names(sel_this_yr)[1] <-"value"
sel_this_yr$Age <-rownames(sel_this_yr)
sel_last_yr$label <-paste(wg,(ay-1))
names(sel_last_yr)[1] <-"value"
sel_last_yr$Age <-rownames(sel_last_yr)

com<-rbind(sel_this_yr,sel_last_yr)
com$Age <- as.factor(as.numeric(com$Age))
str(com)

taf.png(paste0(out.dir,"Compare forecast selectivity.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=com, aes(Age, value, group=label)) + geom_line(lwd=1.5,aes(colour = label)) + ylab("fishery selectivity pattern")+labs(colour="")+
  theme_bw()+theme(text=element_text(size=20))+scale_shape_manual()
print(p1)
dev.off()


# Compare forecast catch weights

cw_last_yr<-apply(SAM_fit_last_yr$data$catchMeanWeight[ac(Ay_last_yr),,1],2,mean)
cw_this_yr<-apply(SAM_fit_this_yr$data$catchMeanWeight[ac(Ay_this_yr),,1],2,mean)

cw_this_yr<-as.data.frame(cw_this_yr)
cw_last_yr<-as.data.frame(cw_last_yr)

cw_this_yr$label<-paste(wg,ay)
names(cw_this_yr)[1]<-"value"
cw_this_yr$Age<-rownames(cw_this_yr)
cw_last_yr$label<-paste(wg,(ay-1))
names(cw_last_yr)[1]<-"value"
cw_last_yr$Age<-rownames(cw_last_yr)

com<-rbind(cw_this_yr,cw_last_yr)
com$Age <- as.factor(as.numeric(com$Age))
str(com)

taf.png(paste0(out.dir,"Compare forecast mean cw.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=com, aes(Age, value, group=label)) + geom_line(lwd=1.5,aes(colour = label)) + ylab("mean catch weight (kg)")+labs(colour="")+
  theme_bw()+theme(text=element_text(size=20))+scale_shape_manual()
print(p1)
dev.off()





##########################################
#  Compare F at age

# plot fishing mortality
f_last_year <- rbind(faytable(SAM_fit_last_yr),
                     apply(exp(Fmsy_forecast_last_yr[[2]]$sim[,1:8]),2,median),
                     apply(exp(Fmsy_forecast_last_yr[[3]]$sim[,1:8]),2,median),
                     apply(exp(Fmsy_forecast_last_yr[[4]]$sim[,1:8]),2,median))
rownames(n_last_year) <- rownames(n_this_year)

n_this_year2    <- t(n_this_year[as.character(1991:2026),])
df_n_this_year <- melt(n_this_year2)
colnames(df_n_this_year) <- c("Age","Year","N")
df_n_this_year$"WG"<-"WGCSE 2025"
df_n_this_year$"Type"<-"Data"
df_n_this_year[df_n_this_year$Year==2025,]$"Type"<-"Intermediate year"
df_n_this_year[df_n_this_year$Year==2026,]$"Type"<-"Advice year"

head(df_n_this_year)
table(df_n_this_year$Year,df_n_this_year$Type)


n_last_year2    <- t(n_last_year[as.character(1991:2025),])
df_n_last_year <- melt(n_last_year2)
colnames(df_n_last_year) <- c("Age","Year","N")
df_n_last_year$"WG"<-"WGCSE 2024"
df_n_last_year$"Type"<-"Data"
df_n_last_year[df_n_last_year$Year==2024,]$"Type"<-"Intermediate year"
df_n_last_year[df_n_last_year$Year==2025,]$"Type"<-"Advice year"

head(df_n_last_year)
table(df_n_last_year$Year,df_n_last_year$Type)

N_at_age_dat<-rbind(df_n_last_year,df_n_this_year)
dat<-N_at_age_dat[N_at_age_dat$Year>2022,]
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))


# plot selectivity

sel_this_yr<-apply(faytable(SAM_fit_this_yr)[as.character(seq(2022,2024)),],2,mean)
sel_last_yr<-apply(faytable(SAM_fit_last_yr)[as.character(seq(2021,2023)),],2,mean)

sel_this_yr<-as.data.frame(sel_this_yr)
sel_last_yr<-as.data.frame(sel_last_yr)

sel_this_yr$label<-"mean(22-24)"
names(sel_this_yr)[1]<-"value"
sel_this_yr$Age<-rownames(sel_this_yr)
sel_last_yr$label<-"mean(21-23)"
names(sel_last_yr)[1]<-"value"
sel_last_yr$Age<-rownames(sel_last_yr)

com<-rbind(sel_this_yr,sel_last_yr)
com$Age <- as.factor(as.numeric(com$Age))
str(com)

taf.png(paste0(out.dir,"Compare mean F.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=com, aes(Age, value, group=label)) + geom_line(aes(colour = label)) + ylab("mean F")
print(p1)
dev.off()





