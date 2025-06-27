
plot.by.age <-function(in.dat,yname="",xname=""){
  p <-ggplot(in.dat,
             aes(as.numeric(year),y=data,group=as.character(age),colour=as.character(age),label=as.character(age)))+
    geom_line()+geom_text()+
    #  facet_wrap(~age)+
    ylab(yname)+
    xlab(xname)+
    scale_x_continuous(breaks = seq(1980,2020,5))+
    theme_linedraw()+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.position="none",
          axis.title = element_text(size=16),
          axis.text = element_text(size=14),
          strip.text=element_text(size=14,angle = 0))+facet_grid(type~.)
  p
}

plot.landings.and.discards.by.age <-function(in.dat,yname="",xname="",scaling="fixed"){
  p <-ggplot(in.dat,
             aes(as.numeric(age),y=data,fill=type))+
    geom_bar(stat="identity")+
      facet_wrap(~year,scale=scaling)+
    ylab(yname)+
    xlab(xname)+
#    scale_x_continuous(breaks = seq(1980,2015,5))+
    theme_linedraw()+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.position="none",
          axis.title = element_text(size=12),
          axis.text = element_text(size=12),
 #         strip.text=element_blank(),
          strip.text=element_text(size=10,angle = 0))
  p
}

plot.idx.by.age <-function(in.dat,yname="",xname=""){
  p <-ggplot(in.dat,
             aes(as.numeric(year),y=data,group=as.character(age),colour=as.character(age),label=as.character(age)))+
    geom_line()+geom_text()+
    ylab(yname)+
    xlab(xname)+
#    scale_x_continuous(breaks = seq(1980,2015,5))+
    theme_linedraw()+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.position="none",
          axis.title = element_text(size=16),
          axis.text = element_text(size=14),
          strip.text=element_text(size=14,angle = 0))
  p
}

plot.catch.curve <-function(in.dat,yname="",xname=""){
  p <-ggplot(in.dat,
             aes(as.numeric(year),y=data,group=as.character(cohort),colour=as.character(cohort),label=as.character(age)))+
    geom_line()+geom_text()+
    ylab(yname)+
    xlab(xname)+
    #    scale_x_continuous(breaks = seq(1980,2015,5))+
    theme_linedraw()+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.position="none",
          axis.title = element_text(size=16),
          axis.text = element_text(size=14),
          strip.text=element_text(size=14,angle = 0))
  p
}


m.at.age.and.weight <-function(in.dat,m.par){
  
  per.yr.M <- m.par$mu* (1000 * in.dat@stock.wt)**m.par$b 
  
  mort <-rbind(cbind(as.data.frame(per.yr.M),type="annual"),
                     cbind(as.data.frame(in.dat@m),type="mean weight"))
 
  ggplot(subset(mort,type=="annual"),
    aes(as.numeric(year),y=data,group=as.character(age),colour=as.character(age),label=as.character(age)))+
    geom_line()+geom_text()+
    ylab("")+
    expand_limits(y=0.1)+
    xlab("year")+
    scale_x_continuous(breaks = seq(1980,2020,5))+
    theme_linedraw()+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.position="none",
          axis.title = element_text(size=16),
          axis.text = element_text(size=14))+
    geom_hline(aes(yintercept=subset(mort,type=="mean weight")$data),colour="grey",size=1.0)+
    geom_hline(aes(yintercept=0.2),linetype="dashed",size=1.0)

  
}

plot.catch <-function(in.dat,yname="",xname=""){
  ggplot(in.dat,
             aes(as.numeric(year),y=data,fill=as.character(type),colour=as.character(type)))+
    geom_bar(stat="identity")+
    ylab(yname)+
    xlab(xname)+
 #   scale_x_continuous(breaks = seq(1980,2020,5))+
 #   scale_y_continuous(expand=c(0,0),limits=c(0,1.1*max(in.dat$data)))+
    scale_y_continuous(expand=c(0,0))+
    theme_linedraw()+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          legend.title=element_blank(),
          axis.title = element_text(size=16),
          axis.text = element_text(size=14))
}

catch.at.age.bubble.plot <-function(in.dat,filename){
  

  ggplot(in.dat, aes(year,age,size=data)) +
    geom_point(shape=21,colour=c("black"),fill=c("grey35"))+ylab("Age")+
    theme(axis.text = element_text(size=16))+
    theme(title=element_text(size=18))+  
    theme(legend.position="none")+
    scale_size(range=c(1,20))
  ggsave(filename=paste(filename,"wmf",sep="."),path=paste(getwd(),"/",stock.id,"/",sep=""))
  graphics.off()
  
  windows(width=7,height=5)
  sc.fill.vals <-c("white","grey35")
  if(length(unique(sign(sc.data$standard.num)))>2){
    sc.fill.vals <-c("white","black","grey35")
  }
  ggplot(sc.data, aes(Year,Age,size=abs(standard.num),fill=as.factor(sign(standard.num)))) +
    geom_point(shape=21,colour=c("black"))+ylab("Age")+
    theme(axis.text = element_text(size=16))+
    theme(title=element_text(size=18))+
    
    theme(legend.position="none")+
    scale_size(range=c(1,15))+
    #        scale_fill_brewer()
    scale_fill_manual(values=sc.fill.vals)
  
  ggsave(filename=paste(filename,"standardised","wmf",sep="."),path=paste(getwd(),"/",stock.id,"/",sep=""))
  graphics.off()
}

catch.at.age.bubble.plot <-function(in.dat,st=FALSE){
  
  if(!st){
    ggplot(in.dat, aes(year,age,size=data)) +
      geom_point(shape=21,colour=c("black"),fill=c("grey35"))+ylab("Age")+
      theme(axis.text = element_text(size=16))+
      theme(title=element_text(size=18))+  
      theme(legend.position="none")+
      scale_size(range=c(1,20))
    #  ggsave(filename=paste(filename,"wmf",sep="."),path=paste(getwd(),"/",stock.id,"/",sep=""))
    #  graphics.off()
  }else{
    sc.fill.vals <-c("white","grey35")
    if(length(unique(sign(in.dat$data)))>2){
      sc.fill.vals <-c("white","black","grey35")
    }
    ggplot(in.dat, aes(year,age,size=abs(data),fill=as.factor(sign(data)))) +
      geom_point(shape=21,colour=c("black"))+ylab("Age")+
      theme(axis.text = element_text(size=16))+
      theme(title=element_text(size=18))+
      
      theme(legend.position="none")+
      scale_size(range=c(1,15))+
      #        scale_fill_brewer()
      scale_fill_manual(values=sc.fill.vals)
  } 
  
}

survey.splom <-function(dat,plusgp=FALSE,s.name=""){
  with.plus.gp <-plusgp
  
  ages <-colnames(dat)
  n.ages <-length(ages)
  
  r <- 0.1
  col.reg <- sp::bpy.colors(50)
  for (i in 1:50) col.reg[i] <- colorRampPalette(c(col.reg[i], "white"))(5)[2]
  
  # png(filename=paste0(path, "figure.png"), height=6, width=6, bg="white", pointsize=10, res=300, units="in")
  par(mfrow=c(n.ages, n.ages), oma=c(0.8, 0.8, 2.2, 0.8), mar=c(0, 0, 0, 0), las=1, xaxt="n", yaxt="n", cex=0.8)
  
  for (i in 1:n.ages){
    for (j in 1:n.ages) {
      if (j <= n.ages - i) {
        id <-which(!is.na(dat[,j]) & !is.na(dat[,n.ages-i+1]))
        x <- log(dat[id,j])
        y <- log(dat[id,n.ages-i+1])
        plot.new()
        polygon(100*c(-1, 1, 1, -1, -1), 100*c(-1, -1, 1, 1, -1), col=col.reg[(cor(x, y)+1)*50/2], border=NA)
        box()
        legend("center", legend=round(cor(x, y), 3), bty="n", adj=0.25, cex=1.4)
      }
      
      if (j==n.ages - i + 1) {
        plot.new(); box()
        legend("center", legend=paste0("age ", ages[j], ifelse(with.plus.gp & j==n.ages, "+", "")), bty="n", adj=0.25, cex=1.4)
      } 
      
      if (!(j <= n.ages - i + 1)) {
        id <-which(!is.na(dat[,j]) & !is.na(dat[,n.ages-i+1]))
        
        y <- dat[id,n.ages-i+1]
        x <- dat[id,j]
        xs <- x[!is.na(x) & !is.na(y)] 
        ys <- y[!is.na(x) & !is.na(y)] 
        x <- xs[xs > 0 & ys > 0]
        y <- ys[xs > 0 & ys > 0]
        x <- log(x); y <- log(y)
        xmin <- min(x)-(max(x)-min(x))*r
        xmax <- max(x)+(max(x)-min(x))*r
        ymin <- min(y)-(max(y)-min(y))*r
        ymax <- max(y)+(max(y)-min(y))*r
        plot(NA, NA, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
        polygon(x=100*c(-1, 1, 1, -1, -1), y=100*c(-1, -1, 1, 1, -1), col=col.reg[(cor(x, y)+1)*50/2], border=NA)
        box()
        points(x, y, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
        abline(lsfit(c(x), c(y)))
      }
      
    } 
  }
  mtext(s.name, side=3, outer=T, line=0.2, cex=1.3)
  
}

bioplot <-function(fit){
  miny <-min(fit$data$years)
  maxy <-max(fit$data$years)
  if(fit$conf$stockWeightModel==1){
    swa <- exp(fit$pl$logSW)
    matplot(fit$data$years, fit$data$stockMeanWeight, xlab="Year", ylab="Stock weights",xlim=c(miny,maxy+3))
    matplot(miny:(miny+nrow(swa)-8), swa[1:(nrow(swa)-7),], type="l", add=TRUE, lty="solid", lwd=2)
    abline(v=maxy, lty=2)
  }
  
  if(fit$conf$catchWeightModel==1){
    cwa <- exp(fit$pl$logCW)
    matplot(fit$data$years, fit$data$catchMeanWeight[,,1], xlab="Year", ylab="Catch weights",xlim=c(miny,maxy+3))
    matplot(miny:(miny+nrow(cwa)-8), cwa[1:(nrow(cwa)-7),,1], type="l", add=TRUE, lty="solid", lwd=2)
    abline(v=maxy, lty=2)
    
  }
  if(fit$conf$mortalityModel==1){
    ma <- exp(fit$pl$logNM)
    matplot(fit$data$years, fit$data$natMor, xlab="Year", ylab="Natural mortality",xlim=c(miny,maxy+3))
    matplot(miny:(miny+nrow(ma)-8), ma[1:(nrow(ma)-7),], type="l", add=TRUE, lty="solid", lwd=2)
    abline(v=maxy, lty=2)
    
  }
  if(fit$conf$matureModel==1){
    mo <- plogis(fit$pl$logitMO)
    matplot(fit$data$years, fit$data$propMat, xlab="Year", ylab="Proportion mature",xlim=c(miny,maxy+3))
    matplot(miny:(miny+nrow(mo)-9), mo[1:(nrow(mo)-8),], type="l", add=TRUE, lty="solid", lwd=2)
    abline(v=maxy, lty=2)
    
  }
  
}


plotfit_extra<-function(fit, retro=NULL, residual=NULL,procerr=NULL, name=paste(deparse(substitute(fit)), collapse = "\n")){
  op<-par(oma = c(2, 10, 2, 0), mar = c(0, 0, 0, 0), xpd = NA) # to clean up after data plot 
  dataplot(fit)
  par(op)
  par(mfrow=c(2,2))  
  recplot(fit) 
  ssbplot(fit)
  fbarplot(fit, partial=FALSE)
  catchplot(fit)
  title(name, outer=TRUE, line=-2)
  if(!is.null(retro)){
    m0<-stockassessment::mohn(retro)
    m1<-stockassessment::mohn(retro, lag=1)
    recplot(retro); legend("topright", legend=bquote(rho[Mohn] == .(round(m0[1]*100))*"%"), bty="n") 
    ssbplot(retro); legend("topright", legend=bquote(rho[Mohn] == .(round(m0[2]*100))*"%"), bty="n") 
    fbarplot(retro, partial=FALSE); legend("topright", legend=bquote(rho[Mohn] == .(round(m0[3]*100))*"%"), bty="n") 
    catchplot(retro)
    
    #parplot(retro)
    title(name, outer=TRUE, line=-2)
  }
  par(mfrow=c(1,1))
  if(!is.null(residual)){
    plot(residual)
    title(name, outer=TRUE, line=-2)
    par(mfrow=c(1,1))
  }
  
  if(!is.null(procerr)){
    plot(procerr)
    title(name, outer=TRUE, line=-2)
    par(mfrow=c(1,1))
  }
  fitplot(fit)
  par(mfrow=c(1,1))
  
  obscorrplot(fit)
  par(mfrow=c(1,1))
  
  if(nrow(fit$conf$keyLogFpar)>2){
    qtableplot(qtable(fit))
  }
  
  par(mfrow=c(1,1))
  bioplot(fit)
  
  par(mfrow=c(1,1))
  
  fay.tab <- faytable(fit)
  plot(as.numeric(row.names(fay.tab)),fay.tab[,dim(fay.tab)[2]], type='l', ylim=c(0,max(fay.tab[,dim(fay.tab)[2]])),
       ylab="F at age",xlab="")
  for(i in 1:(dim(fay.tab)[2]-1)) { lines(as.numeric(row.names(fay.tab)),fay.tab[,i],col=i) }
  legend("topright",inset=0.02,legend=paste("Age",colnames(fay.tab)),col=1:ncol(fay.tab),lwd=2,lty=1,cex=0.7,ncol=2)
  
  
  par(mfrow=c(1,1))
  
  # Selectivity
  Fsel <- faytable(fit)/rowSums(faytable(fit))
  
  plot(rownames(Fsel),Fsel[,1],xlab="Year", main = "Selectivity in F",type="n",ylim=range(Fsel),ylab="")
  for(i in 1:ncol(Fsel)){
    lines(rownames(Fsel),Fsel[,i],col=i,lwd=2)
  }
  legend("topleft",inset=0.02,legend=paste("Age",colnames(Fsel)),col=1:ncol(Fsel),lwd=2,lty=1,cex=0.7,ncol=2)
  
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
    legend("topleft",inset=0.02,legend=paste(rownames(toplot)),col=1:nrow(toplot),lwd=2,lty=1,cex=0.7,ncol=2)
    
  }
  par(mfrow=c(1,1))
  sdplot(fit)
  
  
}


record.diagnostics <- function(fit=fit,name="base run",desc="fit conf description",int.yr.survey=F,
                               filename="SAM model runs.csv",append.to.existing=FALSE){
  
  # record SAM fit diagnostics 
  if(!class(fit) %in% "sam") stop("fit is not a SAM fit")
  df <- data.frame("Name"=name)
  df$Description <- toString(desc)
  
  df$npar<- modeltable(fit)[,"#par"]
  df$loglik<- modeltable(fit)[,"log(L)"]
  df$AIC<- modeltable(fit)[,"AIC"]
  df$BIC <- BIC(fit)
  
  m0 <- mohn(ret)
  nms <- names(m0)
  df$Mrho_SSB <- m0[c("SSB")]
  df$Mrho_Rec <- m0[nms[grep("R",nms)]]
  df$Mrho_Fbar <- m0[nms[grep("Fbar",nms)]]
  
  if(int.yr.survey){
    m1 <- mohn(ret, lag=1)
    df$Mrho_Fbar <- m1[nms[grep("Fbar",nms)]]
  }
  
  if(append.to.existing){
    write.table(df,file=filename,append=T,row.names=F,sep=",",col.names=F)
  }else{
    write.table(df,file=filename,row.names=F,sep=",")
  }
  print(df)
}
