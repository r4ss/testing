# source('c:/SS/morphs/R/SSplotDomeBio.R')
SSplotDomeBio <- function(replist, fleet=1, gender=1, col=1, subplots=1:3, add=FALSE,
                          morphs="all", checktime=FALSE, lwd=2, area=1){

  #  tt <- Sys.time()
  
  f <- fleet
  m <- gender

  iarea <- area

  # get things from replist
  natage     <- replist$natage
  natlen     <- replist$natlen
  biology    <- replist$biology
  sizeselex  <- replist$sizeselex
  nlbinspop  <- replist$nlbinspop
  x          <- replist$lbinspop
  ngpatterns <- replist$ngpatterns
  # subset
  sel <- sizeselex[sizeselex$Factor=="Lsel" & sizeselex$gender==m & sizeselex$Fleet==f,]
#print(sel)  
  natlen <- natlen[natlen$Gender==m,]

  
  # warning! implementation of birthseasons may not be correct in this section
  # data frame to combine values across factors
  natlentemp_all <- natlen[natlen$Area==iarea &
                           natlen$Gender==m &
                           natlen$Seas==1 &
                           natlen$Yr>=replist$startyr &                           
                           natlen$BirthSeas==min(natlen$BirthSeas),]

  SS_versionshort <- toupper(substr(replist$SS_version,1,8))
  column1 <- 11
  remove <- -(1:column1) # removes first group of columns
  natlentemp_all <- natlentemp_all[natlentemp_all$"Beg/Mid"=="B",]

  # create data frame with 0 values to fill across submorphs
  morphlist <- unique(natlentemp_all$SubMorph)

  cat("morphlist:",morphlist,"\n")
  if(morphs[1]=="all") morphs <- morphlist
  cat("morphs:",morphs,"\n")
  
  natlentemp0 <- natlentemp_all[natlentemp_all$SubMorph==morphlist[1] &
                                natlentemp_all$Bio_Pattern==1,]
 
  for(ilen in 1:nlbinspop) natlentemp0[,column1 + ilen] <- 0 # matrix of zeros for upcoming calculations
#print(morphs)
#print(ngpatterns)  
  for(imorph in morphs){
    for(igp in 1:ngpatterns){
      natlentemp_imorph_igp <- natlentemp_all[natlentemp_all$SubMorph==morphlist[imorph] &
                                              natlentemp_all$Bio_Pattern==igp,]
      natlentemp0[,column1+1:nlbinspop] <- natlentemp0[,column1+1:nlbinspop] +
                                 natlentemp_imorph_igp[,column1+1:nlbinspop]
    } # end growth pattern loop
  } # end morph loop
  if(ngpatterns>0) natlentemp0$Bio_Pattern==999

  # check for time varying selectivity
  if(checktime){
    time <- FALSE
    for(t in 5 + 1:nlbinspop)
      if(length(unique(sel[,t]))>1)
        time <- TRUE
    if(time) warning("time-varying selectivity not supported in this function, selectivity from first year is used")
  }
  # subset again (selD is dome-shaped selectivity, selA is asymptotic)
  selD <- selA <- sel[sel$year==max(sizeselex$year),]

  # remove descending limb of selA
  for(i in 1:nrow(selA)){
    ncol <- ncol(selA)
    nfull <- max((6:ncol)[selA[i,6:ncol]>0.99999]) # last column with full selex
    selA[i,nfull:ncol] <- 1
  }

  if(mean(selA==selD)==1) warning("selectivity for this fleet is not dome-shaped")
  # get biomass-at-length
  
  n <- nrow(natlentemp0)
  n2 <- ncol(natlentemp0) - column1
  biolen <- natlentemp0 # get dimensions from numbers-at-length
  biolen[,remove] <- NA    # remove old values

  if(m==1) biolen[,remove] <- natlentemp0[,remove]*matrix(as.numeric(biology$Wt_len_F),nrow=n,ncol=n2,byrow=T)
  if(m==2) biolen[,remove] <- natlentemp0[,remove]*matrix(as.numeric(biology$Wt_len_M),nrow=n,ncol=n2,byrow=T)

  # selected biomass-at-length
  selbiolenD <- biolen
  selbiolenD[,remove] <- NA
  selbiolenA <- selbiolenD

  # fill in values
  selbiolenD[,remove] <- biolen[,remove]*matrix(as.numeric(selD[, -(1:5)]),nrow=n,ncol=n2,byrow=T)
  selbiolenA[,remove] <- biolen[,remove]*matrix(as.numeric(selA[, -(1:5)]),nrow=n,ncol=n2,byrow=T)


  selbiolenD$total <- as.numeric(apply(selbiolenD[,remove],1,sum))
  selbiolenA$total <- as.numeric(apply(selbiolenA[,remove],1,sum))
  
  timeD <- aggregate(selbiolenD$total, by=list(year=selbiolenD$Yr), FUN=sum)
  timeA <- aggregate(selbiolenA$total, by=list(year=selbiolenA$Yr), FUN=sum)
  yrs <- timeD$year

  yD <- as.numeric(selbiolenD[1,names(selbiolenD)%in%x ])
  yA <- as.numeric(selbiolenA[1,names(selbiolenA)%in%x ])
  yDb <- as.numeric(selbiolenD[nrow(selbiolenA),names(selbiolenA)%in%x ])
  yAb <- as.numeric(selbiolenA[nrow(selbiolenA),names(selbiolenA)%in%x ])

  missing <- 1 - timeD$x/timeA$x
  if(1 %in% subplots){
    if(!add) plot(yrs, missing, type='n',ylim=range(0,missing),
                  xlab='Year', ylab='Fraction of biomass unselected due to dome-shaped selectivity')
    lines(yrs, missing, lwd=2, col=col)
    abline(h=0, col='grey')
    print(paste('fraction missing in first, last years: ',
                round(missing[1],4), ', ',
                round(missing[length(yrs)],4),sep=''),quote=FALSE)
  }

  if(2 %in% subplots){
    # note that x is the population length bins from replist
    #windows()
    # selected biomass at length
    scale <- max(yD,yA)
    par(mar=c(5,4,1,4)+.1)
    plot(x,x,type='n',ylim=c(0,scale),xlab='Length',ylab='Selected biomass within length bin' )
    abline(h=0,lty=3,col='grey')
    #    abline(h=scale,lty=3,col='grey')
    # first year
    lines(x,yD,lwd=lwd,col=1)
    lines(x,yA,lwd=lwd,col=1,lty=2)
    # final year
    lines(x,yDb,lwd=lwd,col=2)
    lines(x,yAb,lwd=lwd,col=2,lty=2)
    # selectivity
    lines(x, scale*selD[1,-(1:5)],lwd=lwd,col=4)
    lines(x, scale*selA[1,-(1:5)],lwd=lwd,col=4,lty=2)
    # axes
    axis(4,at=scale*pretty(c(0,1)),lab=pretty(c(0,1)))
    mtext(side=4,line=2.5,'Selectivity')
  }

  if(3 %in% subplots){
    ## cumulative plots
    plot(x,x,type='n',ylim=c(0,1),xlab='Length',ylab='Cumulative selected biomass (fraction of total)' )
    # first year
    lines(x,cumsum(yD)/sum(yD),lwd=lwd,col=1)
    lines(x,cumsum(yA)/sum(yA),lwd=lwd,col=1,lty=2)
    # final year
    lines(x,cumsum(yDb)/sum(yDb),lwd=lwd,col=2)
    lines(x,cumsum(yAb)/sum(yAb),lwd=lwd,col=2,lty=2)
    # selectivity
    lines(x, selD[1,-(1:5)],lwd=lwd,col=4)
    lines(x, selA[1,-(1:5)],lwd=lwd,col=4,lty=2)
  }

  if(2 %in% subplots | 3 %in% subplots){
    legend('topleft',bty='n',lty=c(1,1,1,NA,1,2),col=c(1,2,4,NA,1,1),lwd=lwd,
           legend=c(paste('biomass at length in',yrs[1]),
             paste('biomass at length in',tail(yrs,1)),
             paste('selectivity at length in',yrs[1]),
             '',
             'with dome',
             'without dome')
           )
  }

  #  print(Sys.time()-tt)
  #return(invisible(list(missing=missing,yD=yD,yA=yA,yDb=yDb,yAb=yAb,timeD=timeD,timeA=timeA,selD=selD,selA=selA)))
  return(invisible(missing))
}
