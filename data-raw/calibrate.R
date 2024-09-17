
# BEGIN: essential for calibration :: find line for END: essential for calibration

# seek below for flag named "allloss"

#This script aggregates data to a weekly level and its output files are then passed to the Shiny App
#any modifications, variable additions, etc. should be performed in this script
# moodified from original to accomodate CBR data input 
# and April 20 to accomodate the new steelhead loss computation and not s]just salvage
library(tidyr,quietly=TRUE)
library(DMwR,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(tibble,quietly=TRUE) 
library(stringr,quietly=TRUE)
library(lubridate,quietly=TRUE)
library(RCurl,quietly=TRUE)
library(randomForest,quietly=TRUE)
library(quantregForest,quietly=TRUE)
library(gridExtra,quietly=TRUE)
library(Hmisc)

#Read Daily take and covariate data
# df.main.orig <- read.csv("training_data_WY99_17_LOSS.csv")
# savenames <- names(df.main.orig)
savenames <- c( "date","water.year", "wy.week",    "month","X7.days","week", "week.dec", "day", "year.week", "steelhead", 
                "steel.cl",   "winter.cl",  "winter", "spring",  "mal.temp",   "precip", "om.sum", "sac", "dcc", "sjr", "export")


### Original fit was to salvage. Wat to make a loss version and compare.
# addon names will be "" for original or "2" for 2nd generation 
for(indexx in 1:4){
  if(indexx == 1){allloss <- FALSE; nameadd <- "" ; useyears <- 1999:2020;}
  if(indexx == 2){allloss <- TRUE ; nameadd <- "2"; useyears <- 1999:2020;}

  # Using new calibration data 2009 through WY 2020 
  if(indexx == 3){allloss <- FALSE; nameadd <- "3"; useyears <- 2009:2020; }
  if(indexx == 4){allloss <- TRUE ; nameadd <- "4"; useyears <- 2009:2020;}
  
  
  
  df.main <- read.csv("AllYears.Intake.csv")
  # Need to modify datastructure to match original
  df.main <- cbind.data.frame(df.main,"week"=53)
  minY <- min(useyears)
  maxY <- max(useyears)
  df.main <- df.main[df.main$water_year >= minY & df.main$water_year <= maxY,]
  df.main$week <- (df.main$julian -1)  %/% 7 + 1
  names(df.main)
  names(df.main)[match("x",names(df.main),nomatch=0)] <- "x"
  names(df.main)[match("mal_temp",names(df.main),nomatch=0)] <- "temp.mal"
  names(df.main)[match("daily_exports",names(df.main),nomatch=0)] <- "export"
  names(df.main)[match("julian",names(df.main),nomatch=0)] <- "day"
  names(df.main)[match("wk_dec",names(df.main),nomatch=0)] <- "week.dec"
  names(df.main)[match("omr_tf_daily",names(df.main),nomatch=0)] <- "om.sum"
  names(df.main)[match("year_wk",names(df.main),nomatch=0)] <- "year.week"
  names(df.main)[match("water_year",names(df.main),nomatch=0)] <- "water.year"
  names(df.main)[match("wy_week",names(df.main),nomatch=0)] <- "wy.week"
  names(df.main)[match("x7_days",names(df.main),nomatch=0)] <- "x7.days"
  names(df.main)[match("steel_cl",names(df.main),nomatch=0)] <- "steel.cl"
  names(df.main)[match("winter_cl",names(df.main),nomatch=0)] <- "winter.cl"
  names(df.main)[match("stlhd_loss",names(df.main),nomatch=0)] <- "stlhd.loss"
  
  if(0){ # an exploration of df.main
    plot(df.main$stlhd.loss,df.main$steelhead,type="n",xlim=c(0,100),ylim=c(0,40),ylab="Steelhead Salvage",xlab="Steelhead Loss")
    palette(rainbow(12))
    junk <-  unique(df.main$water.year)
    for(i in junk){points(df.main$stlhd.loss[df.main$water.year == i],df.main$steelhead[df.main$water.year == i],
                          col=i-min(junk)+1,
                          pch=i-min(junk)+1,
                          cex=3 - 2*(i-min(junk))/10)
      } 
    legend("topleft",cex=1,bty="n",legend=junk,col=junk-min(junk)+1,pch=junk-min(junk)+1,pt.cex=3 - 2*(junk-min(junk))/10)
  }
  if(0){ # an exploration of df.main
    plot(df.main$steelhead,df.main$stlhd.loss,type="n",ylim=c(0,100),xlim=c(0,40),xlab="Steelhead Salvage",ylab="Steelhead Loss")
    palette(rainbow(12))
    junk <-  unique(df.main$water.year)
    for(i in junk){points(df.main$steelhead[df.main$water.year == i],df.main$stlhd.loss[df.main$water.year == i],
                          col=i-min(junk)+1,
                          pch=i-min(junk)+1,
                          cex=3 - 2*(i-min(junk))/10)
    } 
    legend("topleft",cex=1,bty="n",legend=junk,col=junk-min(junk)+1,pch=junk-min(junk)+1,pt.cex=3 - 2*(junk-min(junk))/10)
  } 
  
  
  if(allloss) {
    df.main <- df.main[,c(1:5,22,6:8,11,13:16,9,10,17:21)]
    names(df.main)[10] <- "steelhead"
  } else {
    ## Now the default for historic is column shifted by one to right beginning with old column 11 "steelhead"
    df.main <- df.main[,c(1:5,22,6:8,12:16,9,10,17:21)]
  }
  print(names(df.main))
  
  # reorder to match original dataframe
  df.main$dcc[df.main$dcc == 3] <- 0.5
  
  # test
  cbind(names(df.main.orig),names(df.main))
  
  # HARD dataframe parsing to ge complete water years
  df.main <- df.main[df.main$water.year <= max(useyears),]
  
  df.main$year.week <- df.main$water.year + df.main$week.dec
  ##### NOW the new dataframe shoudl be completely compatible with the old one and fitting process
  
  #subest of only covariates
  df.covars <-df.main[,c(15:21)]
  #Prepare data to weekly format======================================================
  #Create data frame with only covariates
  df.covars <- data.frame(day = df.main$day,df.covars)
  
  #Examine distribution of NAs accross covariates
  na.check <- data.frame(variable = rep(NA,8), NAs = rep(NA,8))
  for(i in 1:ncol(df.covars)){
    na.check$variable[i] <- names(df.covars[i])  
    na.check$NAs[i] <- length(which(is.na(df.covars[,i])))
  }
  
  #applies k-Nearest Neighbor imputaiton to deal with missing values in covariates
  df.covars.filled <- data.frame(df.main[,c(3,9)],knnImputation(df.covars))
  #Aggregate to weekly dataset
  #Total salmonid counts
  steelhead <- aggregate(df.main$steelhead ~ df.main$year.week + df.main$water.year, FUN= sum)
  winter <- aggregate(df.main$winter ~ df.main$year.week + df.main$water.year, FUN= sum)[,3]
  winter.cl <- aggregate(df.main$winter.cl ~ df.main$year.week + df.main$water.year, FUN= sum)[,3]
  steel.cl <- aggregate(df.main$steel.cl ~ df.main$year.week + df.main$water.year, FUN= sum)[,3]
  spring <- aggregate(df.main$spring ~ df.main$year.week + df.main$water.year, FUN= sum)[,3]
  
  #combine all weekly salvage in to single data frame
  weekly.salvage <- cbind(steelhead,winter.cl,steel.cl,winter,spring)
  
  #Create weekly averages for covariates
  wy.week <- aggregate(wy.week ~ year.week , data = df.covars.filled, mean)[,2]
  temp.mal <- aggregate(temp.mal ~ year.week , data = df.covars.filled, mean)[,2]
  precip <- aggregate(precip ~ year.week, data = df.covars.filled, mean)[,2]
  om.sum <- aggregate(om.sum ~ year.week, data = df.covars.filled, mean)[,2]
  export <- aggregate(export ~ year.week, data = df.covars.filled, mean)[,2]
  sac <- aggregate(sac ~ year.week, data = df.covars.filled, mean)[,2]
  sjr <- aggregate(sjr ~ year.week, data = df.covars.filled, mean)[,2]
  dcc <- aggregate(dcc ~ year.week, data = df.covars.filled, sum)[,2]
  dcc[dcc <3] <- "closed"
  dcc[dcc != "closed"] <- "open"
 
  #complile weekly values into data frame
  df.week <- data.frame(weekly.salvage,
                        wy.week,
                        temp.mal,
                        precip,
                        om.sum,
                        export,
                        sac,
                        sjr,
                        dcc)
  #Correct names of first three columns
  names(df.week)[1:3] <- c("year.week","water.year","steelhead")
  
  
  
  #Offset take to previous week
  df.week.pw <-cbind(df.week[2:(nrow(df.week)),],df.week[1:nrow(df.week)-1,3:15])
  names(df.week.pw)[16:20] <- c("steelhead.pw",
                                "steel.cl.pw",
                                "winter.cl.pw",
                                "winter.pw",
                                "spring.pw")
  names(df.week.pw)[22:28] <- c("temp.mal.pw",
                                "precip.pw",
                                "om.sum.pw",
                                "export.pw",
                                "sac.pw",
                                "sjr.pw",
                                "dcc.pw")                           
  
  #Appended the previous week data to weekly data now return it to dataframe df.week
  df.week <- df.week.pw
  
  
  #================================================Model Training============================================
  # full.fits <- list(qrf.steelhead,qrf.winter,qrf.spring)
  # MADE with Tilloton's orinal calibrations
  # save(full.fits,file="FullFits.Original.R")
  
  
  #Create variable subsets for each run
  predictors.steelhead <- c(8:16)
  predictors.winter <- c(8:15,19)
  predictors.spring <- c(8:15,20)
  
  
  # Compute full Training set fits
  qrf.steelhead  <-quantregForest(x=df.week[,predictors.steelhead],y=df.week$steelhead,
                                 ntree = 1000,
                                 importance = T,
                                 proximity = T,
                                 keep.forest = T,
                                 keep.inbag = T,
                                 do.trace = 100,
                                 rsq = T)
  qrf.winter <-quantregForest(x=df.week[,predictors.winter],y=df.week$winter,
                              ntree = 1000,
                              importance = T,
                              proximity = T,
                              keep.forest = T,
                              keep.inbag = T,
                              do.trace = 100,
                              rsq = T)
  
  # qrf.spring <-quantregForest(x=df.week[,predictors.spring],y=df.week$spring,
  #                             ntree = 1000,
  #                             importance = T,
  #                             proximity = T,
  #                             keep.forest = T,
  #                             keep.inbag = T,
  #                             do.trace = 100,
  #                             rsq = T)
  
  if(nameadd==""){  full.fits  <- list(qrf.steelhead,qrf.winter) ; save(full.fits, file=paste("FullFits.R",sep="")) }
  if(nameadd=="2"){ full.fits2 <- list(qrf.steelhead,qrf.winter) ; save(full.fits2,file=paste("FullFits2.R",sep="")) }
  if(nameadd=="3"){ full.fits3 <- list(qrf.steelhead,qrf.winter) ; save(full.fits3,file=paste("FullFits3.R",sep="")) }
  if(nameadd=="4"){ full.fits4 <- list(qrf.steelhead,qrf.winter) ; save(full.fits4,file=paste("FullFits4.R",sep="")) }
  
  
  # load(paste("FullFits.",nameadd,".R",sep=""))
  # qrf.steelhead <- full.fits[[1]]
  # qrf.winter <- full.fits[[2]]
  # qrf.spring <- full.fits[[3]]
  
  
  #==============A block to run through drop one year analysis and save fitted objects to list Separated fro the observations to speed process
all.years <- seq(1999,2020)
#Create data subsets excluding each year.
{
  df.week.1999.rm <- df.week[df.week$water.year != 1999,]
  df.week.2000.rm <- df.week[df.week$water.year != 2000,]
  df.week.2001.rm <- df.week[df.week$water.year != 2001,]
  df.week.2002.rm <- df.week[df.week$water.year != 2002,]
  df.week.2003.rm <- df.week[df.week$water.year != 2003,]
  df.week.2004.rm <- df.week[df.week$water.year != 2004,]
  df.week.2005.rm <- df.week[df.week$water.year != 2005,]
  df.week.2006.rm <- df.week[df.week$water.year != 2006,]
  df.week.2007.rm <- df.week[df.week$water.year != 2007,]
  df.week.2008.rm <- df.week[df.week$water.year != 2008,]
  df.week.2009.rm <- df.week[df.week$water.year != 2009,]
  df.week.2010.rm <- df.week[df.week$water.year != 2010,]
  df.week.2011.rm <- df.week[df.week$water.year != 2011,]
  df.week.2012.rm <- df.week[df.week$water.year != 2012,]
  df.week.2013.rm <- df.week[df.week$water.year != 2013,]
  df.week.2014.rm <- df.week[df.week$water.year != 2014,]
  df.week.2015.rm <- df.week[df.week$water.year != 2015,]
  df.week.2016.rm <- df.week[df.week$water.year != 2016,]
  df.week.2017.rm <- df.week[df.week$water.year != 2017,]
  df.week.2018.rm <- df.week[df.week$water.year != 2018,]
  df.week.2019.rm <- df.week[df.week$water.year != 2019,]
  df.week.2020.rm <- df.week[df.week$water.year != 2020,]
  
  
  #Create data subsets of single years
  df.week.1999<- df.week[df.week$water.year == 1999,]
  df.week.2000<- df.week[df.week$water.year == 2000,]
  df.week.2001<- df.week[df.week$water.year == 2001,]
  df.week.2002<- df.week[df.week$water.year == 2002,]
  df.week.2003<- df.week[df.week$water.year == 2003,]
  df.week.2004<- df.week[df.week$water.year == 2004,]
  df.week.2005<- df.week[df.week$water.year == 2005,]
  df.week.2006<- df.week[df.week$water.year == 2006,]
  df.week.2007<- df.week[df.week$water.year == 2007,]
  df.week.2008<- df.week[df.week$water.year == 2008,]
  df.week.2009<- df.week[df.week$water.year == 2009,]
  df.week.2010<- df.week[df.week$water.year == 2010,]
  df.week.2011<- df.week[df.week$water.year == 2011,]
  df.week.2012<- df.week[df.week$water.year == 2012,]
  df.week.2013<- df.week[df.week$water.year == 2013,]
  df.week.2014<- df.week[df.week$water.year == 2014,]
  df.week.2015<- df.week[df.week$water.year == 2015,]
  df.week.2016<- df.week[df.week$water.year == 2016,]
  df.week.2017<- df.week[df.week$water.year == 2017,]
  df.week.2018<- df.week[df.week$water.year == 2018,]
  df.week.2019<- df.week[df.week$water.year == 2019,]
  df.week.2020<- df.week[df.week$water.year == 2020,]
  
  #=======================Compile single years and excluded years in to lists -----
  
  single.years <- list(df.week.1999,df.week.2000,df.week.2001,df.week.2002,
                       df.week.2003,df.week.2004,df.week.2005,df.week.2006,df.week.2007,
                       df.week.2008,df.week.2009 ,df.week.2010 ,df.week.2011 ,df.week.2012 ,
                       df.week.2013 ,df.week.2014 ,df.week.2015 ,df.week.2016 ,df.week.2017 ,
                       df.week.2018 ,df.week.2019,df.week.2020 )
  
  rm.years <- list(df.week.1999.rm,df.week.2000.rm,df.week.2001.rm,df.week.2002.rm,
                   df.week.2003.rm,df.week.2004.rm,df.week.2005.rm,df.week.2006.rm,df.week.2007.rm,
                   df.week.2008.rm,df.week.2009.rm,df.week.2010.rm,df.week.2011.rm,df.week.2012.rm,
                   df.week.2013.rm,df.week.2014.rm,df.week.2015.rm,df.week.2016.rm,df.week.2017.rm, 
                   df.week.2018.rm, df.week.2019.rm, df.week.2020.rm)
  
  
  #save(single.years,file="SingleYears.R")
  #save(rm.years,file="rmYears.R")
  #write.csv(df.week,"WeeklyData.csv",row.names=F)
}

whatyearindices <- match(useyears,all.years)
if(1){  # OBS
  w.obs.matrix <- NULL
  s.obs.matrix <- NULL
  for(i in whatyearindices){
    obs1 <- single.years[[i]][,6]
    w.obs.matrix <- cbind(w.obs.matrix,obs1[1:52])
  }
  w.obs.matrix.normalize <-  w.obs.matrix
  for(j in 1:dim(w.obs.matrix)[2]){w.obs.matrix.normalize[,j] <- cumsum(w.obs.matrix[,j])/sum(w.obs.matrix[,j])}
  meanobs <- valsobs <- 1:52
  for(j in 1:52){
    meanobs[j] <- mean(w.obs.matrix.normalize[j,])
    valsobs[j] <- sqrt(var(w.obs.matrix.normalize[j,]))
  }
  alt.winter.cum.plot.data <- cbind.data.frame("winter.mean"=meanobs,"winter.sd"=valsobs)
  
  for(i in whatyearindices){  
    obs1 <- single.years[[i]][,3]
    s.obs.matrix <- cbind(s.obs.matrix,obs1[1:52])
  }
  s.obs.matrix.normalize <-  s.obs.matrix
  for(j in 1:dim(s.obs.matrix)[2]){s.obs.matrix.normalize[,j] <- cumsum(s.obs.matrix[,j])/sum(s.obs.matrix[,j])}
  
  for(i in 1:52){
    valobs[i] <- sqrt(var(s.obs.matrix.normalize[i,]))
    meanobs[i] <- mean(s.obs.matrix.normalize[i,])
  }
  alt.steel.cum.plot.data <- cbind.data.frame("steel.mean"=meanobs,"steel.sd"=valobs)
  write.csv(cbind.data.frame(alt.steel.cum.plot.data, alt.winter.cum.plot.data),paste("CumPlotData",nameadd,".csv",sep=""),row.names=FALSE) 
}

# Remove one year fits
if(0){ # PRED
  #winter
  w.pred.matrix <- NULL
  s.pred.matrix <- NULL
  
  for(i in 1:22){
    qrf  <-quantregForest(x=rm.years[[i]][,predictors.winter],y=rm.years[[i]][,6],
                          ntree = 500,
                          importance = T,
                          proximity = T,
                          keep.forest = T,
                          keep.inbag = T,
                          do.trace = F,
                          rsq = T)
    assign(paste("winter.drop",out.years[i]),qrf)
    pred1 <- predict(qrf,newdata=single.years[[i]][,predictors.winter])
    w.pred.matrix<- cbind(w.pred.matrix,pred1[1:52,2])
  }
  
  w.pred.matrix.normalize <-  w.pred.matrix
  for(j in 1:21){w.pred.matrix.normalize[,j] <- cumsum(w.pred.matrix[,j])/sum(w.pred.matrix[,j])}
  
  vals <- meanpred <- 1:52
  for(j in 1:52){
    # vals[j] <- quantile(xx[j,],c(0.975))
    vals[j] <- sqrt(var(w.pred.matrix.normalize[j,]))
    meanpred[j] <- mean(w.pred.matrix.normalize[j,])
  }
  winter.cum.plot.data <- cbind.data.frame("winter.mean"=meanpred,"winter.sd"=vals)
  
  
  if(0){ # Examine pred and obs 
    for(j in 1:21){lines(s.obs.matrix.normalize[,j],type="l")}
    
    plot(w.obs.matrix.normalize[,1],type="l")
    for(j in 1:21){lines(w.obs.matrix.normalize[,j],type="l",col="grey")}
    
    plot(w.pred.matrix.normalize[,1],type="l")
    for(j in 1:21){lines(w.pred.matrix.normalize[,j],type="l",col="grey")}
    
    xx <- abs(w.obs.matrix.normalize - w.pred.matrix.normalize)
    vals <- meanpred <- meanobs <- 1:52
    for(i in 1:52){
      # vals[i] <- quantile(xx[i,],c(0.975))
      vals[i] <- sqrt(var(w.pred.matrix.normalize[i,]))
      meanpred[i] <- mean(w.pred.matrix.normalize[i,])
      meanobs[i] <- mean(w.obs.matrix.normalize[i,])
    }
    
    plot(meanpred,type="l",ylim=c(-0.1,1.1))
    lines(meanpred + 1.96*vals)
    lines(meanpred - 1.96*vals)
  } # end if(Examine) block  
  
  # #Steelhead
  # 
  for(i in 1:22){
    qrf  <-quantregForest(x=rm.years[[i]][,predictors.steelhead],y=rm.years[[i]][,3],
                          ntree = 500,
                          importance = T,
                          proximity = T,
                          keep.forest = T,
                          keep.inbag = T,
                          do.trace = F,
                          rsq = T)
    assign(paste("steelhead.drop",out.years[i]),qrf)
    
    pred1 <- predict(qrf,newdata=single.years[[i]][,predictors.steelhead])
    s.pred.matrix<- cbind(s.pred.matrix,pred1[1:52,2])
  }
  s.pred.matrix.normalize <-  s.pred.matrix
  for(i in 1:21){s.pred.matrix.normalize[,i] <- cumsum(s.pred.matrix[,i])/sum(s.pred.matrix[,i])}
  
  vals <- meanpred <- meanobs <- valobs <- meandels <- valdels <-  1:52
  for(i in 1:52){
    # vals[i] <- quantile(xx[i,],c(0.975))
    vals[i] <- sqrt(var(s.pred.matrix.normalize[i,]))
    meanpred[i] <- mean(s.pred.matrix.normalize[i,])
    meandels[i] <-  mean(s.pred.matrix.normalize[i,] - s.obs.matrix.normalize[i,])
    valdels[i] <- sqrt(var(s.pred.matrix.normalize[i,] - s.obs.matrix.normalize[i,]))
  }
  
  # order matters. this dataframe is written out, re--read and used byy column not nname at present
  steel.cum.plot.data <- cbind.data.frame("steel.mean"=meanpred,"steel.sd"=vals)
  alt2.steel.cum.plot.data <- cbind.data.frame("steel.mean"=meandels,"steel.sd"=valdels)
  
  ########################################### 
  # # #Spring
  # # 
  # # for(i in 1:21){
  # #   qrf  <-quantregForest(x=rm.years[[i]][,predictors.spring],y=rm.years[[i]][,7],
  # #                         ntree = 500,
  # #                         importance = T,
  # #                         proximity = T,
  # #                         keep.forest = T,
  # #                         keep.inbag = T,
  # #                         do.trace = 100,
  # #                         rsq = T)
  # #   assign(paste("spring.drop",out.years[i]),qrf)
  # #   
  # # }
  ###########################33 
  
  # "alt" version is for the observations
  write.csv(cbind.data.frame(alt.steel.cum.plot.data, alt.winter.cum.plot.data),paste("CumPlotData",nameadd,".csv",sep=""),row.names=FALSE) 

} # end remove one year fits. PRED
}

# END: essential for calibration
if(0){
          take.limit <- take.limit2 <- 1
        
        usemeans <- means <- steel.cum.plot.data[,1]
        usesd <- steel.cum.plot.data[,2]
        titlee <- "Distributions of the predictions"
        
        usemeans <- means <- alt.steel.cum.plot.data[,1]
        usesd <- alt2.steel.cum.plot.data[,2]
        titlee <- "Distributions of (pred - obs) around the observations"
        
        usemeans <- means <- alt.steel.cum.plot.data[,1]
        usesd <- alt.steel.cum.plot.data[,2]
        titlee <- "Distributions of the observations"
        
        usemeans <- means <- alt.winter.cum.plot.data[,1]
        usesd <- alt.winter.cum.plot.data[,2]
        titlee <- "Distributions of the Winter observations"
        
        usemeans <- means <- winter.cum.plot.data[,1]
        usesd <- winter.cum.plot.data[,2]
        titlee <- "Distributions of the Winter observations"
        
        
        par(mar=c(3,2,3,1))
        plot(seq(1,52),usemeans,type="l",xlab="Date",axes=F,col=rgb(0,.6,0),lwd=2,yaxs="i",
             xlim=c(1,42),ylim=c(-0.1,1.2),ylab="",main=title)
        axis(1,at=c(9.8,18.6,27,35.8,44.5),c("1-Dec","1-Feb","1-Apr","1-Jun","1-Aug"))
        axis(2)
        att <- c(min(means),(min(means)+max(means))/4,(min(means)+max(means))/2,(min(means)+max(means))*3/4,max(means))
        par(las=0)
        errors <- usesd*1.96   ## why did Tillotooson divide by sqrt(9) ??  Must review the cumplotdata for soure and replicability.
        polygon(c(seq(1,52),seq(52,1)),c(usemeans+errors,rev(usemeans-errors)),border=NA,col=rgb(0,.6,0,.25))
        box()
        title(titlee)
        
        #  MT's cum.plot.data
        cpd <- cum.plot.data <- read.csv("C:/Users/nick/Desktop/SacPAS/Incidental_Take_Prediction_121319.original.download//Incidental_Take_Prediction/CumPlotData.csv")
        cpd <-  cpd[1:52,]
        lines(1:52, cpd[,3],col="blue")
        polygon(c(seq(1,52),seq(52,1)),c(cpd[,3] - cpd[,4]*1.96,rev(cpd[,3] + cpd[,4]*1.96)),border=NA,col=rgb(0,0,0.6,.25))
        
        polygon(c(1,10,52,52,10,1),c(0,0,0,1,1,1),border=NA,col=rgb(0,.6,0,.25))
        title("100% prediction confidence !")
        box()
        
        plot(1:52,cpd[,2]-c(cpd[-1,2],0))
}