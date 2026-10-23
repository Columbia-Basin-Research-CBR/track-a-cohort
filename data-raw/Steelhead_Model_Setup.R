#' @title Steelhead Model Setup - Tillotson et al 2022 
#' @description This script was shared via BOR request to set-up Tillotson et al 2022 steelhead model. 
#' Model can be run with combined facilities (Simple_Combined) or by facility (Simple_FACILITY). For TAC, the model is run with combined facility. 
#' Main difference between species model code is the response variable, predictors are the same. 
#' @return script is called in `utils_fct_predict_tillotson_model.R` to predict weekly loss for Steelhead.

require(randomForest)
require(quantregForest)
require(tidyverse)
require(stringr)
require(lubridate)
require(zoo)
require(caret)
source(here::here("R/brt.functions.R"))
select <- dplyr::select

df.ss <- read.csv("https://cbr.washington.edu/sacramento/data/generated/tac_weekly_calib.csv")
df.ss <- df.ss[df.ss$water_year >= 2009 & df.ss$water_year <= 2025,]
df.week <- df.ss 
df.week$dcc <- as.factor(df.week$dcc)
df.ss$dcc <- as.factor(df.ss$dcc)
df.week <- df.week %>% mutate(dcc= if_else(dcc==1,"open","closed"))
df.ss <- df.ss %>% mutate(dcc= if_else(dcc==1,"open","closed"))


#=========================Flexible function for fitting two-part incidental take model=====================

hurdle.model <- function(data, response, 
                         binary.predictors, 
                         quantile.predictors, 
                         binary.form="rf",
                         quantile.form="qrf",
                         predict.data=NA,
                         which.quant=.5,
                         n.trees=500,
                         quantile.nodesize=10,
                         log.quant=F,
                         pres.only = T,
                         ...){
  resp.loc <- (which(colnames(data)==response))
  
  data.binary <- data.frame(data)
  data.binary[,resp.loc] <- as.factor(ifelse(data[,resp.loc]==0,0,1))
  names(data.binary)[resp.loc] <- response
  data.pres <- data.frame(data[data[,resp.loc]!=0,])
  
  if(pres.only == F){
    data.pres <- data.frame(data)
  }
  
 if(binary.form=="rf"){
    
    binary.model <- randomForest(x=data.binary[,binary.predictors],y=data.binary[,resp.loc],
                                 ntree = n.trees,
                                 importance = T, 
                                 proximity = T,
                                 keep.forest = T,
                                 keep.inbag = T,
                                 rsq = T)
  }
  
  
  if(quantile.form=="qrf"&binary.form!="none"){

    quantile.model <- quantregForest(x=data.pres[,quantile.predictors],y=data.pres[,resp.loc],
                                     ntree = n.trees,
                                     importance = T, 
                                     proximity = T,
                                     keep.forest = T,
                                     keep.inbag = T,
                                     rsq = T,
                                     nodesize=quantile.nodesize)
  }
  
  
  if(binary.form=="none"){
    binary.model <-NA
    
    data <- data.frame(data)
    quantile.model <- quantregForest(x=data[,quantile.predictors],y=data[,resp.loc],
                                     ntree = n.trees,
                                     importance = T, 
                                     proximity = T,
                                     keep.forest = T,
                                     keep.inbag = T,
                                     rsq = T,
                                     nodesize=quantile.nodesize)
    
    
    if(is.na(predict.data)==T){
      prediction.a <- predict(quantile.model,newdata=data[,quantile.predictors],what=which.quant)
      
      Pred_Mean <- predict(quantile.model,newdata=data[,quantile.predictors],what=mean)
      
    }else{
      prediction.a <- (predict(quantile.model,newdata=predict.data,what=which.quant))
      
      Pred_Mean <- (predict(quantile.model,newdata=predict.data,what=mean))
    }
    
  }else{
    if(is.na(predict.data)==T){
      prediction.a <- as.numeric(as.character(predict(binary.model,newdata=data)))*
        (predict(quantile.model,newdata=data,what=which.quant))
      
      Pred_Mean <- as.numeric(as.character(predict(binary.model,newdata=data)))*
        (predict(quantile.model,newdata=data,what=mean))
      
      
    }else{
      prediction.a <- as.numeric(as.character(predict(binary.model,newdata=predict.data)))*
        (predict(quantile.model,newdata=predict.data,what=which.quant))
      
      Pred_Mean <- as.numeric(as.character(predict(binary.model,newdata=predict.data)))*
        (predict(quantile.model,newdata=predict.data,what=mean))
      
    }
  }
  
  prediction <- data.frame(cbind(prediction.a,Pred_Mean))
  
  
  return(list(binary.model,quantile.model,prediction))
}

# WAS With the previous load(.....rda) 
# steelhead.pred.cols <- c(2:4,17:21,29)
# steelhead.response <- "stlhd_loss"

# For CBR input data modifidd to be same is now also:
steelhead.pred.cols <- c(2:4,17:21,29)
steelhead.response <- "stlhd_loss"

Stlhd_Simple_Combined <- hurdle.model(data = df.ss,
                                      response = steelhead.response,
                                      binary.predictors = NA,
                                      quantile.predictors = steelhead.pred.cols,
                                      binary.form = "none",
                                      quantile.form = "qrf",
                                      which.quant = c(0.01,0.05,.1,.25,.5,.75,.9,.95,.99),
                                      n.trees=500,
                                      quantile.nodesize = 5,
                                      pres.only=F
)



#Facilities Separated

Stlhd_Simple_CVP <- hurdle.model(data = df.ss,
                                 response = "stlhd_loss_cvp",
                                 binary.predictors = NA,
                                 quantile.predictors = c(2:4,17:21,32),
                                 binary.form = "none",
                                 quantile.form = "qrf",
                                 which.quant = c(0.01,0.05,.1,.25,.5,.75,.9,.95,.99),
                                 n.trees=300,
                                 quantile.nodesize = 5,
                                 pres.only=F
)


Stlhd_Simple_SWP <- hurdle.model(data = df.ss,
                                 response = "stlhd_loss_swp",
                                 binary.predictors = NA,
                                 quantile.predictors = c(2:4,17:21,35),
                                 binary.form = "none",
                                 quantile.form = "qrf",
                                 which.quant = c(0.01,0.05,.1,.25,.5,.75,.9,.95,.99),
                                 n.trees=300,
                                 quantile.nodesize = 5,
                                 pres.only=F
)



