require(randomForest)
require(quantregForest)
require(tidyverse)
require(stringr)
require(lubridate)
require(zoo)
require(caret)
source("brt.functions.r")
select <- dplyr::select
load("ITMData.rda")

df.ss <- df.ss%>%mutate(dcc= if_else(dcc==2,"open","closed"))


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
    if(log.quant==T){
    }
    
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
      prediction.a <- predict(quantile.model,newdata=data,what=which.quant)
      
      Pred_Mean <- predict(quantile.model,newdata=data,what=mean)
      
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


Stlhd_Simple_Combined <- hurdle.model(data = df.ss,
                                      response = "stlhd_loss",
                                      binary.predictors = NA,
                                      quantile.predictors = c(2:4,17:21,29),
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




