# install.packages("gbm")
# c("gbm","xda","xgboost","reshape")

library(reshape)
library(gbm)
path="/home/aks/Desktop/test/Data/"
setwd(path)
dt<-read.csv("train.csv")
init_seed=1234
set.seed(init_seed)
dt$price_range<-ifelse(dt$price_range=="high",1,0)

myfunc<-function(dt, base_data, trees , shrinkage, min_obs, dep, gbm_seed, iter, sampling, rate){
  gbm_fuc<-function(dt_gbm, ntress , min_obs, sampling, depth, shrik, rate, gbm_seed){
    set.seed(gbm_seed)
    model <- gbm(
                price_range~. -Id -set_flag
               ,distribution="bernoulli"
               ,data=dt_gbm
               ,n.trees = ntress
               ,n.minobsinnode = min_obs
               ,shrinkage = shrik
               ,bag.fraction = rate
               ,interaction.dep=dep
               )
    return(model)
  }
 
  aks = data.frame()
  
  
  for(i in iter){
      sed = i*11223344
      set.seed(sed)
      select.obs <- runif(nrow(dt))
      select.cols <- c(1,sample(c(0,1), replace=TRUE, size=ncol(dt)-2),1,1)
      select.cols <- which(select.cols %in% 1)
      dt_tr <- dt[select.obs<0.7,]
      dt_tr$set_flag <- "Training"
      dt_test <- dt[select.obs>=0.7,]
      dt_test$set_flag <- "Test"
      dt_gbm <- rbind(dt_tr, dt_test) 
      assign(paste0("gbm_model_",i)
             ,value=gbm_fuc(dt_gbm = dt_gbm[dt_gbm$set_flag == "Training", select.cols]
                               , ntress = trees
                               , min_obs=min_obs
                               , sampling = sampling 
                               , depth = dep
                               , shrik=shrinkage
                               , rate = rate
                               ,gbm_seed=gbm_seed
                               )
      )
      dt_gbm$iteration <- i
      aks <- rbind(dt_gbm,aks)
  
  }
  tmp2 <- data.frame()
  for(i in iter){
    pred_type <- paste0("pred_tree_", i, "_model_", i)
    tmp <- as.data.frame(predict.gbm(eval(parse(text = paste0("gbm_model_",i))), newdata=aks, n.trees=trees, single.tree=TRUE, type="response"))
    colnames(tmp) <- c(pred_type)
    aks <- cbind(aks, tmp)
    aks[,paste0("log_loss_",i)] <- -( aks[,"price_range"]*log(aks[,pred_type]) + (1-aks[,"price_range"])*(log(1-aks[,pred_type])))
    tmp1 <- aggregate(eval(parse(text = paste0("log_loss_",i)))~set_flag+iteration, aks, mean)
    colnames(tmp1)[3]<-"log_loss"
    tmp1$model <- i
    tmp2<-rbind(tmp2,tmp1)
  }
  
  tmp3 <- reshape(tmp2, idvar = c("iteration", "model"), timevar = "set_flag", direction = "wide")
  tmp3$log_loss_diff <- abs(tmp3$log_loss.Training-tmp3$log_loss.Test)
  tmp4<-aggregate(log_loss_diff ~ model, tmp3, mean)
  
  
  rownames(tmp4)<-tmp4$model
  tmp4$model <- NULL
  tmp5<-as.data.frame(t(tmp4))
    
  model_selected <- colnames(tmp5)[apply(tmp5,1,which.min)]
  final_model <- eval(parse(text=paste0("gbm_model_",model_selected)))
  base_data$pred <- predict.gbm(final_model, newdata=base_data, n.trees=1, single.tree=TRUE, type="response")
  pred_type<-"pred"
  base_data$log_loss <- -(base_data[,"price_range"]*log(base_data[,pred_type]) + (1-base_data[,"price_range"])*(log(1-base_data[,pred_type])))
  return(list(base_data, final_model, summary(final_model), mean(base_data$log_loss)))
}

models <-list()
var_imp <- list()
loss <- list()
bull <- myfunc(dt = dt, base_data =dt, trees =1, shrinkage = 0.01, min_obs = c(30), dep = 3, gbm_seed = 1234, iter = c(0,1,2,3), sampling = 1, rate = 1)
new_data <- bull[[1]]
models <- append(models, bull[2])
var_imp <- append(var_imp, bull[3])
loss <- append(loss, bull[4])

for (i in seq(1:100)){
  new_data$rank <- rank(new_data$log_loss, na.last = TRUE,ties.method = c("first"))
  new_data1 <- new_data[new_data$rank >= nrow(new_data)*0.5,]
  new_data1$rank <- NULL
  new_data1$log_loss <- NULL
  bull <- myfunc(dt = new_data1, base_data = new_data, trees =1, shrinkage = 0.01, min_obs = c(30), dep = 3, gbm_seed = 1234, iter = c(0,1,2,3), sampling = 1, rate = 1)
  new_data <- bull[[1]]
  models <- append(models, bull[2])
  var_imp <- append(var_imp, bull[3])
  loss <- append(loss, bull[4])
}
me<-unlist(loss, recursive = TRUE, use.names = TRUE)
plot(me)
lines(me)
