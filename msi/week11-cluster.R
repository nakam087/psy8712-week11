# Script Settings and Resources
#removed setwd
library(tidyverse)
library(haven)
library(caret)
library(parallel)
library(doParallel)
library(tictoc)

# Data Import and Cleaning
gss_import_tbl <- read_spss("../data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS)) %>%
  select(-HRS1, -HRS2,-USUALHRS,-LEASTHRS) 

gss_tbl <- gss_import_tbl[, colSums(is.na(gss_import_tbl))<.75*nrow(gss_import_tbl)] %>%
  mutate(across(everything(), as.numeric))

#visualization removed

# Analysis 
split <- createDataPartition(gss_tbl$MOSTHRS,
                                       p = .25,
                                       list = T)$Resample1
test_tbl <- gss_tbl[split,]
train_tbl <- gss_tbl[-split,]

folds<-createFolds(train_tbl$MOSTHRS)

tic()
ols_reg<- train(MOSTHRS ~ .,
                train_tbl,
                method = "lm",
                na.action=na.pass,
                metric = "Rsquared",
                preProcess=c("center","scale","zv", "nzv","medianImpute"), 
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         verboseIter=T, 
                                         indexOut = folds)
)
ols_reg_tt<-toc()
ols_reg_time1<-(ols_reg_tt$toc-ols_reg_tt$tic)
ols_reg
cv_m1 <- ols_reg$results$Rsquared
holdout_m1 <- cor(
  predict(ols_reg, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS)^2

tic()
elastic_net<- train(
  MOSTHRS ~ .,
  train_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = folds)
)
elastic_net_tt<-toc() 
elastic_net_time1<-(elastic_net_tt$toc-elastic_net_tt$tic)
elastic_net
cv_m2 <- max(elastic_net$results$Rsquared)
holdout_m2 <- cor(
  predict(elastic_net, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS)^2

tic()
random_forest<- train(
  MOSTHRS ~ .,
  train_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = folds)
)
random_forest_tt<-toc() 
random_forest_time1<-(random_forest_tt$toc-random_forest_tt$tic)
random_forest
cv_m3 <- max(random_forest$results$Rsquared)
holdout_m3 <- cor(
  predict(random_forest, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS)^2

tic()
eXtreme<- train(
  MOSTHRS ~ .,
  train_tbl,
  method="xgbLinear",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = folds)
)
eXtreme_tt<-toc() 
eXtreme_time1<-(eXtreme_tt$toc-eXtreme_tt$tic)
eXtreme
cv_m4 <- max(eXtreme$results$Rsquared)
holdout_m4 <- cor(
  predict(eXtreme, test_tbl, na.action = na.pass),
  test_tbl$MOSTHRS
)^2

summary(resamples(list(ols_reg, elastic_net, random_forest, eXtreme)), metric="Rsquared")
dotplot(resamples(list(ols_reg, elastic_net, random_forest, eXtreme)), metric="Rsquared")

local_cluster <- makeCluster(15) 
registerDoParallel(local_cluster) 

tic()
ols_reg<- train(MOSTHRS ~ .,
                train_tbl,
                method = "lm",
                na.action=na.pass,
                metric = "Rsquared",
                preProcess=c("center","scale","zv", "nzv","medianImpute"), 
                trControl = trainControl(method="cv", 
                                         number=10, 
                                         verboseIter=T, 
                                         indexOut = folds)
)
ols_reg_tt_para<-toc() 
ols_reg_time2<-(ols_reg_tt_para$toc-ols_reg_tt_para$tic) 

tic()
elastic_net<- train(
  MOSTHRS ~ .,
  train_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = folds)
)
elastic_net_tt_para<-toc() 
elastic_net_time2<-(elastic_net_tt_para$toc-elastic_net_tt_para$tic)

tic()
random_forest<- train(
  MOSTHRS ~ .,
  train_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = folds)
)
random_forest_tt_para<-toc() 
random_forest_time2<-(random_forest_tt_para$toc-random_forest_tt_para$tic)

tic()
eXtreme<- train(
  MOSTHRS ~ .,
  train_tbl,
  method="xgbLinear",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = folds)
)
eXtreme_tt_para<-toc() 
eXtreme_time2<-(eXtreme_tt_para$toc-eXtreme_tt_para$tic)

stopCluster(local_cluster) 
registerDoSEQ()

# Publication
thanks_richard <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

table3_tbl <- tibble( #changed table 1 to 3
  algo = c("regression","elastic net","random forests","xgboost"),
  cv_rqs = c(
    thanks_richard(cv_m1),
    thanks_richard(cv_m2),
    thanks_richard(cv_m3),
    thanks_richard(cv_m4)
  ),
  ho_rqs = c(
    thanks_richard(holdout_m1),
    thanks_richard(holdout_m2),
    thanks_richard(holdout_m3),
    thanks_richard(holdout_m4)
  )
)

table4_tbl <- tibble(model= c("regression","elastic net","random forests","xgboost"), #changed table 2 to 4
                      supercomputer= as.numeric(c(ols_reg_time1,elastic_net_time1, random_forest_time1,eXtreme_time1)),
                      supercomputer_15=as.numeric(c(ols_reg_time2,elastic_net_time2,random_forest_time2,eXtreme_time2))
                     )

#writing csv files from tables
write_csv(table3_tbl, "table3.csv")
write_csv(table4_tbl, "table4.csv")
