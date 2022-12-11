setwd("F:/OneDrive/Doctorado/Docencia/UV/Ejemplos")

library(tidyverse)
library(readxl)
#library(ggplot2)
library(readr)
library(rpart)
library(rpart.plot)
library(partykit) #ctree
library(Metrics)#mape, mae...
library(caret)
library(verification)
#library(pROC)

df <-  read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"), header = FALSE)%>%as.data.frame

names(df) <- c(list("ID", "Cancer"),LETTERS[1:26],letters[1:4])

names(df) <- make.names(names(df)) 
head(df) 



 

#df2<-df%>%select(EDAD, TENURE, P_1, P_2, P_3, P_4, P_5)
df2<-df%>%dplyr::select(-ID)

df2[sapply(df2, is.character)] <- lapply(df2[sapply(df2, is.character)], 
                                           as.factor)

str(df2)

summary(df2)

dim(df2)
#Tree####



## 75% of the sample size
smp_size <- floor(0.75 * nrow(df2))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(df2)), size = smp_size)

train_df2 <- df2[train_ind, ]
test_df2 <- df2[-train_ind, ]


tree_model_all_ctree <- ctree(Cancer~., data=train_df2)
tree_model_all_ctree
summary(tree_model_all_ctree)

plot(tree_model_all_ctree)
plot(tree_model_all_ctree , type="simple")

#validation test
tree_predict_test <- predict(tree_model_all_ctree, test_df2)
tree_predict_prob_test <- predict(tree_model_all_ctree, test_df2, type = "prob")%>%as.data.frame%>%.$M



accuracy(as.logical(test_df2$Cancer=="M")%>%as.numeric, as.logical(tree_predict_test%>% as.vector()%>%as.factor=="M")%>%as.numeric)
recall(as.logical(test_df2$Cancer=="M")%>%as.factor, as.logical(tree_predict_test%>% as.vector()%>%as.factor=="M")%>%as.factor)
precision(as.logical(test_df2$Cancer=="M")%>%as.factor, as.logical(tree_predict_test%>% as.vector()%>%as.factor=="M")%>%as.factor)
auc(as.logical(test_df2$Cancer=="M")%>%as.numeric, as.logical(tree_predict_test%>% as.vector()%>%as.factor=="M")%>%as.numeric)

confusionMatrix(test_df2$Cancer, tree_predict_test, positive="M")

roc.plot(as.logical(test_df2$Cancer=="M")%>%as.numeric, tree_predict_prob_test)


