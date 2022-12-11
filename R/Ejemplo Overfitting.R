
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base", 
                      "package:RevoUtilsMath", "package:RevoUtils", "package:RevoIOQ", "package:MicrosoftR",  
                      "package:tools", "package:RUnit" )
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

#Librerias para distintos contrastes del modelo

library(magrittr)
library(dplyr)
library(readxl)
library(data.table)
library(magrittr)
library(R.utils)
library(tidyr)

library(rpart)
library(rpart.plot)

#creacci?n del los datos del modelo
df <- data.frame(x1 = rnorm(10000))
df$y <- 3*df$x1 +  rnorm(10000)+6

str(df)

summary(df)



#LM (minimos cuadrados)

model_lm <- lm(df, formula= y~.)
summary(model_lm)

predict_lm <- data.frame (X1=df$x1, pred=predict(data=df, model_lm)) %>%arrange(X1)

model_rt <- rpart(df, formula= y~., control = rpart.control(cp = 0.05))

rpart.plot(model_rt)

predict_rt <- data.frame (X1=df$x1, pred=predict(data=df, model_rt)) %>%arrange(X1)

model_rt_2 <- rpart(df, formula= y~., control = rpart.control(cp = 0.01))

rpart.plot(model_rt_2)

predict_rt_2 <- data.frame (X1=df$x1, pred=predict(data=df, model_rt_2)) %>%arrange(X1)

model_rt_3 <- rpart(df, formula= y~., control = rpart.control(cp = 0.0001))

rpart.plot(model_rt_3)

predict_rt_3 <- data.frame (X1=df$x1, pred=predict(data=df, model_rt_3)) %>%arrange(X1)

plot(df$x1[1:nrow(df)],df$y[1:nrow(df)], col="black" )
lines(predict_lm [1:nrow(df),1:2], col="red")
lines(predict_rt [1:nrow(df),1:2], col="blue")
lines(predict_rt_2 [1:nrow(df),1:2], col="orange")
lines(predict_rt_3 [1:nrow(df),1:2], col="pink")





