
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
library(lmtest)
library(gvlma)
library(MASS)
library(car)
library(tseries) #JB
library(sfsmisc) # test wald para RLM
library(corrgram)
library(DescTools)
library(dplyr)
library(readxl)
library(data.table)
library(magrittr)
library(R.utils)
library(tidyr)
library(dtplyr)
library(corrgram)
library(olsrr)
library(robustbase)


#creacci?n del los datos del modelo
df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
df$y <- 3*df$x1 + 2*df$x2 + rnorm(100)+6

str(df)

summary(df)

#creacci?n lags 
LAG1<-lag(df$x1, 1)

#Correlaciones
corrgram(df, order=NULL, lower.panel=NULL, upper.panel=panel.cor, diag.panel=panel.density)
cor(df) ->correlo
corrgram(df, order=NULL, lower.panel=corrgram::panel.ellipse, upper.panel=panel.cor, diag.panel=panel.density)



#LM (minimos cuadrados)

model <- lm(df, formula= y~.)
summary(model)

###Asunciones LM

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))


model_tests<-gvlma(model) #kurtosis y Skewness 

summary(model_tests)
plot(model_tests)

mean(model$residuals) # la media de los residos es cero

#MAPE
MAPE(model)
RMSE(model)

MAPE=mean(abs(df$y-predict(model))/df$y)*100
MAPE
#https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/Measures%20of%20Accuracy

#Autocorrelación
dwtest(model) #H0: ausencia de correlación

##Normalidad
sresid <- studres(model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals",  breaks = 15)
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) # es normal!!
boxplot(residuals(model), main = "Boxplot of Residuals")
#qqplot
qqPlot(model, main="QQ Plot")
qqnorm(residuals(model),main = "Q-Q Plot")
qqline(residuals(model))
#JB
jarque.bera.test(residuals(model)) #H0: es normal!!
#outliers
leveragePlots(model)
influencePlot(model,	id.method="identify")



##Homoecedasticiadad

ncvTest(model) #H0: homocedasticidad
# White
white.test(model)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(model) 

##Multicolinearedad 
vif(model)# solo una variable indep..
sqrt(vif(model)) #Si el factor de inflación de la varianza de una variable predictora fuera 5.27 (sqrt 5.27 = 2.3) esto significa que el error estándar para el coeficiente de esa variable predictora es 2.3 veces mayor que si esa variable predictora no estuviera correlacionada con las otras variables predictoras


# Evaluate Nonlinearity
# component + residual plot 
crPlots(model)
lmtest::reset(model, power=2, type=c("fitted"), data=DF) #Ramseys Test
#MAPE
#https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/Measures%20of%20Accuracy


model_robusto <- rlm(formula= y~x1+x2,data= df)
summary (model_robusto)

par(mfrow=c(2,2))
plot(model_robusto)
par(mfrow=c(1,1))



model_robusto <- lmrob(formula= y~x1+x2,data= df, method="SMDM",setting = "KS2014")
summary(model_robusto)