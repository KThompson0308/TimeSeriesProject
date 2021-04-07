library('tswge')
library(dplyr)
library(tseries)
library(orcutt)
library(dplyr)
library(tidyverse)
library(purrr)
library(corrplot)
library(GGally)
library(ggplot2)
library(nnfor)
library(xts)
# devtools::install_github("bips-hb/neuralnet")


### loading the data
APPL = read.csv("Rfolder/AAPL.csv", header = TRUE)
DXY = read.csv("Rfolder/DX-Y.NYB.csv", header = TRUE)
CAC40 = read.csv("Rfolder/CAC40.csv", header = TRUE)
Gold = read.csv("Rfolder/GC=F.csv", header = TRUE)
Copper = read.csv("Rfolder/HG=F.csv", header = TRUE)
CQQQ = read.csv("Rfolder/CQQQ.csv", header = TRUE)
TNYR = read.csv("Rfolder/TNX.csv", header = TRUE)
WTI = read.csv("Rfolder/CL=F.csv", header = TRUE)
NASDAQ = read.csv("Rfolder/IXIC.csv", header = TRUE)

### APPL - Date & Adj.Close
head(APPL)
APPL = APPL %>% dplyr::select(Date, Adj.Close)
APPL$Date = as.Date(APPL$Date, "%m/%d/%Y")
summary(APPL)
### Dollar Index - Date & Adj.Close
head(DXY)
DXY = DXY %>% dplyr::select(Date, Adj.Close)
DXY$Adj.Close = as.numeric(DXY$Adj.Close)
DXY$Date = as.Date(DXY$Date, "%m/%d/%Y")
summary(DXY)
### FTSE (Financial Times Index at London) - Date & Adj.Close
head(CAC40)
CAC40 = CAC40 %>% dplyr::select(Date, Adj.Close)
CAC40$Adj.Close = as.numeric(CAC40$Adj.Close)
CAC40$Date = as.Date(CAC40$Date, "%m/%d/%Y")
summary(CAC40)
### Gold (commodity price) - Date & Adj.Close
head(Gold)
Gold = Gold %>% dplyr::select(Date, Adj.Close)
Gold$Adj.Close = as.numeric(Gold$Adj.Close)
Gold$Date = as.Date(Gold$Date, "%m/%d/%Y")
summary(Gold)
### Copper (commodity price) - Date & Adj.Close
head(Copper)
Copper = Copper %>% dplyr::select(Date, Adj.Close)
Copper$Adj.Close = as.numeric(Copper$Adj.Close)
Copper$Date = as.Date(Copper$Date, "%m/%d/%Y")
summary(Copper)
### CQQQ (Invesco China Technology ETF - Proxy of chinese tech stock)
head(CQQQ)
CQQQ = CQQQ %>% dplyr::select(Date, Adj.Close)
CQQQ$Adj.Close = as.numeric(CQQQ$Adj.Close)
CQQQ$Date = as.Date(CQQQ$Date, "%m/%d/%Y")
summary(CQQQ)
### 10 year Treasury note  
head(TNYR)
TNYR = TNYR %>% dplyr::select(Date, Adj.Close)
TNYR$Adj.Close = as.numeric(TNYR$Adj.Close)
TNYR$Date = as.Date(TNYR$Date, "%m/%d/%Y")
summary(TNYR)
### WTI Crude 
head(WTI)
WTI = WTI %>% dplyr::select(Date, Adj.Close)
WTI$Adj.Close = as.numeric(WTI$Adj.Close)
WTI$Date = as.Date(WTI$Date, "%m/%d/%Y")
summary(WTI)
### NASDAQ index
head(NASDAQ)
NASDAQ = NASDAQ %>% dplyr::select(Date, Adj.Close)
NASDAQ$Adj.Close = as.numeric(NASDAQ$Adj.Close)
NASDAQ$Date = as.Date(NASDAQ$Date, "%m/%d/%Y")
summary(NASDAQ)

### Creating data set - joining Apple, Nasdaq, wti, gold, copper
Appl.df = list(APPL,NASDAQ,WTI, Gold, Copper) %>% reduce(inner_join, by = 'Date')
names(Appl.df)[2] = 'APPL'
names(Appl.df)[3] = 'NASDAQ'
names(Appl.df)[4] = 'WTI'
names(Appl.df)[5] = 'Gold'
names(Appl.df)[6] = 'Copper'
summary(Appl.df)
head(Appl.df)
dim(Appl.df) #--- 1509, 6
### Creating dataset - joining TNYR and DXY
tnxy = inner_join(TNYR, DXY, by = 'Date')
names(tnxy)[2] = 'TNYR'
names(tnxy)[3] = 'DXY'
summary(tnxy)
dim(tnxy) # --- 1824, 3
### Creating final dataset
Appl.data = list(Appl.df, tnxy, CQQQ,CAC40) %>% reduce(inner_join, by = 'Date')
names(Appl.data)[9] = 'CQQQ'
names(Appl.data)[10] = 'CAC40'
summary(Appl.data)
dim(Appl.data) ## ---1494, 10

###--------------------Imputing NAs ------------------------------------###
## Logic: use the average of preceeding and following values of NAs
Appl.data$WTI = 0.5*(na.locf(Appl.data$WTI,fromlast=TRUE) + na.locf(Appl.data$WTI))
Appl.data$Gold = 0.5*(na.locf(Appl.data$Gold,fromlast=TRUE) + na.locf(Appl.data$Gold))
Appl.data$Copper = 0.5*(na.locf(Appl.data$Copper,fromlast=TRUE) + na.locf(Appl.data$Copper))
Appl.data$TNYR = 0.5*(na.locf(Appl.data$TNYR,fromlast=TRUE) + na.locf(Appl.data$TNYR))
Appl.data$DXY = 0.5*(na.locf(Appl.data$DXY,fromlast=TRUE) + na.locf(Appl.data$DXY))
summary(Appl.data)

### Exporting the file 
write.csv(Appl.data,"Rfolder/Appl.data.csv", row.names = FALSE)

### Corelation plots
# ggpair plot for correlation
Appl.data.corl%>%
  ggpairs(columns=c("APPL","NASDAQ","WTI","Gold", "Copper","TNYR","DXY","CQQQ","CAC40"), 
          upper = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na =
                         "na"),
          lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na =
                         "na"),
          diag = list(continuous = "densityDiag", discrete = "barDiag", na = "naDiag"))

# Corrplot 
#Examine the correlation between the continous predictors
Appl.data.corl <- Appl.data[,c(2,3,4, 5,6,7,8,9,10)]
pairs(Appl.data.corl)
my.cor<-cor(Appl.data.corl)
my.cor
pairs(Appl.data.corl,col=y)
corrplot(Appl.data.corl, tl.col = "black", order = "hclust", 
         hclust.method = "average", addrect = 4, tl.cex = 0.7)

### Checking the data stationary or not --- for stock price only
Appl.stock = Appl.data$APPL
plotts.sample.wge(Appl.stock)
# doing the log transformation of the data
Appl.stock.log = log(Appl.stock)
plotts.sample.wge(Appl.stock.log)
# taking out first business cycle (first 250 observations) to work with meaningful data
length(Appl.stock)
Appl.stock.Lst = Appl.stock.log ## it does not matter in the bigger scheme of thing
plotts.sample.wge(Appl.stock.Lst)

### ========================================================================###
### Decomposing the series , Original, log-trans, log-trans plus 1st Diff   ###

## Original series Appl.stock
Appl.stock.ts = ts(Appl.stock.Lst, freq = 5)
decomp1.appl.add = decompose (Appl.stock.ts, type = "multiplicative")
plot(decomp1.appl.add)

## Log transformed series
# Appl.log.ts = ts(Appl.stock.log.Lst, freq = 5)
# decomp2.appl.add = decompose (Appl.ts, type = "additive")
# plot(decomp2.appl.add)

## Log-transformed plus first difference series
# Appl.log.1.ts = ts(Appl.stock.log.Lst.1, freq = 5)
# decomp3.appl.add = decompose (Appl.ts, type = "additive")
# plot(decomp3.appl.add)


Appl.stock.1 = artrans.wge(Appl.stock.Lst, phi.tr = 1) ## linear not quardatic
plotts.sample.wge(Appl.stock.1)
acf(Appl.stock.1)
pacf(Appl.stock.1)
aic5.wge(Appl.stock.1, type = 'aic')

#### ========================================================================####
###             ------------Model Overfitting--------------------------------####
# appl.est.overfit = est.ar.wge(Appl.stock.log.Lst, p=5, type = 'burg')
appl.est.overfit = est.ar.wge(Appl.stock.1, p=7, type = 'burg')
## (1-0.9994B) is the dominent factor, which indicates first differencing

###============================= Model 1  ===============================#########
###-------------------------  ARIMA(1,1,0) --------------------------------
Appl.stock.est1 = est.arma.wge(Appl.stock.1, p=5, q=1) # burg and mle are same
Appl.stock.est1$phi
factor.wge(phi = c(-0.850236709,-0.055524504,-0.006590103,-0.025647299, 0.068208938))
Appl.stock.est1$theta
factor.wge(phi= c(-0.7585476))
Appl.stock.est1$avar # variance
Appl.stock.est1$aic  # model AIC

## Calculation ASE for last 7 days >>>> Short Term >>>>> ===========########

Appl.fore.lastn.7 = fore.aruma.wge(Appl.stock, phi = Appl.stock.est1$phi, d=1,
                         theta = Appl.stock.est1$theta, n.ahead = 7, limits = T, 
                         lastn = T)
Appl.fore.lastn.7$f
len = length(Appl.stock.log.Lst)
ASE_forecast1 = mean((Appl.fore.lastn.7$f - Appl.stock[(len-6):len])^2)
ASE_forecast1

## Rolling window ASE for model - first difference --ARIMA(1,1,0)
# Appl.stock.1
phis = Appl.stock.est1$phi
thetas = Appl.stock.est1$theta
trainingSize = 50
horizon = 7
ASEHolder.model1 = numeric()
for( i in 1:(100-(trainingSize + horizon) + 1))
{
  forecasts = fore.aruma.wge(Appl.stock[i:(i+(trainingSize-1))],
                             phi = phis, theta = thetas, d=1, n.ahead = horizon)
  
  ASE = mean((Appl.stock[(trainingSize+i):(trainingSize+ i + (horizon) - 1)]
              - forecasts$f)^2)
  
  ASEHolder.model1[i] = ASE
}

ASEHolder.model1
hist(ASEHolder.model1)
WindowedASE.model1 = mean(ASEHolder.model1)
summary(ASEHolder.model1)
WindowedASE.model1

## Calculation ASE for last 30 days >>>> Long Term >>>>> ===========########
Appl.fore.lastn.30 = fore.aruma.wge(Appl.stock, phi = Appl.stock.est1$phi,
                                    theta = Appl.stock.est1$theta, d=1,
                                   n.ahead = 30, limits = T, lastn = T)
Appl.fore.lastn.30$f
len = length(Appl.stock)
ASE_forecast1.2 = mean((Appl.fore.lastn.30$f - Appl.stock[(len-29):len])^2)
ASE_forecast1.2

## Rolling window ASE for model - log & first difference --ARIMA(1,1,0)
# Appl.stock.log.Lst
phis = Appl.stock.est1$phi
trainingSize = 50
horizon = 30
ASEHolder.model1.2 = numeric()
for( i in 1:(100-(trainingSize + horizon) + 1))
{
  forecasts = fore.aruma.wge(Appl.stock[i:(i+(trainingSize-1))],
                             phi = phis,d=1, n.ahead = horizon)
  
  ASE = mean((Appl.stock[(trainingSize+i):(trainingSize+ i + (horizon) - 1)]
              - forecasts$f)^2)
  
  ASEHolder.model1.2[i] = ASE
}

ASEHolder.model1.2
hist(ASEHolder.model1.2)
WindowedASE.model1.2 = mean(ASEHolder.model1.2)
summary(ASEHolder.model1.2)
WindowedASE.model1.2

####========= Model 1 is the top model , so running forecast on Model 1 =======#####
####           >>>>>>>> Short terms 7 days forecase >>>>>>>                  #######
Appl.fore.7days = fore.aruma.wge(Appl.stock, phi = Appl.stock.est1$phi,
                                  theta = Appl.stock.est1$theta,  d=1, n.ahead = 7,
                                   limits = T, lastn = F)
Appl.fore.7days$f

#plot next 7 days
plot(seq(1,100,1), Appl.stock[1395:1494], type = "l",xlim = c(0,115), 
     ylab = "Stock Price", main = "7 Days Forecast")
lines(seq(101,107,1), Appl.fore.7days$f, type = "l", col = "red")

####           >>>>>>>> Short terms 30 days forecase >>>>>>>                  #######
Appl.fore.30days = fore.aruma.wge(Appl.stock, phi = Appl.stock.est1$phi, d=1,
                                 n.ahead = 30, limits = T, lastn = F)
Appl.fore.30days$f

#plot next 30 days
plot(seq(1,100,1), Appl.stock[1395:1494], type = "l",xlim = c(0,140), 
     ylab = "Stock Price", main = "30 Days Forecast")
lines(seq(101,130,1), Appl.fore.30days$f, type = "l", col = "red")

####=============================== Model 2 ==================================####
####                               ARIMA(4,1,2)            -----------------------

# ## Estimating the model
# Appl.stock.est2 = est.arma.wge(Appl.stock.1, p=4, q=2) 
# Appl.stock.est2$phi
# factor.wge(phi = c(-1.853964323, -1.021855038, -0.050886530, -0.005807988))
# Appl.stock.est2$theta
# factor.wge(phi = c(-1.7762135, -0.8872197))
# Appl.stock.est2$avar # variance
# Appl.stock.est2$aic  # model AIC
# ### Calculation ASE for last 7 days >>>> Short Term >>> ===================###
# Appl.fore.lastn.7 = fore.aruma.wge(Appl.stock, phi = Appl.stock.est2$phi, 
#                                    theta = Appl.stock.est2$theta, d=1, 
#                                    n.ahead = 7, limits = T, lastn = T)
# Appl.fore.lastn.7$f
# len = length(Appl.stock.log.Lst)
# ASE_forecast2 = mean((Appl.fore.lastn.7$f - Appl.stock.log.Lst[(len-6):len])^2)
# ASE_forecast2
# 
# ## Rolling window ASE for model - log & first difference - ARIMA(4,1,2)
# # Appl.stock.log.Lst
# phis = Appl.stock.est2$phi
# thetas = Appl.stock.est2$theta
# trainingSize = 50
# horizon = 7
# ASEHolder.model2 = numeric()
# for( i in 1:(100-(trainingSize + horizon) + 1))
# {
#   forecasts = fore.aruma.wge(Appl.stock[i:(i+(trainingSize-1))],
#                              phi = phis, theta = thetas, d=1, n.ahead = horizon)
#   
#   ASE = mean((Appl.stock[(trainingSize+i):(trainingSize+ i + (horizon) - 1)]
#               - forecasts$f)^2)
#   
#   ASEHolder.model2[i] = ASE
# }
# 
# ASEHolder.model2
# hist(ASEHolder.model2)
# WindowedASE.model2 = mean(ASEHolder.model2)
# summary(ASEHolder.model2)
# WindowedASE.model2
# 
# ### Calculation ASE for last 30 days >>>> Long Term >>> ===================###
# Appl.fore.lastn.30 = fore.aruma.wge(Appl.stock, phi = Appl.stock.est2$phi, 
#                                    theta = Appl.stock.est2$theta, d=1, 
#                                    n.ahead = 30, limits = T, lastn = T)
# Appl.fore.lastn.30$f
# len = length(Appl.stock.log.Lst)
# ASE_forecast2.2 = mean((Appl.fore.lastn.30$f - Appl.stock[(len-29):len])^2)
# ASE_forecast2.2
# 
# ## Rolling window ASE for model - log & first difference - ARIMA(4,1,2)
# # Appl.stock.log.Lst
# phis = Appl.stock.est2$phi
# thetas = Appl.stock.est2$theta
# trainingSize = 50
# horizon = 30
# ASEHolder.model2.2 = numeric()
# for( i in 1:(100-(trainingSize + horizon) + 1))
# {
#   forecasts = fore.aruma.wge(Appl.stock[i:(i+(trainingSize-1))],
#                              phi = phis, theta = thetas, d=1, n.ahead = horizon)
#   
#   ASE = mean((Appl.stock[(trainingSize+i):(trainingSize+ i + (horizon) - 1)]
#               - forecasts$f)^2)
#   
#   ASEHolder.model2.2[i] = ASE
# }
# 
# ASEHolder.model2.2
# hist(ASEHolder.model2.2)
# WindowedASE.model2.2 = mean(ASEHolder.model2.2)
# summary(ASEHolder.model2.2)
# WindowedASE.model2.2

### ============= Strategy and Plans for the rest of the analysis ============#####
### --------------------------------------------------------------------------#####
# 1. try with noise-plus model
# 2. check the residuals are whitened
#     a. Visual
#     b. Ljung-box test
# 3. Use the multiple regression (multivariate analysis) to identify the model
# 4. Use the var model (multivariate)
# 5. Use the neural network model (multivariate)

###======================  Signal plus noise model =========================###
##--------------------------------------------------------------------------###
plotts.sample.wge(Appl.stock)
# creating deterministic model
n = length(Appl.stock) ## 1494
t = c(1:n)
plotts.sample.wge(t)
det.appl.fit = lm(Appl.stock~t)
summary(det.appl.fit)
det.r = Appl.stock - det.appl.fit$coefficients[1] - det.appl.fit$coefficients[2]*t ## residuals

est.det.r = aic.wge(det.r, p=0:6, q=0:0) ## p=2
est.det.r
det.y.trans = artrans.wge(Appl.stock,phi.tr = est.det.r$phi)
det.t.trans = artrans.wge(t, phi.tr = est.det.r$phi)
det.appl.tran.fit = lm(det.y.trans ~ det.t.trans)
summary(det.appl.tran.fit)
plotts.sample.wge(det.appl.tran.fit$residuals, arlimits = T)
## checking the residuals are whitened 
acf(det.appl.tran.fit$residuals)
# Ljung-box test
ljung.wge(det.appl.tran.fit$residuals, K = 24) ## $pval: 3.895293e-08
ljung.wge(det.appl.tran.fit$residuals, K = 48) ## $pval: 2.460254e-13
## model for Zt (residual)
est.det.r$phi
factor.wge(phi = est.det.r$phi)
est.det.r$vara
det.appl.fit$coefficients[1]
det.appl.fit$coefficients[2]
## Model Equation: Xt = 6.5851 + 0.0584*t + Zt ; Variance = 1.6785
## (1-0.9976B)(1+0.1121B)Zt = at
## Verifying the model with generated realization
gen.sigplusnoise.wge(1494, b0 = 6.5851, b1 = 0.0584, phi = est.det.r$phi, vara = est.det.r$vara )
plotts.sample.wge(Appl.stock)
plot.ts(Appl.stock)

## forecasting based on signal plus model
## forecast for next 7 days
det.appl.fcst.7 = fore.sigplusnoise.wge(Appl.stock, max.p = 2, n.ahead = 7, limits = F)
det.appl.fcst.7$f
#plot next 7 days
plot(seq(1,100,1), Appl.stock[1395:1494], type = "l",xlim = c(0,115), 
     ylab = "Stock Price", main = "7 Days Forecast")
lines(seq(101,107,1), det.appl.fcst.7$f, type = "l", col = "red")

## forecast for next 30 days
det.appl.fcst.30 = fore.sigplusnoise.wge(Appl.stock, max.p = 2, n.ahead = 30, limits = F)
det.appl.fcst.30$f
#plot next 30 days
plot(seq(1,100,1), Appl.stock[1395:1494], type = "l",xlim = c(0,140), 
     ylab = "Stock Price", main = "30 Days Forecast")
lines(seq(101,130,1), det.appl.fcst.30$f, type = "l", col = "red")

####------------------------------------------------------------------------###
####                      Neural Network                                    ###
####========================================================================###

Appl.data
str(Appl.data) ## length: 1494
APPL  = ts(Appl.data$APPL)
## Univariate model for Apple Stock ==================
## forecasting for 7 days
appl.uni.trn.7 = ts(APPL[1:1488]) ## train data
appl.uni.tst.7 = ts(APPL[1488:1494]) # test data

appl.uni.fit.mlp = mlp(appl.uni.trn.7,reps = 50,comb = "mean")
appl.uni.fit.mlp
plot(appl.uni.fit.mlp)

appl.uni.fore.mlp = forecast(appl.uni.fit.mlp, h = 7)
plot(seq(1,100,1),APPL[1389:1488], type = 'l', xlim = c(1,115))
lines(seq(101,107,1), appl.uni.fore.mlp$mean, col = 'blue')
## ASE for 7 days
ASE.appl.7 = mean((APPL[1488:1494] - appl.uni.fore.mlp$mean)^2)
ASE.appl.7

## forecasting for 30 days
appl.uni.trn.30 = ts(APPL[1:1464]) ## train data
appl.uni.tst.30 = ts(APPL[1465:1494]) # test data

appl.uni.fit.mlp.2 = mlp(appl.uni.trn.30,reps = 50,comb = "mean")
appl.uni.fit.mlp.2
plot(appl.uni.fit.mlp.2)

appl.uni.fore.mlp.2 = forecast(appl.uni.fit.mlp.2, h = 30)
plot(seq(1,100,1),APPL[1365:1464], type = 'l', xlim = c(1,115))
lines(seq(101,130,1), appl.uni.fore.mlp.2$mean, col = 'blue')
## ASE for 30 days
ASE.appl.30 = mean((APPL[1465:1494] - appl.uni.fore.mlp.2$mean)^2)
ASE.appl.30

## Multivariate model =========================================
NASDAQ = ts(Appl.data$NASDAQ, frequency = 5)
WTI  = ts(Appl.data$WTI)
Gold = ts(Appl.data$Gold)
Copper = ts(Appl.data$Copper)
TNYR = ts(Appl.data$TNYR, frequency = 5)
DXY = ts(Appl.data$DXY)
CQQQ = ts(Appl.data$CQQQ)
CAC40 = ts(Appl.data$CAC40)

## 7 days ASE
APPL.1487 = ts(APPL[1:1487])
APPL.dfx = data.frame(NASDAQ, TNYR, DXY)
appl.multi.fit.mlp.1 = mlp(APPL.1487, xreg = APPL.dfx, hd = 5, reps = 25,comb = "mean" )
plot(appl.multi.fit.mlp.1)
appl.multi.fit.mlp.1.f = forecast(appl.multi.fit.mlp.1, h=7, xreg = APPL.dfx)
plot(appl.multi.fit.mlp.1.f)
plot(seq(1,100,1),APPL[1389:1488], type = 'l', xlim = c(1,115))
lines(seq(101,106,1), appl.multi.fit.mlp.1.f$mean, col = 'blue')
ASE.multi.1 = mean((APPL[1489:1494] - appl.multi.fit.mlp.1.f$mean)^2)
ASE.multi.1

## 30 days ASE
APPL.1464 = ts(APPL[1:1464])
APPL.dfx = data.frame(NASDAQ, WTI, Gold, Copper, TNYR, DXY, CQQQ, CAC40)
appl.multi.fit.mlp.2 = mlp(APPL.1464, xreg = APPL.dfx, hd = 5, reps = 25,comb = "mean" )
plot(appl.multi.fit.mlp.2)
appl.multi.fit.mlp.2.f = forecast(appl.multi.fit.mlp.2, h=30, xreg = APPL.dfx)
plot(appl.multi.fit.mlp.2.f)
plot(seq(1,100,1),APPL[1365:1464], type = 'l', xlim = c(1,115))
lines(seq(101,107,1), appl.multi.fit.mlp.1.f$mean, col = 'blue')
ASE.multi.1 = mean((APPL[1465:1494] - appl.multi.fit.mlp.1.f$mean)^2)
ASE.multi.1

### 7 days forecast ========================================
# NASDAQ - 7days forecast
fit.mlp.NASDAQ= mlp(NASDAQ,reps = 50,comb = "mean")
fit.mlp.NASDAQ
plot(fit.mlp.NASDAQ)
fore.mlp.NASDAQ = forecast(fit.mlp.NASDAQ, h=7)
plot(fore.mlp.NASDAQ)
tp.NASDAQ=data.frame(fore.mlp.NASDAQ)
ts.tp.NASDAQ = ts(tp.NASDAQ$Point.Forecast)
ts1.NASDAQ<-as.xts(NASDAQ)
ts2.NASDAQ<-as.xts(ts.tp.NASDAQ)
str(ts3.NASDAQ <- c(ts1.NASDAQ, ts2.NASDAQ))
NASDAQ.1501 = ts3.NASDAQ

# WTI - 7days forecast
fit.mlp.WTI= mlp(WTI,reps = 50,comb = "mean")
fit.mlp.WTI
plot(fit.mlp.WTI)
fore.mlp.WTI = forecast(fit.mlp.WTI, h=7)
plot(fore.mlp.WTI)
tp.WTI =data.frame(fore.mlp.WTI)
ts.tp.WTI = ts(tp.WTI$Point.Forecast)
ts1.WTI<-as.xts(WTI)
ts2.WTI<-as.xts(ts.tp.WTI)
str(ts3.WTI <- c(ts1.WTI, ts2.WTI))
WTI.1501 = ts3.WTI

# Gold - 7days forecast
fit.mlp.Gold = mlp(Gold,reps = 50,comb = "mean")
fit.mlp.Gold
plot(fit.mlp.Gold)
fore.mlp.Gold = forecast(fit.mlp.Gold, h=7)
plot(fore.mlp.Gold)
tp.Gold =data.frame(fore.mlp.Gold)
ts.tp.Gold = ts(tp.Gold$Point.Forecast)
ts1.Gold<-as.xts(Gold)
ts2.Gold<-as.xts(ts.tp.Gold)
str(ts3.Gold <- c(ts1.Gold, ts2.Gold))
Gold.1501 = ts3.Gold

# Copper - 7days forecast
fit.mlp.Copper = mlp(Copper,reps = 50,comb = "mean")
fit.mlp.Copper
plot(fit.mlp.Copper)
fore.mlp.Copper = forecast(fit.mlp.Copper, h=7)
plot(fore.mlp.Copper)
tp.Copper =data.frame(fore.mlp.Copper)
ts.tp.Copper = ts(tp.Copper$Point.Forecast)
ts1.Copper<-as.xts(Copper)
ts2.Copper<-as.xts(ts.tp.Copper)
str(ts3.Copper <- c(ts1.Copper, ts2.Copper))
Copper.1501 = ts3.Copper

# TNYR - 7days forecast
fit.mlp.TNYR = mlp(TNYR,reps = 50,comb = "mean")
fit.mlp.TNYR
plot(fit.mlp.TNYR)
fore.mlp.TNYR = forecast(fit.mlp.TNYR, h=7)
plot(fore.mlp.TNYR)
tp.TNYR =data.frame(fore.mlp.TNYR)
ts.tp.TNYR = ts(tp.TNYR$Point.Forecast)
ts1.TNYR<-as.xts(TNYR)
ts2.TNYR<-as.xts(ts.tp.TNYR)
str(ts3.TNYR <- c(ts1.TNYR, ts2.TNYR))
TNYR.1501 = ts3.TNYR

# DXY - 7days forecast
fit.mlp.DXY = mlp(DXY,reps = 50,comb = "mean")
fit.mlp.DXY
plot(fit.mlp.DXY)
fore.mlp.DXY = forecast(fit.mlp.DXY, h=7)
plot(fore.mlp.DXY)
tp.DXY =data.frame(fore.mlp.DXY)
ts.tp.DXY = ts(tp.DXY$Point.Forecast)
ts1.DXY<-as.xts(DXY)
ts2.DXY<-as.xts(ts.tp.DXY)
str(ts3.DXY <- c(ts1.DXY, ts2.DXY))
DXY.1501 = ts3.DXY

# CQQQ - 7days forecast
fit.mlp.CQQQ = mlp(CQQQ,reps = 50,comb = "mean")
fit.mlp.CQQQ
plot(fit.mlp.CQQQ)
fore.mlp.CQQQ = forecast(fit.mlp.CQQQ, h=7)
plot(fore.mlp.CQQQ)
tp.CQQQ =data.frame(fore.mlp.CQQQ)
ts.tp.CQQQ = ts(tp.CQQQ$Point.Forecast)
ts1.CQQQ<-as.xts(CQQQ)
ts2.CQQQ<-as.xts(ts.tp.CQQQ)
str(ts3.CQQQ <- c(ts1.CQQQ, ts2.CQQQ))
CQQQ.1501 = ts3.CQQQ

# CAC40 - 7days forecast
fit.mlp.CAC40 = mlp(CAC40,reps = 50,comb = "mean")
fit.mlp.CAC40
plot(fit.mlp.CAC40)
fore.mlp.CAC40 = forecast(fit.mlp.CAC40, h=7)
plot(fore.mlp.CAC40)
tp.CAC40 =data.frame(fore.mlp.CAC40)
ts.tp.CAC40 = ts(tp.CAC40$Point.Forecast)
ts1.CAC40<-as.xts(CAC40)
ts2.CAC40<-as.xts(ts.tp.CAC40)
str(ts3.CAC40 <- c(ts1.CAC40, ts2.CAC40))
CAC40.1501 = ts3.CAC40

## Creating data frame with 7 days forecasted values
Appl.dfx.1501 = data.frame(cbind(NASDAQ.1501, WTI.1501,Gold.1501,Copper.1501,
                           TNYR.1501, DXY.1501, CQQQ.1501, CAC40.1501))

cmort.mlp.518 = mlp(cmort, xreg = cmort.dfx.518)
cmort.f.518 = forecast(cmort.mlp.518, h=10, xreg = cmort.dfx.518)
plot(cmort.f.518)
plot(seq(1,100,1), cmort[409:508], type = "l",xlim = c(0,115), 
     main = "10 Days Forecast")
lines(seq(101,110,1), cmort.f.518$mean, type = "l", col = "red")





### 30 days forecast ========================================
# NASDAQ - 30days forecast
fore.mlp.NASDAQ.30 = forecast(fit.mlp.NASDAQ, h=30)
plot(fore.mlp.NASDAQ.30)
tp.NASDAQ.30=data.frame(fore.mlp.NASDAQ.30)
ts.tp.NASDAQ.30 = ts(tp.NASDAQ.30$Point.Forecast)
ts1.NASDAQ.30<-as.xts(NASDAQ)
ts2.NASDAQ.30<-as.xts(ts.tp.NASDAQ.30)
str(ts3.NASDAQ.30 <- c(ts1.NASDAQ.30, ts2.NASDAQ.30))
NASDAQ.1524 = ts(ts3.NASDAQ.30)

# WTI - 30 days forecast
fore.mlp.WTI.30 = forecast(fit.mlp.WTI, h=30)
plot(fore.mlp.WTI.30)
tp.WTI.30 =data.frame(fore.mlp.WTI.30)
ts.tp.WTI.30 = ts(tp.WTI.30$Point.Forecast)
ts1.WTI.30<-as.xts(WTI)
ts2.WTI.30<-as.xts(ts1.WTI.30)
str(ts3.WTI.30 <- c(ts1.WTI.30, ts2.WTI.30))
WTI.1524 = ts(ts3.WTI.30)

# Gold - 30 days forecast
fore.mlp.Gold.30 = forecast(fit.mlp.Gold, h=30)
plot(fore.mlp.Gold.30)
tp.Gold.30 =data.frame(fore.mlp.Gold.30)
ts.tp.Gold.30 = ts(tp.Gold.30$Point.Forecast)
ts1.Gold.30<-as.xts(Gold)
ts2.Gold.30<-as.xts(ts.tp.Gold.30)
str(ts3.Gold.30 <- c(ts1.Gold.30, ts2.Gold.30))
Gold.1524 = ts(ts3.Gold.30)

# Copper - 30days forecast
fore.mlp.Copper.30 = forecast(fit.mlp.Copper, h=30)
plot(fore.mlp.Copper.30)
tp.Copper.30 =data.frame(fore.mlp.Copper.30)
ts.tp.Copper.30 = ts(tp.Copper.30$Point.Forecast)
ts1.Copper.30<-as.xts(Copper)
ts2.Copper.30<-as.xts(ts.tp.Copper.30)
str(ts3.Copper.30 <- c(ts1.Copper.30, ts2.Copper.30))
Copper.1524 = ts(ts3.Copper.30)

# TNYR - 30 days forecast
fore.mlp.TNYR.30 = forecast(fit.mlp.TNYR, h=30)
plot(fore.mlp.TNYR.30)
tp.TNYR.30 =data.frame(fore.mlp.TNYR.30)
ts.tp.TNYR.30 = ts(tp.TNYR.30$Point.Forecast)
ts1.TNYR.30<-as.xts(TNYR)
ts2.TNYR.30<-as.xts(ts.tp.TNYR.30)
str(ts3.TNYR.30 <- c(ts1.TNYR.30, ts2.TNYR.30))
TNYR.1524 = ts(ts3.TNYR.30)

# DXY - 30days forecast
fore.mlp.DXY.30 = forecast(fit.mlp.DXY, h=7)
plot(fore.mlp.DXY.30)
tp.DXY.30 =data.frame(fore.mlp.DXY.30)
ts.tp.DXY.30 = ts(tp.DXY.30$Point.Forecast)
ts1.DXY.30<-as.xts(DXY)
ts2.DXY.30<-as.xts(ts.tp.DXY.30)
str(ts3.DXY.30 <- c(ts1.DXY.30, ts2.DXY.30))
DXY.1524 = ts(ts3.DXY.30)

# CQQQ - 30days forecast
fore.mlp.CQQQ.30 = forecast(fit.mlp.CQQQ, h=30)
plot(fore.mlp.CQQQ.30)
tp.CQQQ.30 =data.frame(fore.mlp.CQQQ.30)
ts.tp.CQQQ.30 = ts(tp.CQQQ.30$Point.Forecast)
ts1.CQQQ.30<-as.xts(CQQQ)
ts2.CQQQ.30<-as.xts(ts1.CQQQ.30)
str(ts3.CQQQ.30 <- c(ts1.CQQQ.30, ts1.CQQQ.30))
CQQQ.1524 = ts(ts3.CQQQ.30)

# CAC40 - 30days forecast
fore.mlp.CAC40.30 = forecast(fit.mlp.CAC40, h=30)
plot(fore.mlp.CAC40.30)
tp.CAC40.30 =data.frame(fore.mlp.CAC40.30)
ts.tp.CAC40.30 = ts(tp.CAC40.30$Point.Forecast)
ts1.CAC40.30<-as.xts(CAC40)
ts2.CAC40.30<-as.xts(ts.tp.CAC40.30)
str(ts3.CAC40.30 <- c(ts1.CAC40.30, ts2.CAC40.30))
CAC40.1524 = ts(ts3.CAC40.30)
