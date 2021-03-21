library('tswge')
library(dplyr)
library(tseries)
library(orcutt)
library(dplyr)
library(tidyverse)
library(purrr)

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
APPL = APPL %>% select(Date, Adj.Close)
APPL$Date = as.Date(APPL$Date, "%m/%d/%Y")
summary(APPL)
### Dollar Index - Date & Adj.Close
head(DXY)
DXY = DXY %>% select(Date, Adj.Close)
DXY$Adj.Close = as.numeric(DXY$Adj.Close)
DXY$Date = as.Date(DXY$Date, "%m/%d/%Y")
summary(DXY)
### FTSE (Financial Times Index at London) - Date & Adj.Close
head(CAC40)
CAC40 = CAC40 %>% select(Date, Adj.Close)
CAC40$Adj.Close = as.numeric(CAC40$Adj.Close)
CAC40$Date = as.Date(CAC40$Date, "%m/%d/%Y")
summary(CAC40)
### Gold (commodity price) - Date & Adj.Close
head(Gold)
Gold = Gold %>% select(Date, Adj.Close)
Gold$Adj.Close = as.numeric(Gold$Adj.Close)
Gold$Date = as.Date(Gold$Date, "%m/%d/%Y")
summary(Gold)
### Copper (commodity price) - Date & Adj.Close
head(Copper)
Copper = Copper %>% select(Date, Adj.Close)
Copper$Adj.Close = as.numeric(Copper$Adj.Close)
Copper$Date = as.Date(Copper$Date, "%m/%d/%Y")
summary(Copper)
### CQQQ (Invesco China Technology ETF - Proxy of chinese tech stock)
head(CQQQ)
CQQQ = CQQQ %>% select(Date, Adj.Close)
CQQQ$Adj.Close = as.numeric(CQQQ$Adj.Close)
CQQQ$Date = as.Date(CQQQ$Date, "%m/%d/%Y")
summary(CQQQ)
### 10 year Treasury note  
head(TNYR)
TNYR = TNYR %>% select(Date, Adj.Close)
TNYR$Adj.Close = as.numeric(TNYR$Adj.Close)
TNYR$Date = as.Date(TNYR$Date, "%m/%d/%Y")
summary(TNYR)
### WTI Crude 
head(WTI)
WTI = WTI %>% select(Date, Adj.Close)
WTI$Adj.Close = as.numeric(WTI$Adj.Close)
WTI$Date = as.Date(WTI$Date, "%m/%d/%Y")
summary(WTI)
### NASDAQ index
head(NASDAQ)
NASDAQ = NASDAQ %>% select(Date, Adj.Close)
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

### Exporting the file 
write.csv(Appl.data,"Rfolder/Appl.data.csv", row.names = FALSE)







