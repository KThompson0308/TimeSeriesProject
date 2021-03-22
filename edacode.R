library(tswge)
library(dplyr)


#Import data and take relevant columns
apple = read.csv("apple.csv")
closing = apple %>% select(c(date, Adj.Close))

#Plot the data for raw data and first difference data
plotts.sample.wge(closing$Adj.Close)
d1 = artrans.wge(closing$Adj.Close)

# AIC of Differenced Data (I got p = 4, q = 2 and then p=5 and q=2)
aic5.wge(d1)

# estimate phis and thetas on differenced data
fourtwo_estimate = est.arma.wge(d1, p=4, q=2)
fivetwo_estimate = est.arma.wge(d1, p=5, q=2)

# Variance estimates for both models
fourtwo_var = fourtwo_estimate$avar
fivetwo_var = fivetwo_estimate$avar

# forecasts 
f_fourtwo_long = fore.aruma.wge(closing$Adj.Close, phi=fourtwo_estimate$phi,
                                theta = four_two_estimate$theta, d=1)
f_fivetwo_long = fore.aruma.wge(closing$Adj.Close, phi=fivetwo_estimate$phi,
                                theta = fivetwo_estimate$theta, d=1)
