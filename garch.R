#garch
#Generalised Autoregressive Conditional Heteroskedasticity

# https://www.quantstart.com/articles/Generalised-Autoregressive-Conditional-Heteroskedasticity-GARCH-p-q-Models-for-Time-Series-Analysis

# white noise and random walk
# https://www.quantstart.com/articles/White-Noise-and-Random-Walks-in-Time-Series-Analysis 

# ARMA(autoreggresive moving average)
# https://www.quantstart.com/articles/Autoregressive-Moving-Average-ARMA-p-q-Models-for-Time-Series-Analysis-Part-1

#corelogram

?acf   # Auto- and Cross- Covariance and -Correlation Function Estimation  #pacf() , ccf() 

#lag - liczba krokow czasowych do pop[rzedniego elementu
# blue lines - p-value for 5% confidence with H0: correlation = 0


# trend
obj <- acf(seq(1,100))

#residues
plot(obj$acf)

#repeated sequence
acf(rep(1:10, 10))


set.seed(1)
acf(rnorm(100))

x<-rnorm(1000)
acf(x)   # plot 
#wystaje k = 6, 15, 18
3/30
Box.test(acf(x)$acf, lag = 30, type = "Ljung-Box")


# random walk

set.seed(4)
x<- w <- rnorm(1000)
for(t in 2:1000) x[t] <- x[t -1] + w[t]
plot(x, type = "l")

acf(x)

#fitting random walk # from def series of the differences should have a series that resembles discrete white noise

acf(diff(x))


####
# AR(1) with a1=0.6
set.seed(1)
x <- w <- rnorm(100)
for(t in 2:100) x[t] <- 0.6*x[t -1] + w[t]
# for from order p + 1
layout(1:2)
plot(x, type = "l")
acf(x)

# we can fit autoregressive model with ar() function based on AIC (Akaike Information Criterion)  # the lower the better 
?ar

x_ar <- ar(x, method = "mle") #maximum likelihood estimation
x_ar$order
x_ar$aic
x_ar$ar    # coefficient
x_ar$asy.var.coef   #  wariancja coef


# AR(2) with a1=0.6, a2 = -0.3
set.seed(1)
x <- w <- rnorm(100)
for(t in 3:100) x[t] <- 0.6*x[t -1] - 0.3*x[t-2] + w[t]
layout(1:2)
plot(x, type = "l")
acf(x)

x_ar <- ar(x, method = "mle") #maximum likelihood estimation
x_ar$order
x_ar$aic
x_ar$ar    # coefficient
x_ar$asy.var.coef   #  wariancja of coef


# MA(1) with b1=0.6
set.seed(1)
x <- w <- rnorm(100)
for(t in 2:100) x[t] <- 0.6*w[t -1] + w[t]
# for from order p + 1
layout(1:2)
plot(x, type = "l")
acf(x)   # powyzej k = q nie powinno byc znaczacej korelacji

?arima  # order (autoregressive, integrated components, moving average)

# The major difference between arima and ar is that arima estimates an intercept term because it does not subtract the mean value 
# of the series. Hence we need to be careful when carrying out predictions using the arima command. 

x_ma <- arima(x, c(0,0,1))
x_ma

arima(x, c(0,0,2))  # widac ze 2-gi parametr to zero


x_ma$coef
x_ma$var.coef
sqrt(x_ma$var.coef[1,1])
x_ma$residuals
x_ma$aic   # rownowanzne z AIC(x_ma)

# MA(3) with b1=0.6, b2 = 0.4, b3 = 0.2
set.seed(3)
x <- w <- rnorm(1000)
for(t in 4:1000) x[t] <- w[t] + 0.6*w[t-1] + 0.4*w[t-2] + 0.3*w[t-3]
# for from order p + 1
layout(1:2)
plot(x, type = "l")
acf(x)

x_ma <- arima(x, c(0,0,3))
x_ma


#ARMA(1,1), a1 = 0.5, b1 = -0.5
set.seed(1)
x <- arima.sim(n=1000, model=list(ar=0.5, ma=-0.5))
plot(x)
acf(x)

x_arma <- arima(x, c(1,0,1))
x_arma

set.seed(1)
x <- arima.sim(n=1000, model=list(ar=c(0.5, -0.25), ma=c(0.5, -0.3)))
plot(x)
acf(x)

x_arma <- arima(x, c(2,0,2))   # b1 and b2 do not match coefficients
x_arma

###### testing a good model based on AIC
set.seed(3)
x <- arima.sim(n=1000, model=list(ar=c(0.5, -0.25, 0.4), ma=c(0.5, -0.3)))

final_aic <- Inf
final_order <- c(0,0,0)
for (i in 0:4) for (j in 0:4) {
  print(i,j)
     current_aic <- AIC(arima(x, order=c(i, 0, j)))
   if (current_aic < final_aic) {
     final_aic <- current_aic
     final_order <- c(i, 0, j)
     final_arma <- arima(x, order=final_order)
   }
}

#komunikat o warningu
# test <- arima(x, order=c(4, 0, 3))
# #zwraca FASLE gdy cos nie tak
# arimaFit = tryCatch( arima(x, order=c(4, 0, 3)),
#                      error=function( err ) FALSE,
#                      warning=function( err ) FALSE )
# arimaFit


final_aic
final_order
final_arma

acf(x)
acf(final_arma$residuals)


# for testing randomness over a group of lags
Box.test(x, lag = 20, type = "Ljung-Box")
Box.test(final_arma$residuals, lag = 20, type = "Ljung-Box")


#####  ARIMA

test <- seq(1, 10, 2)
diff(test)
diff(test, d=2)   # to arry out repeated differencing


set.seed(2) 
x <- arima.sim(list(order = c(1,1,1), ar = 0.6, ma=-0.5), n = 1000)   # d = 1 non-stationary time series with a stochastic trending component
plot(x)

x_arima <- arima(x, order = c(1,1,1))
x_arima

acf(x_arima$residuals)

x_arima_0 <- arima(x, order = c(1,0,1))

Box.test(x_arima$residuals, lag = 20, type = "Ljung-Box")
Box.test(x_arima_0$residuals, lag = 20, type = "Ljung-Box")


library(forecast)
no <- 20
plot(forecast(x_arima, h = no), xlim = c(950, 1000 + no), ylim = c(70, 90) )  # 95 and 99 error band


#### searching for arima

final_aic <- Inf
final_order <- c(0,0,0)
for (p in 0:4) for (d in 0:1) for (q in 1:4) {
  current_aic <- AIC(arima(x, order=c(p, d, q)))
  if (current_aic < final_aic) {
    final_aic <- current_aic
    final_order <- c(p, d, q)
    final_arima <- arima(x, order=final_order)
  }
}

final_aic
final_order
final_arima

acf(final_arima$residuals)
Box.test(final_arima$residuals, lag = 20, type = "Ljung-Box")

no <- 20
plot(forecast(final_arima, h = no), xlim = c(950, 1000 + no), ylim = c(70, 90) )  # 95 and 99 error band


# GARCH(1,1)
set.seed(2)
a0 <- 0.2
a1 <- 0.5
b1 <- 0.3
w <- rnorm(10000)
eps <- rep(0, 10000)  #store time series vals
sigsq <- rep(0, 10000)   # store ARMA variances
for (i in 2:10000) {
   sigsq[i] <- a0 + a1 * (eps[i-1]^2) + b1 * sigsq[i-1]
   eps[i] <- w[i]*sqrt(sigsq[i])
}

plot(eps, type = "l")

acf(eps)
Box.test(acf(eps)$acf, lag = 20, type = "Ljung-Box")  # like discrete whitre noise

acf(eps^2)   # evidence of conditionally heteroskedastic

library(tseries)
eps_garch <- garch(eps, trace = F)    # trace - excessive output   , domyslnie order - c(1,1)
eps_garch

eps_garch$order
eps_garch$coef
eps_garch$vcov

confint(eps_garch)

garch_res <- eps_garch$residuals[-1]   # first is NA

acf(garch_res)
acf(garch_res^2)


# GARCH(X,X)  X => 1:2

set.seed(2)
a0 <- 0.2
a1 <- 0.5
a2 <- 0.2
b1 <- 0.3
b2 <- 0.2
w <- rnorm(10000)
eps <- rep(0, 10000)  #store time series vals
sigsq <- rep(0, 10000)   # store ARMA variances
for (i in 3:10000) {
  #sigsq[i] <- a0 + a1 * (eps[i-1]^2) + a2 * (eps[i-2]^2) +b1 * sigsq[i-1] + b2 * sigsq[i-2]
  #sigsq[i] <- a0 + a1 * (eps[i-1]^2) + b1 * sigsq[i-1] + b2 * sigsq[i-2]
  sigsq[i] <- a0 + a1 * (eps[i-1]^2) + a2 * (eps[i-2]^2) +b1 * sigsq[i-1] 
  eps[i] <- w[i]*sqrt(sigsq[i])
}

plot(eps, type = "l")

acf(eps)
Box.test(acf(eps)$acf, lag = 20, type = "Ljung-Box")  # like discrete whitre noise

acf(eps^2)   # evidence of conditionally heteroskedastic

library(tseries)
eps_garch <- garch(eps, order = c(1,2), trace = F)    # trace - excessive output
eps_garch

eps_garch$order
eps_garch$coef
eps_garch$vcov

confint(eps_garch)   # wartosci nijak sie maja do ustawianych, nawet jak poprawnie wybierzemy rzad

garch_res <- eps_garch$residuals[-1]   # first is NA

acf(garch_res)
acf(garch_res^2)


################3 combine arima and garch
library(rugarch)

# spReturns = diff(log(Cl(GSPC)))   # zamiast integrated 

# Create the forecasts vector to store the predictions
windowLength = 500
foreLength = length(spReturns) - windowLength
forecasts <- vector(mode="character", length=foreLength)

for (d in 0:foreLength) {
  # Obtain the S&P500 rolling window for this day
  spReturnsOffset = spReturns[(1+d):(windowLength+d)]
  
  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    if ( p == 0 && q == 0) {
      next
    }
    
    arimaFit = tryCatch( arima(spReturnsOffset, order=c(p, 0, q)),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(spReturnsOffset, order=final.order)
      }
    } else {
      next
    }
  }
  
  # Specify and fit the GARCH model
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
    distribution.model="sged"
  )
  fit = tryCatch(
    ugarchfit(
      spec, spReturnsOffset, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  if(is(fit, "warning")) {
    forecasts[d+1] = paste(index(spReturnsOffset[windowLength]), 1, sep=",")
    print(paste(index(spReturnsOffset[windowLength]), 1, sep=","))
  } else {
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
    print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")) 
  }
}


