library(quantmod)
library(xts)
getSymbols("SCCO", from="2020-01-01", to="2023-01-01")
plot(SCCO$SCCO.Close)
SCCO.lrs <- diff.xts(SCCO$SCCO.Close, lag=1, log= TRUE, na.pad = FALSE)
plot(SCCO.lrs)
##log.returns2 <- log(ALB$ALB.Close)-log(lag(ALB$ALB.Close, k= 1))
##plot(log.returns2)

###Calculatind "Drift" and "Volatility"
vol <- sd(SCCO.lrs)*sqrt(255)
### multiply by 255
drift <- mean(SCCO.lrs)

##vol2 <- sd(log.returns2)*sqrt(255)
##drift2 <- mean(log.returns2)

###Now we define Black_Scholes Call and put formula
BS.Call <- function(s, k, time, rate, vol){
  vt <- vol*sqrt(time)
  d1 <- (log(s / k) + (rate + .5*vol^2)*sqrt(time))/ vt
  d2 <- d1 - vt
  s * pnorm(d1) - exp(-rate*time) * k * pnorm(d2)
}

BS.Put <- function(s, k, time, rate, vol){
  vt <- vol*sqrt(time)
  d1 <- (log(s / k) + (rate + .5*vol^2)*sqrt(time))/ vt
  d2 <- d1 - vt
  exp(-rate*time) * k * pnorm(-d2) - s * pnorm(-d1) 
}
## Computing the values of European Call and Put option

BS.Call(s=80, k=55.72, time=0.5, rate=0.05, vol)
BS.Put(s=80, k=55.72, time=0.5, rate=0.05, vol)

###Plot Black Scholes formulas as a function of stock price.
plot(function(x) BS.Call(s=x, k=55.72, time=0.5, rate=0.05, vol),
     xlim=c(0,100),
     col="red",
     xlab="Stock Price",
     ylab="Call Price")


abline(v=55.72, col = "darkgray")
text(x=53.5, y=20, srt=90, 'Strike Price = 55.72')

plot(function(x) BS.Put(s=x, k=55.72, time=0.5, rate=0.05, vol),
     xlim=c(0,100),
     col="Red",
     xlab="Stock Price",
     ylab="Put Price")
abline(v=55.72, col = "darkgray")
text(x=53, y=40, srt=90, 'Strike Price = 55.72')

## Define "implied volatility" function 

ivol.Call <- function(call, s, k, time, rate, upper = 5) {
   if(max(0, s - k) < call & call < s) {
    uniroot(function (x) BS.Call(s = s, k = k, time = time, rate = rate, vol = x) - call,
            lower = 0, upper = upper, extendInt = "no")$root
  }
  else NA
}

ivol.Put <- function(put, s, k, time, rate, upper = 5) {
  if (max(0, exp(-rate*time) * k - s) < put & put < exp(-rate*time) * k) {
    uniroot(function (x) BS.Put(s = s, k = k, time = time, rate = rate, vol = x) - put,
            lower = 0, upper = upper, extendInt = "no")$root
  } else NA
}

### Computing implied volatilies
####  *********** Question *********** : if I out call = 9.3 which is on CBOE it return 0. does it mean it's undervalued?
ivol.Call(call = 15.2, s = 75.59, k = 65, time = .5, rate = .05, upper = 5)
ivol.Put(put = 3.5, s = 80.49, k = 70, time = .5, rate = .05, upper = 5)

### Importing option chain and ploting for different strike prices and maturies
SCCO.options <- getOptionChain("SCCO", "2023/2024")

ivols.Call <- NULL

compute.ivols.Call <- function(options, s, rate = .05, time) {
  n <- length(SCCO.options)
  for (i in 1:n){
    k <- SCCO.options$Strike[i]
    call <- SCCO.options$Ask[i]
    ivols.Call$Vol[i] <<- ivol.Call(call = call,
                                    s = s,
                                    k = k,
                                    rate = rate,
                                    time = time)
    ivols.Call$Strike[i] <<- k
  }
}

compute.ivols.Call(SCCO.options$Sep.15.2023$calls, s = 80.49, rate = 0.05, time = .5)
compute.ivols.Call(100, s = 80, rate = 0.05, time = .5)


SCCO.options$Sep.15.2023$calls$Ask
ivols.Put <- NULL

compute.ivols.Put <- function(options, s, rate = .05, time){
  n <- length(SCCO.options)
  for (i in 1:n){
    k <- SCCO.options$Strike[i]
    put <- SCCO.options$Ask[i]
    ivols.Put$Vol[i] <<- ivol.Put(put = put,
                                  s = s,
                                  k = k,
                                  rate = rate,
                                  time = time)
    ivols.Put$Strike[i] <<- k
  }
}

compute.ivols.Put(SCCO.options$Sep.15.2023$puts, s = 80.49, rate = .05, time = .5)
SCCO.options$Sep.15.2023$puts$Last

SCCO.options$Sep.15.2023$puts$Strike

# ALB.options$Mar.31.2023$calls$Strike
# 
# compute.ivols.Call(options=285, s = 200, rate = .01, time = 3/12)
### 224 is the current stock price
plot(SCCO.options$Sep.15.2023$calls$Strike, SCCO.options$Sep.15.2023$calls$Ask, type = "l", col = "red")
lines(SCCO.options$Sep.15.2023$calls$Strike, 80.49 - SCCO.options$Sep.15.2023$calls$Strike, type = "l")

plot(SCCO.options$Sep.15.2023$puts$Strike, SCCO.options$Sep.15.2023$puts$Last, type = "l", col = "red")
lines(SCCO.options$Sep.15.2023$puts$Strike, exp(-.01*1/12)*SCCO.options$Sep.15.2023$puts$Strike - 8.49, type = "l")

### ploting implied volatility as a fucntion of strike price.

plot(ivols.Call$Strike, ivols.Call$Vol, type = "l", col = "red",
     xlab = "Strike", ylab = "Implied Vol")

lines(ivols.Call$Strike, ivols.Call$Vol, type = "line", col = "blue")

plot(ivols.Put$Strike, ivols.Put$Vol, type= "l",col = "red",
     xlab = "Strike", ylab = "Implied Vol")

lines(ivols.Put$Strike, ivols.Put$Vol, type = "l", col = "blue")

### 2.3 : Analysis of model assumptions and data
### Assumption 1 : Gausian Stochastic Process
hist(SCCO.lrs, breaks=20, col="lightblue")
qqnorm(scale(SCCO.lrs), col="lightblue")
qqline(scale(SCCO.lrs), col="blue")

###plotting log-returns on Simple Moving Average
plot(SCCO.lrs, col="darkgreen")
lines(sqrt(SMA(SCCO.lrs^2, n=30)), col="red")

###Assumption 2 : Stock Price is a Markov process
### Computin Auto correlation function
k.simple <- function (data , tau , n) {
  cor (data [1:n,], data[(1+abs(tau)):(n+abs(tau)),])
 }

plot(Vectorize(function(tau) k.simple(SCCO.lrs, tau = tau , n =100) ),
  xlim = c ( -200 ,200) ,
  col = " blue ",
  xlab = " tau",
  ylab = " Cor")
abline (h = 0, col = " lightgray ")

acf(SCCO.lrs, lag.max = 100)

###Computing correlation time
plot ( Vectorize ( function (x) k.simple ( SCCO.lrs , x, n = 100) ),
  from = 0, to = 100 ,
  col = " darkgreen ",
  xlab = " Tau",
  ylab = " Cor")

abline (h = 0, col = " lightgray ")

plot ( Vectorize ( function (x) abs(k.simple ( SCCO.lrs , x, n = 100) )),
  from = 0, to = 100 ,
  col = "red ",
  add = TRUE )

sum( sapply (0:100 , function (x) abs (as.numeric (k.simple (SCCO.lrs , x, n = 100) ))))


###Computeing and plotting spectral density
plot(abs(fft(apply(matrix(-100:100), 1,
                  function (x) k.simple (SCCO.lrs , x, 50)))),
      type = "l",
      col = "red ",
      ylab = " Amplitude ")
