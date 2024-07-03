# Volatility Smile

## Overview

This repository provides a comprehensive analysis of the volatility smile, including the calculation of drift and volatility, implementation of Black-Scholes formulas for European options, computation of implied volatility, and analysis of stock price processes. The code is implemented in R and uses the `quantmod` and `xts` libraries for data handling and manipulation.

## Installation

Ensure you have the following R packages installed:

```R
install.packages("quantmod")
install.packages("xts")
```

## Usage

### Data Import and Preprocessing

1. **Load necessary libraries:**

   ```R
   library(quantmod)
   library(xts)
   ```

2. **Get historical stock data:**

   ```R
   getSymbols("SCCO", from="2020-01-01", to="2023-01-01")
   plot(SCCO$SCCO.Close)
   ```

3. **Calculate log returns:**

   ```R
   SCCO.lrs <- diff.xts(SCCO$SCCO.Close, lag=1, log=TRUE, na.pad=FALSE)
   plot(SCCO.lrs)
   ```

### Calculating Drift and Volatility

1. **Compute volatility and drift:**

   ```R
   vol <- sd(SCCO.lrs) * sqrt(255)
   drift <- mean(SCCO.lrs)
   ```

### Black-Scholes Formulas

1. **Define Black-Scholes call and put option formulas:**

   ```R
   BS.Call <- function(s, k, time, rate, vol){
     vt <- vol * sqrt(time)
     d1 <- (log(s / k) + (rate + 0.5 * vol^2) * time) / vt
     d2 <- d1 - vt
     s * pnorm(d1) - exp(-rate * time) * k * pnorm(d2)
   }

   BS.Put <- function(s, k, time, rate, vol){
     vt <- vol * sqrt(time)
     d1 <- (log(s / k) + (rate + 0.5 * vol^2) * time) / vt
     d2 <- d1 - vt
     exp(-rate * time) * k * pnorm(-d2) - s * pnorm(-d1)
   }
   ```

2. **Compute option values:**

   ```R
   BS.Call(s=80, k=55.72, time=0.5, rate=0.05, vol)
   BS.Put(s=80, k=90, time=0.5, rate=0.05, vol)
   ```

3. **Plot Black-Scholes formulas as a function of stock price:**

   ```R
   plot(function(x) BS.Call(s=x, k=55.72, time=0.5, rate=0.05, vol),
        xlim=c(0,400),
        col="chartreuse3",
        xlab="Stock Price",
        ylab="Call Price")
   abline(v=55.72, col="darkgray")

   plot(function(x) BS.Put(s=x, k=55.72, time=0.5, rate=0.05, vol),
        xlim=c(0,400),
        col="Red",
        xlab="Stock Price",
        ylab="Put Price")
   ```

### Implied Volatility

1. **Define implied volatility functions:**

   ```R
   ivol.Call <- function(call, s, k, time, rate, upper=5) {
     if (max(0, s - k) < call & call < s) {
       uniroot(function(x) BS.Call(s=s, k=k, time=time, rate=rate, vol=x) - call,
               lower=0, upper=upper, extendInt="no")$root
     } else 0
   }

   ivol.Put <- function(put, s, k, time, rate, upper=5) {
     if (max(0, exp(-rate*time) * k - s) < put & put < exp(-rate*time) * k) {
       uniroot(function(x) BS.Put(s=s, k=k, time=time, rate=rate, vol=x) - put,
               lower=0, upper=upper, extendInt="no")$root
     } else NA
   }
   ```

2. **Compute implied volatilities:**

   ```R
   ivol.Call(call=15, s=80.49, k=70, time=0.5, rate=0.05, upper=5)
   ivol.Put(put=3.5, s=80.49, k=70, time=0.5, rate=0.05, upper=5)
   ```

### Option Chain and Implied Volatility Plotting

1. **Import option chain:**

   ```R
   SCCO.options <- getOptionChain("SCCO", "2023/2024")
   ```

2. **Compute implied volatilities for calls and puts:**

   ```R
   ivols.Call <- NULL

   compute.ivols.Call <- function(options, s, rate=0.05, time) {
     n <- length(options)
     for (i in 1:n) {
       k <- options$Strike[i]
       call <- options$Ask[i]
       ivols.Call$Vol[i] <<- ivol.Call(call=call, s=s, k=k, rate=rate, time=time)
       ivols.Call$Strike[i] <<- k
     }
   }

   compute.ivols.Call(SCCO.options$Sep.15.2023$calls, s=80, rate=0.05, time=0.5)

   ivols.Put <- NULL

   compute.ivols.Put <- function(options, s, rate=0.05, time) {
     n <- length(options)
     for (i in 1:n) {
       k <- options$Strike[i]
       put <- options$Ask[i]
       ivols.Put$Vol[i] <<- ivol.Put(put=put, s=s, k=k, rate=rate, time=time)
       ivols.Put$Strike[i] <<- k
     }
   }

   compute.ivols.Put(SCCO.options$Sep.15.2023$puts, s=80.49, rate=0.05, time=0.5)
   ```

3. **Plot implied volatility:**

   ```R
   plot(ivols.Call$Strike, ivols.Call$Vol, type="l", col="red",
        xlab="Strike", ylab="Implied Vol")
   lines(ivols.Call$Strike, ivols.Call$Vol, type="l", col="blue")

   plot(ivols.Put$Strike, ivols.Put$Vol, type="l", col="red",
        xlab="Strike", ylab="Implied Vol")
   lines(ivols.Put$Strike, ivols.Put$Vol, type="l", col="blue")
   ```

### Model Assumptions and Data Analysis

1. **Assumption 1: Gaussian Stochastic Process:**

   ```R
   hist(SCCO.lrs, breaks=20, col="lightblue")
   qqnorm(scale(SCCO.lrs), col="lightblue")
   qqline(scale(SCCO.lrs), col="blue")
   ```

2. **Plot log-returns on Simple Moving Average:**

   ```R
   plot(SCCO.lrs, col="darkgreen")
   lines(sqrt(SMA(SCCO.lrs^2, n=30)), col="red")
   ```

3. **Assumption 2: Stock Price is a Markov Process:**

   ```R
   k.simple <- function(data, tau, n) {
     cor(data[1:n,], data[(1+abs(tau)):(n+abs(tau)),])
   }

   plot(Vectorize(function(tau) k.simple(SCCO.lrs, tau=tau, n=50)),
        xlim=c(-200,200),
        col="blue",
        xlab="tau",
        ylab="Cor")
   abline(h=0, col="lightgray")

   acf(SCCO.lrs, lag.max=100)

   plot(Vectorize(function(x) k.simple(SCCO.lrs, x, n=100)),
        from=0, to=100,
        col="darkgreen",
        xlab="Tau",
        ylab="Cor")
   abline(h=0, col="lightgray")
   plot(Vectorize(function(x) abs(k.simple(SCCO.lrs, x, n=100))),
        from=0, to=100,
        col="red",
        add=TRUE)

   sum(sapply(0:100, function(x) abs(as.numeric(k.simple(SCCO.lrs, x, n=100)))))
   ```

4. **Compute and plot spectral density:**

   ```R
   plot(abs(fft(apply(matrix(-100:100), 1,
                     function(x) k.simple(SCCO.lrs, x, 50)))),
        type="l",
        col="red",
        ylab="Amplitude")
   ```

## Contributing

Contributions are welcome. Please open an issue or submit a pull request for any improvements or bug fixes.

## License

This project is licensed under the MIT License.
