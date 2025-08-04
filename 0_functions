rm(list=ls())


#####################################
#### function to plot SP500 prices, returns, vol, etc for whole history
#####################################
plotRaw = function(data = X, CEX = 1.4)
{
   # options(repr.plot.width = 13, repr.plot.height = 6)
    par(mfrow=c(3,1),mar = c(2,4,2,1))

    

    plot(data$ref_date, data$ret_adjusted_prices, type='l', col=4, xlab='', ylab='Returns', cex.lab = CEX, cex.axis = CEX)
    abline(h = 0, lty=2, col='grey69')
    plot(data$ref_date, cumsum(data$ret_adjusted_prices), type='l', col=4, xlab='', ylab='Additive Returns', cex.lab = CEX, cex.axis = CEX)
    abline(h = 0, lty=2, col='grey69')


    plot(data$ref_date, data$Vol,type='l', col=4, xlab='', ylab='Volatility (of Returns)', cex.lab = CEX, cex.axis = CEX, ylim = c(0,0.035))
#    abline(h = 0, lty=2, col='grey69')

}

plotAdj = function(data = X, CEX = 1.4)
{
   # options(repr.plot.width = 13, repr.plot.height = 6)
    par(mfrow=c(3,1),mar = c(2,4,2,1))

    plot(data$ref_date, data$RetAdj, type='l', col=4, xlab='', ylab='Adj Returns', cex.lab = CEX, cex.axis = CEX)
    abline(h = 0, lty=2, col='grey69')
    plot(data$ref_date, cumsum(data$RetAdj), type='l', col=4, xlab='', ylab='Additive Adj Returns', cex.lab = CEX, cex.axis = CEX)
    abline(h = 0, lty=2, col='grey69')

    plot(data$ref_date, sqrt(EWMA(data$RetAdj^2,100, init=1)),type='l', col=4, xlab='', ylab='Volatility (of Adj Returns)', cex.lab = CEX, cex.axis = CEX, ylim = c(0,3))
    abline(h = 1, lty=2, col='grey69')


}


####################
dual_axis_plot <- function(x, y1, y2, y1_label = "Y1", y2_label = "Y2", x_label = "X", main_title = "Dual Axis Plot") {
  par(mar = c(4.5,4.0,1.5,4.5))
  # Plot y1 vs x
  plot(x, y1, type = "l", col = "blue", lwd = 2,
       ylab = y1_label, xlab = x_label, main = main_title)

  # Add y2 vs x on a new axis
  par(new = TRUE)
  plot(x, y2, type = "l", col = "red", lwd = 2, axes = FALSE, xlab = "", ylab = "")
  axis(side = 4, col = "red", col.axis = "red")  # Add right-side axis
  mtext(y2_label, side = 4, line = 3, col = "red")  # Right-side label

  # Add legend
  legend("topleft", legend = c(y1_label, y2_label),  col = c("blue", "red"), lty = 1, lwd = 2)
}


##########
rolling_mean = function(x, window = 5, sides = 1) {
  stats::filter(x, rep(1 / window, window), sides = sides)
}
#####################################
#### fast EWMA function, initialised to be the mean of the first non NA value
#####################################
EWMA = function (x, days = 100, init = mean(x[which(!is.na(x))[1]], 
    na.rm = TRUE)) 
{
    OUT = rep(NA, length(x))
    lambda = 1 - 1/days
    if (sum(!is.na(x)) < 1) {
        return(OUT)
    }
    k = !is.na(x)
    tt = stats::filter((1 - lambda) * x[k], filter = lambda, 
        method = "recursive", init = init)
    OUT[k] = tt
    return(OUT)
}

#####################################
#### download SP500 price data from Yahoo
#####################################
GetPrices = function(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2033-07-01') # date when index started consisting of 500 stocks
{
    library(yfR)

 #   first_date <- as.Date('1957-03-04') # date when index started consisting of 500 stocks
 #   last_date <- Sys.Date()

# Step 3: Download data
    sp500_data <- yf_get(
        tickers = Ticker,
        first_date = startDate,
        last_date = endDate,
        thresh_bad_data = 0.05,
        be_quiet = TRUE,
        do_cache = FALSE
    )

    sp500_data = data.frame(sp500_data[,c('ref_date','price_adjusted','ret_adjusted_prices')])
    sp500_data$Vol = sqrt(EWMA(sp500_data$ret_adjusted_prices^2, 100)) # use 100 day EWMA to calc vol of returns
    sp500_data$Vol = data.table::shift(sp500_data$Vol) # lag vol by one time step
    sp500_data$RetAdj = sp500_data$ret_adjusted_prices/sp500_data$Vol # vol adjusted returns(t) = ret(t)/vol(t-1)

    sp500_data = na.omit(sp500_data) # remove any rows with NA
    sp500_data$addPrices = 1+cumsum(sp500_data$ret_adjusted_prices)
    return(sp500_data)
}



LabelFnOLD = function(DF = inDF, S = 50, retType = 'Adj')
{
    if (retType=='Adj')
    {
    Price = cumsum(DF$RetAdj)
    }
    if (retType=='Raw')
    {
    Price = cumsum(DF$ret_adjusted_prices)
    }
    F = EWMA(Price ,S)
    B = rev(EWMA(rev(Price ),S))

    P = 1*(F<B)
    out = data.frame(Date=DF$ref_date, P)
    out = out[101:( nrow(out) - 100 ),] # remove first and last 100 rows (EWMA's warming up)
    return(out)
}

# as above, but don't remove first and last x rows
LabelFn = function(DF = inDF, S = 50, retType = 'Adj')
{
    if (retType=='Adj')
    {
    Price = cumsum(DF$RetAdj)
    }
    if (retType=='Raw')
    {
    Price = cumsum(DF$ret_adjusted_prices)
    }
    F = EWMA(Price ,S)
    B = rev(EWMA(rev(Price ),S))

    P = 1*(F<B)
    out = data.frame(Date=DF$ref_date, P)
    return(out)
}


LabelFnSyn = function(RETS = ret, S = 50)
{
    Price = cumsum(RETS)
 
    F = EWMA(Price ,S)
    B = rev(EWMA(rev(Price ),S))

    P = 1*(F<B)
    out = P
    out = out[101:( length(out) - 100 )] # remove first and last 100 rows (EWMA's warming up)
    return(out)
}

## as above, but doens't remove start and end warm up periods
#
LabelFnBasic = function(RETS = ret, S = 50)
{
    Price = cumsum(RETS)
 
    F = EWMA(Price ,S)
    B = rev(EWMA(rev(Price ),S))

    P = 1*(F<B)
    out = P
    return(out)
}


LabelFnBasicDouble = function(RETS = ret, S = 50)
{
    Price = cumsum(RETS)
 
    F = EWMA(Price ,S)
    B = rev(EWMA(rev(Price ),S))
    X1 = F-B
### now do second pass
    FF = EWMA(X1, S)
    BB = rev(EWMA(rev(X1), S))
    X = rowMeans(cbind(BB, FF)) # pair-wise means of EWMA's
    P = 1*(X < 0)
    return(P)
}



LabelFn2 = function(Rets = poo$ret_adjusted_prices, S = 50)
{
    Price = cumsum(Rets)
    F = rolling_mean(Price ,S)
    B = rev(rolling_mean(rev(Price ),S))

    P = 1*(F<B)
    return(P)
}


## uses forwards/backwards EWMA's to determine trends, with an additional EWMA step to remove any noisy/short categories
LabelFnDouble = function(DF = inDF, S = 50, retType = 'Adj')
{
    if (retType == 'Adj')
    {
    Price = cumsum(DF$RetAdj)
    }
    if (retType == 'Raw')
    {
    Price = cumsum(DF$ret_adjusted_prices)
    }
    F = EWMA(Price, S)
    B = rev(EWMA(rev(Price), S))

    X1 = F-B
### now do second pass
    FF = EWMA(X1, S)
    BB = rev(EWMA(rev(X1), S))
    X = rowMeans(cbind(BB, FF)) # pair-wise means of EWMA's
    P = 1*(X < 0)
    out = data.frame(Date=DF$ref_date, P)
    return(out)
}



## fn which calculates runs of binary series
LabelRuns = function(x = P)
{
# Use run-length encoding
    runs = rle(x)
# Combine into a data frame for clarity
    out <- data.frame(value = runs$values,length = runs$lengths)
    return(out)
}

## function to calc: mean, min, max of a vector
AGG = function(y = y)
{
    return(aggregate(length ~ value, data = y, FUN = function(x) c(mean = mean(x), min = min(x), max = max(x))))
}

AGGa = function(y = y)
{
    return(dplyr::summarise(dplyr::group_by(y, value), mean = mean(length), min = min(length), max = max(length)))
}

## function to calc: mean, upper/lower decile of a vector
AGGsimp = function(x = x)
{
return(data.frame(mean=mean(x),UQ=quantile(x, probs = 0.9),LQ = quantile(x, probs = 0.1)))
}






## function to calc drawdown of a price series
calculate_drawdown <- function(price_series) {

  # Calculate cumulative historical peak
  peak_so_far <- cummax(as.numeric(price_series))
  
  # Calculate drawdown
  drawdown <- peak_so_far - price_series
  
  return(drawdown)
}

#####
#### - AHL type momentum
Momentum = function(returns = SPdata$RetAdj, K = k, Factor=3)
{
P = cumsum(returns)
Mom = EWMA(P,(2^K)) - EWMA(P,Factor*(2^K))
return(Mom)
}

