#################### load required packages
library(smotefamily)
library(caret)
library(data.table)
library(lubridate)
library(pROC)
library(yfR)
library(h2o)
library(ggplot2)
library(cowplot)
################
DateCUTOFF = '2005-01-01' # train/test switch date
SS = 40                   # smooth used by EWMA heuristic
#####################################
#### function to plot SP500 prices, returns, vol, etc for whole history
#####################################
plotRaw = function(data = X, CEX = 1.0)
{
  par(mfrow=c(3,1),mar = c(1.9, 5.5, 0.5, 1))

  plot(data$ref_date, data$ret_adjusted_prices, type = 'l', col = 4, xlab = '', ylab = 'Returns', cex.lab = CEX, cex.axis = CEX)
  abline(h = 0, lty = 2, col = 'grey69')
  plot(data$ref_date, cumsum(data$ret_adjusted_prices), type='l', col = 4, xlab = '', ylab = 'Additive\nReturns', cex.lab = CEX, cex.axis = CEX)
  abline(h = 0, lty = 2, col = 'grey69')

  plot(data$ref_date, data$Vol, type = 'l', col = 4, xlab = '', ylab = 'Volatility\n(of Returns)', cex.lab = CEX, cex.axis = CEX, ylim = c(0,0.035))
}

plotAdj = function(data = X, CEX = 1.0)
{
    par(mfrow=c(3,1),mar = c(1.9, 5.5, 0.5, 1))

    plot(data$ref_date, data$RetAdj, type='l', col=4, xlab='', ylab='Adj Returns', cex.lab = CEX, cex.axis = CEX)
    abline(h = 0, lty=2, col='grey69')
    plot(data$ref_date, cumsum(data$RetAdj), type='l', col=4, xlab='', ylab='Additive\nAdj Returns', cex.lab = CEX, cex.axis = CEX)
    abline(h = 0, lty=2, col='grey69')

    plot(data$ref_date, sqrt(EWMA(data$RetAdj^2,100, init=1)),type='l', col=4, xlab='', ylab='Volatility\n(Adj Returns)', cex.lab = CEX, cex.axis = CEX, ylim = c(0,3))
    abline(h = 1, lty=2, col='grey69')
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
 #   first_date <- as.Date('1957-03-04') # date when index started consisting of 500 stocks
 #   last_date <- Sys.Date()

# Step 3: Download data
    sp500_data <- yf_get(
        tickers = Ticker,
        first_date = startDate,
        last_date = endDate,
        thresh_bad_data = 0.05,
        be_quiet = TRUE, do_cache = FALSE
    )

    sp500_data = data.frame(sp500_data[,c('ref_date','price_adjusted','ret_adjusted_prices')])
    sp500_data$Vol = sqrt(EWMA(sp500_data$ret_adjusted_prices^2, 100)) # use 100 day EWMA to calc vol of returns
    sp500_data$Vol = data.table::shift(sp500_data$Vol) # lag vol by one time step
    sp500_data$RetAdj = sp500_data$ret_adjusted_prices/sp500_data$Vol # vol adjusted returns(t) = ret(t)/vol(t-1)

    sp500_data = na.omit(sp500_data) # remove any rows with NA
    sp500_data$addPrices = 1+cumsum(sp500_data$ret_adjusted_prices)
    return(sp500_data)
}


# Straightforward CP labelling using forwards/backwards EWMA's
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

# as above but takes a vector, not a DF
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
LabelFnBasic = function(RETS = ret, S = 50)
{
    Price = cumsum(RETS)
 
    F = EWMA(Price ,S)
    B = rev(EWMA(rev(Price ),S))

    P = 1*(F<B)
    out = P
    return(out)
}

## uses forwards/backwards EWMA's to determine trends, with an additional EWMA step to remove any noisy/short categories
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


## function to calc drawdown of a price series
calculate_drawdown <- function(price_series) 
{
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

#### functions for Covering Metric Calculation
# Build segment list from change-point indices (1-based, inclusive ends)
# cpts: integer vector of change-point end indices (sorted); LEN: series length
segments_from_cpts <- function(cpts, LEN) {
  cpts <- sort(unique(cpts))
  ends   <- c(cpts, LEN)
  starts <- c(1L, head(ends, -1L) + 1L)
  data.frame(start = starts, end = ends, len = ends - starts + 1L, check.names = FALSE)
}

# Length of intersection between two closed intervals [a1,a2] and [b1,b2]
inter_len <- function(a1, a2, b1, b2) {
  lo <- max(a1, b1); hi <- min(a2, b2)
  max(0L, hi - lo + 1L)
}

# Jaccard overlap between two closed intervals
jaccard_interval <- function(a1, a2, b1, b2) {
  inter <- inter_len(a1, a2, b1, b2)
  if (inter == 0L) return(0)
  len_a <- a2 - a1 + 1L
  len_b <- b2 - b1 + 1L
  inter / (len_a + len_b - inter)
}

# Covering metric C(G, G'):
# G_true, G_hat are data.frames with columns start,end,len (as from segments_from_cpts)
covering_metric <- function(G_true, G_hat, LEN) {
  if (nrow(G_true) == 0L) return(0)
  # For each true segment, find the max Jaccard with any detected segment
  contrib <- vapply(seq_len(nrow(G_true)), function(i) {
    a1 <- G_true$start[i]; a2 <- G_true$end[i]
    if (nrow(G_hat) == 0L) return(0)
    jmax <- max(vapply(seq_len(nrow(G_hat)), function(j) {
      jaccard_interval(a1, a2, G_hat$start[j], G_hat$end[j])
    }, numeric(1)))
    G_true$len[i] * jmax
  }, numeric(1))
  sum(contrib) / LEN
}

# Convenience wrapper: give change points directly
# true_cpts / det_cpts: integer vectors of change-point end indices; T: series length
covering_from_cpts <- function(true_cpts, det_cpts, LEN) {
  G_true <- segments_from_cpts(true_cpts, LEN)
  G_hat  <- segments_from_cpts(det_cpts,  LEN)
  covering_metric(G_true, G_hat, LEN)
}
###################