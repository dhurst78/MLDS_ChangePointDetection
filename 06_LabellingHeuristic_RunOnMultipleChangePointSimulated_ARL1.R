# ------------------------------------------------------------
# Multiple-CP simulation study (EWMA labelling heuristic)
# - Loads S&P 500 data
# - Builds simulated returns with alternating regimes
# - Compares labels, visualises overlap, computes ARL1
# ------------------------------------------------------------

rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########

delta = 0.1

#####################################################################################################################################################################
# get real data, and extract trend lengths
#
SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')

LABEL = LabelFnBasicDouble(RETS = SPdata$RetAdj, S = SS) # label data

SPdata$LABEL = LABEL
median(SPdata$RetAdj[LABEL==0])
median(SPdata$RetAdj[LABEL==1])

mean(SPdata$RetAdj[LABEL==0])
mean(SPdata$RetAdj[LABEL==1])


## get run lengths
runs = rle(LABEL)$lengths
mean(runs) # 491

DF = data.frame(length=rle(LABEL)$lengths, value = rle(LABEL)$values)

mean(DF$length[DF$value==0])  # 237
mean(DF$length[DF$value==1])  # 731


mean(DF$length[DF$value==0])/252
mean(DF$length[DF$value==1])/252


min(DF$length[DF$value==0])  # 20
min(DF$length[DF$value==1])  # 76


### use the above trend lengths to generate random data

generate_alternating_data <- function(v) {
  means <- rep(c(delta, -delta), length.out = length(v))
  data <- unlist(mapply(function(n, m) rnorm(n, mean = m), v, means))
  return(data)
}

# Example usage
v <- runs
#set.seed(123)  # For reproducibility
simulated_data <- generate_alternating_data(v)
plot(SPdata$ref_date,cumsum(simulated_data), type='l')


################### - now label these classes
### - initially, count number of correct predictions on each data

N = 1000
M = matrix(nrow=nrow(SPdata), ncol=N)

for (i in 1:N)
{
simulated_data <- generate_alternating_data(v)
LABELtemp = LabelFnBasicDouble(RETS = simulated_data, S = SS)
M[,i] = 1*(LABELtemp==LABEL)
}

CP = abs(LABEL-data.table::shift(LABEL))


### draw a few cases of random data, together with actual SP500 data
N=10
COL = gray.colors(N , 0.1, 0.9)


CEX = 1.
pdf(file="ARL1_multiple.pdf", width=10, height=5)
par(mar = c(2., 4, 0.8,0.5),mfrow=c(2,1),mgp = c(2.2, 0.6, 0))

plot(SPdata$ref_date,cumsum(SPdata$RetAdj), type='n', cex.axis = CEX, cex.lab = CEX, cex.main=CEX, ylab = 'Simulated Additive returns', xlab='', col=4, ylim = c(-100,1000))


for (i in 1:N)
{
lines(SPdata$ref_date,cumsum(generate_alternating_data(v)), col=COL[i])
}

lines(SPdata$ref_date,cumsum(SPdata$RetAdj), col=4)

abline(v=SPdata$ref_date[CP==1],lty=2,col=2)
legend('topleft', legend = c('SP500','Simulated Data'), cex = CEX, text.col=c(4, 'grey69'),bg = "white", y.intersp = 0.8)
legend('bottomright', legend = c('Actual Changepoints'),bg = "white", cex = CEX, lty=c(2), col=2, y.intersp = 0.8)
box()

### plot accuracy of labelling heuristic
plot(SPdata$ref_date,1-rowMeans(M),type='l', ylim = c(0,1), xlab='',ylab='Proportion of\nMiss-classifications', col=4, cex.axis = CEX, cex.lab = CEX, cex.main=CEX)
abline(v=SPdata$ref_date[CP==1],lty=2,col=2)
######

legend('topleft', legend = c('Actual Changepoints'),bg = "white", cex = CEX, lty=c(2), col=2, y.intersp = 0.8)
dev.off()

mean(M)

mean(rowMeans(M))
median(rowMeans(M))

true_cps = which(CP!=0) ##### get actual CP's


#### Fn to calculate ARL1 in presence of multiple CP's
#### following logic described in bodenham paper

calculate_ARL1 <- function(true_cps, predicted_cps, B = 50) {
  delays <- c()
  
  for (i in seq_along(true_cps)) {
    cp <- true_cps[i]
    
    # Set end of window (either next CP or Inf)
    window_end <- if (i < length(true_cps)) true_cps[i + 1] else Inf
    
    # Define valid prediction window
    window_start <- cp + B
    
    # Find predictions in the window
    candidate_preds <- predicted_cps[predicted_cps > window_start & predicted_cps < window_end]
    
    if (length(candidate_preds) > 0) {
      delay <- candidate_preds[1] - cp
      delays <- c(delays, delay)
    } 
    # else: missed detection, do not add to delays
  }
  
  ARL1 <- mean(delays)
  return(ARL1)
}



##### function to generate simulated data with multiple CP's
##### run labelling heuristic
##### calculate ARL1

ARL1_mult = function()
{
simulated_data <- generate_alternating_data(v)
LABELtemp = LabelFnBasicDouble(RETS = simulated_data, S = SS)

CPtemp = abs(LABELtemp-data.table::shift(LABELtemp))
predicted_cps = which(CPtemp==1)

return (calculate_ARL1(true_cps, predicted_cps, B = 5)
)

}

E = replicate(1000,ARL1_mult())
E
mean(E) # 254
mean(E)/252 

mean(SPdata$RetAdj[LABEL==0])
median(SPdata$RetAdj[LABEL==1])


mean(DF$length[DF$value==0])/252
mean(DF$length[DF$value==1])/252


########################################
# - now calculate Covering Metric
# - generate random data
# - label it
# - calculate the covering metric
# - plot histogram of resulting values
#######################################

N = 1000

RelRetMean = rep(NA,N)
scoreCovering = rep(NA,N)

true_cpts = cumsum(v)
LEN = nrow(SPdata)

for (i in 1:N)
{
simulated_data = generate_alternating_data(v)
LABELtemp = LabelFnBasicDouble(RETS = simulated_data, S = SS)
P = 1*(LABELtemp==LABEL) # position
RET = P*simulated_data
RelRetMean[i] = mean(RET-simulated_data)

change = LABELtemp-shift(LABELtemp)
 
det_cpts  = which(abs(change)==1) 
scoreCovering[i] = covering_from_cpts(true_cpts, det_cpts, LEN)

}


CEX = 1.1

pdf(file="CoveringScore_hist.pdf", width=6, height=3.)
par(mar = c(3.8,3.8,0.4,0.4),mgp = c(2.2, 0.6, 0))

hist(scoreCovering, col = rgb(0, 0, 1, 0.5), xlim = c(0, 1),main='', ylim = c(0,8),
			xlab = "Covering Metric", freq=FALSE, breaks = sqrt(N))
grid()

dev.off()

range(scoreCovering)
mean(scoreCovering)
