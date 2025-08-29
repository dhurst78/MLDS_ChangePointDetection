# ------------------------------------------------------------
# Run Labelling Heuristic on synthetic data
# consider two cases: zero CP's, one CP
# calculate ARL0 and ARL1
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########

delta = 0.1

set.seed(6)
####################
###################
################### - calculate ARL0 and ARL1 on simulated datasets
####################
###################

# ARL0 - It is the expected number of observations collected before a false alarm is raised.
# In other words, it's the average time to a false detection when there is no actual changepoint.


###################
# - function to generate simulated data with no changepoint
# - applies labelling heuristic with a specified smooth, s
# - returns the location of the first detected CP
ARL0_Fn = function(delta = 0.1, S = 100)
{
x = rnorm(mean = delta, sd = 1, 100000)  # generate data
XX = LabelFnBasicDouble(RETS = x, S = S) # label it
Delta = XX-shift(XX)
Delta[1] = 0
return(which(Delta!=0)[1])               # return location of first detected CP
}

# run function 1000 times, for varying smooth values

ARL0_s020 = replicate(1000, ARL0_Fn(delta = 0.1, S =  20))
ARL0_s040 = replicate(1000, ARL0_Fn(delta = 0.1, S =  40))
ARL0_s060 = replicate(1000, ARL0_Fn(delta = 0.1, S =  60))
ARL0_s080 = replicate(1000, ARL0_Fn(delta = 0.1, S =  80))
ARL0_s100 = replicate(1000, ARL0_Fn(delta = 0.1, S = 100))
ARL0_s120 = replicate(1000, ARL0_Fn(delta = 0.1, S = 120))
ARL0_s140 = replicate(1000, ARL0_Fn(delta = 0.1, S = 140))

## calculate means and put into DF
x = c(20,40,60,80,100,120,140)
d = list(ARL0_s020,ARL0_s040,ARL0_s060,ARL0_s080,ARL0_s100,ARL0_s120,ARL0_s140)
y = unlist(lapply(d,mean,na.rm=TRUE))


#######
###################
# - function to generate simulated data with 1 changepoint
# - applies labelling heuristic with a specified smooth, s
# - returns the location of the first detected CP

ARL1_Fn = function(delta = 0.1, S = 100)
{
x = c(rnorm(mean = delta,sd=1,10000),rnorm(mean = -delta,sd=1,10000))
XX = LabelFnBasicDouble(RETS = x, S = S)
Delta = XX-shift(XX)
Delta[1] = 0
return(which(Delta!=0)[1])
}



# run function 1000 times, for varying smooth values

ARL1_s020 = replicate(2000,ARL1_Fn(delta = 0.1, S =  20))
ARL1_s040 = replicate(2000,ARL1_Fn(delta = 0.1, S =  40))
ARL1_s060 = replicate(2000,ARL1_Fn(delta = 0.1, S =  60))
ARL1_s080 = replicate(2000,ARL1_Fn(delta = 0.1, S =  80))
ARL1_s100 = replicate(2000,ARL1_Fn(delta = 0.1, S = 100))
ARL1_s120 = replicate(2000,ARL1_Fn(delta = 0.1, S = 120))
ARL1_s140 = replicate(2000,ARL1_Fn(delta = 0.1, S = 140))


FnMeanThreshold = function(x,thresh)
{
x = x[x>thresh] # only keep values past a threshold
return(mean(x,na.rm=TRUE))
}

# calculate mean values (ignoring cases where the CP is detected before actual point)
t020 = FnMeanThreshold(ARL1_s020,thresh = 10000)
t040 = FnMeanThreshold(ARL1_s040,thresh = 10000)
t060 = FnMeanThreshold(ARL1_s060,thresh = 10000)
t080 = FnMeanThreshold(ARL1_s080,thresh = 10000)
t100 = FnMeanThreshold(ARL1_s100,thresh = 10000)
t120 = FnMeanThreshold(ARL1_s120,thresh = 10000)
t140 = FnMeanThreshold(ARL1_s140,thresh = 10000)


### plot
CEX = 1.15
pdf(file="ARL0_ARL1_scatter.pdf", width=10, height=4)
par(mar = c(4.,4,1.5,0.8),mfrow=c(1,2),mgp = c(2.2, 0.6, 0))

plot(x,y,xlab = 'EWMA Smooth, days', ylab='ARL0 (days)', pch=16, col=4, log = 'y', cex.lab = CEX, cex.axis = CEX, cex=1.3)
grid()

plot(x,c(t020,t040,t060, t080, t100, t120, t140)-10000, xlab='EWMA Smooth, days', ylab = 'ARL1 (days)', pch = 16, col = 4, cex.lab = CEX, cex.axis = CEX, cex=1.3)
grid()
dev.off()

sum(ARL1_s020>10000)
