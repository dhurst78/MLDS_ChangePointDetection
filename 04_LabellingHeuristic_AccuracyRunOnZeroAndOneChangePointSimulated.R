# ------------------------------------------------------------
# Run Labelling Heuristic on synthetic data
# consider two cases: zero CP's, one CP
# calculate the fraction of runs where Label is correct
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########
set.seed(123) # set seed for reproducability

delta = 0.1


# function to create simulated data with 1 change point
# which is then labelled by two step heuristic

singleCP = function(delta = 0.1, S = 100)
{
x = c(rnorm(mean = delta, sd = 1, 1000), rnorm(mean = -delta, sd = 1, 1000))
XX = LabelFnBasicDouble(RETS = x, S = S)
return(XX)
}

singleCP()

# generate 1000 sets of data, using different smooths for labelling heuristic
z020 = replicate(10000,singleCP(delta = 0.1, S = 20))
z040 = replicate(10000,singleCP(delta = 0.1, S = 40))
z060 = replicate(10000,singleCP(delta = 0.1, S = 60))
z100 = replicate(10000,singleCP(delta = 0.1, S = 100))
z150 = replicate(10000,singleCP(delta = 0.1, S = 150))

# calculate average label (on each 'day') (fraction of runs labelled as 1)
M020 = rowMeans(z020)
M040 = rowMeans(z040)
M060 = rowMeans(z060)
M100 = rowMeans(z100)
M150 = rowMeans(z150)

# put data into DF together with an index
DF = data.frame(t = seq(-999,1000),M020,M040,M060,M100,M150)
# switch the fraction for t<=0, as here the true trend was up.
DF$M020[DF$t<=0] = 1-DF$M020[DF$t<=0]
DF$M040[DF$t<=0] = 1-DF$M040[DF$t<=0]
DF$M060[DF$t<=0] = 1-DF$M060[DF$t<=0]
DF$M100[DF$t<=0] = 1-DF$M100[DF$t<=0]
DF$M150[DF$t<=0] = 1-DF$M150[DF$t<=0]


#################
######################### - repeat with no changepoints
# function to create simulated data with 0 change point
# which is then labelled by two step heuristic

zeroCP = function(delta = 0.1, S = 100)
{
x = rnorm(mean=delta,sd=1,2000)
XX = LabelFnBasicDouble(RETS = x, S = S)
return(XX)
}

TRUTH = rep(1,3000)


z020 = replicate(10000,zeroCP(delta = 0.1, S = 20))
z040 = replicate(10000,zeroCP(delta = 0.1, S = 40))
z060 = replicate(10000,zeroCP(delta = 0.1, S = 60))
z100 = replicate(10000,zeroCP(delta = 0.1, S = 100))
z150 = replicate(10000,zeroCP(delta = 0.1, S = 150))

A = 1-rowMeans(z020)
B = 1-rowMeans(sweep(z020, 1, TRUTH, FUN = "=="))

plot(A,B)

M020 = 1-rowMeans(z020)
M040 = 1-rowMeans(z040)
M060 = 1-rowMeans(z060)
M100 = 1-rowMeans(z100)
M150 = 1-rowMeans(z150)


DF2 = data.frame(t = seq(1,length(M020)), M020, M040, M060, M100, M150)

#######################################################################
##### plot the results
#######################################################################

CEX = 1.0
pdf(file="Simulated_category_both.pdf", width=9, height=4)
par(mar = c(4.,4,1.5,0.8),mfrow=c(1,2),mgp = c(2.2, 0.6, 0))
plot(DF$t,DF$M020, col=4, type='l', ylim = c(0,0.52), xlim = c(-400, 400), xlab='Time from Change Point', ylab='Proportion of Miss-classifications', cex.lab = CEX, cex.axis = CEX, 
	main = expression(bold("With Change Point, " * delta == 0.1)))
lines(DF$t,DF$M040, col=5)
lines(DF$t,DF$M060, col=6) 
lines(DF$t,DF$M100, col=3) 
lines(DF$t,DF$M150, col=2) 
legend('topleft', legend= paste('s = ',c(20,40,60,100,150)),bg = "white", cex = CEX, text.col = c(4,5,6,3,2),bty = "n")
grid()
######
plot(DF2$M020, col=4, type='l', ylim = c(0.0,0.2), xlab='Time', ylab='Proportion of Miss-classifications', cex.lab = CEX, cex.axis = CEX, 
	main = expression(bold("With No Change Point, " * delta == 0.1)))
lines(DF2$M040, col=5)
lines(DF2$M060, col=6) 
lines(DF2$M100, col=3) 
lines(DF2$M150, col=2) 
legend('topleft', legend= paste('s = ',c(20,40,60,100,150)),bg = "white", cex = CEX, text.col = c(4,5,6,3,2),bty = "n")
grid()
dev.off()


