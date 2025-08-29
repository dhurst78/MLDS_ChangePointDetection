# ------------------------------------------------------------
# Draw Illustrative Plots of Labelling Heuristics run on a simulated Single changepoint
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########
set.seed(123) # set seed for reproducability

delta = 0.1 # parameter required for simulated data (the mean changes from +delta to -delta at the changepoint)

# generate simulated data with 1 changepoint
#---------
# concatonate series of 'returns' with mean +delta and -delta
y = c(rnorm(mean = delta, sd = 1, 500), rnorm(mean = -delta, sd=1, 500))

x = seq(-500,499) # create an index for plotting purposes, which is 0 at the CP
# run one-step heuristic over
#XX = LabelFnBasic(RETS = y, S = 40)    ##


# function to do one and two step heuristic ()
LabelFn = function(RETS, S = 75)
{
    Price = cumsum(RETS)
 
    F = EWMA(Price ,S)
    B = rev(EWMA(rev(Price ),S))
    X1 = F-B
### now do second pass
    FF = EWMA(X1, S)
    BB = rev(EWMA(rev(X1), S))
    X = rowMeans(cbind(BB, FF)) # pair-wise means of EWMA's

    P = 1*(F<B)
    out = data.frame(RETS,F,B,P,FF,BB,X, X1)
    return(out)
}

## apply labelling function to data-set
E = LabelFn(RETS = y, S = SS)


###### draw illustrative plot of single changepoint

CEX = 1.1

pdf(file="simulated_examples.pdf", width=10, height=4.5)
par(mar = c(4.,4,1.5,0.8),mfrow=c(1,2),mgp = c(2.2, 0.6, 0))

plot(x,cumsum(E$RETS),ylab='Price',xlab='Time from Changepoint',type='n', main = 'One Step Heuristic')
lines(x,E$F,col=2, lwd=2)
lines(x,E$B,col=3, lwd=2)
lines(x,cumsum(E$RETS),col=4, lwd=2)
grid()
box()
abline(v = x[which(E$F>E$B)[1]],col=8, lwd=2)
legend('bottomright', legend = c('EWMA (forwards)',"'Price'",'EWMA (backwards)','Detected Changepoint'),bg = "white", cex = 1, text.col = c(2,4,3,8),bty = "n")

############

plot(x,E$X1,ylab='Price',xlab='Time from Changepoint', type='l', lwd=2, main = 'Two Step Heuristic')
lines(x,E$FF,col=2, lwd=2)
lines(x,E$BB,col=3, lwd=2)
lines(x,E$X,col=4, lwd=2)
grid()
box()
abline(v = x[which(E$P==0)[1]],col=8, lwd=2)
legend('topleft', legend = c('Price EWMA Difference','EWMA (forwards)','EWMA (backwards)','EWMA (Two-sided)','Detected Changepoint'),bg = "white", cex = 1, text.col = c(1,2,3,4,8),bty = "n")

dev.off()

