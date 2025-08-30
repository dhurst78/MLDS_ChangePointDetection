# ------------------------------------------------------------
# Visualisation of momentum weighting scheme
# - plots weights at differetn lags for the various momentum 'oscillators'
# - calculates lag of peak weight, to indicate the effective timescale over which price changes ae being calculated.
# ------------------------------------------------------------

rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_Functions.R'))
setwd(outDIR)



t = seq(0,2000) # days

ii = 6

dayf = 2^ii
days = 3*dayf


W = (1-1/days)^t - (1-1/dayf)^t 
W = W/sum(W)

MomentumWeight = function(ii=2)
{
dayf = 2^ii
days = 3*dayf


W = (1-1/days)^t - (1-1/dayf)^t 
W = W/sum(W)

return(data.frame(t,W))

}
O2 = MomentumWeight(ii=2)
O3 = MomentumWeight(ii=3)
O4 = MomentumWeight(ii=4)
O5 = MomentumWeight(ii=5)
O6 = MomentumWeight(ii=6)
O7 = MomentumWeight(ii=7)


pdf(file="Oscillator_shape.pdf", width = 9, height = 4.0)
par(mar = c(3, 4, 1.4, 0.8), mfrow=c(1,3), mgp = c(1.8, 0.6, 0))

plot(-O2$t,O2$W, type='l', xlab='days lag', ylab = 'weight', xlim = c(-800,0))
lines(-O3$t,O3$W, col=2)
lines(-O4$t,O4$W, col=3)
lines(-O5$t,O5$W, col=4)
lines(-O6$t,O6$W, col=5)
lines(-O7$t,O7$W, col=6)


legend("topleft",
       legend = c("Momentum2", "Momentum3", "Momentum4", "Momentum5", "Momentum6","Momentum7"),   text.col = c(1,2,3,4,5,6),    bty = "n")

plot(-O5$t,O6$W, type='l', xlab='days lag', ylab = 'weight', col=5)
lines(-O7$t,O7$W, col=6)

legend("topleft",
       legend = c("Momentum6","Momentum7"),   text.col = c(5,6),    bty = "n")





plot(seq(2,7),c(which.max(O2$W),which.max(O3$W),which.max(O4$W),which.max(O5$W),which.max(O6$W),which.max(O7$W)),xlab='Momentum',ylab='Lag of Peak Weight (days)', pch=16,col=4)
grid()

dev.off()
## peak weights
which.max(O2$W)
which.max(O3$W)
which.max(O4$W)
which.max(O5$W)
which.max(O6$W)
which.max(O7$W)

