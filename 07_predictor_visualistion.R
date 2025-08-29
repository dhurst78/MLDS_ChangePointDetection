# ------------------------------------------------------------
# Momentum (K = 2..7) and Drawdown plots for S&P 500
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########

SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')


###### plot momentum measures
###########################################
#########

DD = calculate_drawdown(cumsum(SPdata$RetAdj))

#################
# price difference momentum

Ks  <- 2:7
cols <- seq_along(Ks)  # 1..6

CEX = 1.0
###
pdf(file="Momentum_and_DD.pdf",width = 8.5, height=4.)
par(mar = c(1.9, 4, 0.2, 0.2),mfrow=c(2,1),mgp = c(2.2, 0.6, 0))

plot(SPdata$ref_date,  Momentum(K = 2),type='n',col=1,xlab='',ylab='Momentum', cex.lab = CEX, cex.axis = CEX, ylim = c(-20,32))
for (i in 1:length(Ks)) {
  lines(SPdata$ref_date, Momentum(K = Ks[i]), col = cols[i])
}
abline(h = 0, lty=2, col='grey69')
legend('bottomright', legend=paste('K:',c(2,3,4,5,6,7)),bg = "white", cex = CEX, text.col = seq(1,6), bty = "n", ncol = 6)
#################
plot(SPdata$ref_date,DD,type='l',xlab='',ylab='Drawdown', cex.lab = CEX, cex.axis = CEX, col=4)

dev.off()
