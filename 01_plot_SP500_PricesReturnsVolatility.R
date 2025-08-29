# ------------------------------------------------------------
# Visualise SP500 data
# - Load S&P 500 data
# - Calculate and plot: additive returns, returns, volatility, VolAdjReturns
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########

SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')

plot(SPdata$ref_date,SPdata$addPrices,type='l',col=4,xlab='',ylab='P & L (additive returns)',ylim = c(0,13))

#########

pdf(file="0_SP_raw.pdf",width=9, height=5)
plotRaw(SPdata, CEX = 1.4)
dev.off()

pdf(file="0_SP_adj.pdf",width=9, height=5)
plotAdj(SPdata, CEX = 1.4)
dev.off()


nrow(SPdata)
head(SPdata)

range(SPdata$ref_date)
