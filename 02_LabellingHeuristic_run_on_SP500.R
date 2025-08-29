# ------------------------------------------------------------
# Apply Labelling Heuristic(s) to SP500 data
# used to select smoothing parameter
# - Load S&P 500 data
# - Definite labelling Heuristics (two versions of)
# - apply them to S&P500 data
# - record the resulting max/min/mean lenghts of trend periods
# - repeat for various smooth (s) values
# - plot results
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########

SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')
#########
S_values <- seq(5, 200) # define values of s over which we will loop

############################# definine initial 'one step' heuristic

LabelFn_OneStep = function(DFF = SPdata, SS = 50, RetType = 'Adj')
{
XX = LabelFn(DF = DFF, S = SS, retType = RetType)
y = LabelRuns(XX$P)
s = data.frame(mean = mean(y$length), min = min(y$length), max = max(y$length))
s$S = SS
return(s)
}

############################# definine the 'two step' heuristic
LabelFn_TwoStep = function(DFF = SPdata, SS = 50, RetType = 'Adj')
{
XX = LabelFnDouble(DF = DFF, S = SS, retType = RetType)
y = LabelRuns(XX$P)
s = data.frame(mean = mean(y$length), min = min(y$length), max = max(y$length))
s$S = SS
return(s)
}

## apply these functions each to: SP500 returns, and voladjusted returns

temp_list <- lapply(S_values, function(s) LabelFn_OneStep(DF = SPdata, SS = s, RetType = 'Adj'))
results_Adj_OneStep <- data.frame(do.call(rbind, temp_list))

temp_list <- lapply(S_values, function(s) LabelFn_OneStep(DF = SPdata, SS = s, RetType = 'Raw'))
results_Raw_OneStep <- data.frame(do.call(rbind, temp_list))

temp_list <- lapply(S_values, function(s) LabelFn_TwoStep(DF = SPdata, SS = s, RetType = 'Adj'))
results_Adj_TwoStep <- data.frame(do.call(rbind, temp_list))

temp_list <- lapply(S_values, function(s) LabelFn_TwoStep(DF = SPdata, SS = s, RetType = 'Raw'))
results_Raw_TwoStep <- data.frame(do.call(rbind, temp_list))

#########
###########################################
##### plot
###########################################


CEX = 1.02
pdf(file="1_NoChangePointsDouble.pdf", width=10, height=5)
par(mar = c(4.,4,1.5,0.8),mfrow=c(1,2),mgp = c(2.2, 0.6, 0))

plot(results_Raw_OneStep$S,results_Raw_OneStep$max ,ylim = c(1,5000),xlab='EWMA smooth (s)', ylab = 'Length (business days)',
	cex.axis = CEX, cex.lab = CEX, cex.main=CEX, log='xy', type='l', main='EWMA Heuristic - One Step', col = 2, lty = 2)
lines(results_Raw_OneStep$S,results_Raw_OneStep$mean,col=2,lty=1)
lines(results_Raw_OneStep$S,results_Raw_OneStep$min,col=2,lty=3)
lines(results_Adj_OneStep$S,results_Adj_OneStep$max,col=1,lty=2)
lines(results_Adj_OneStep$S,results_Adj_OneStep$mean,col=1,lty=1)
lines(results_Adj_OneStep$S,results_Adj_OneStep$min,col=1,lty=3)
legend('bottomleft', legend=c('Max', 'Mean', 'Min'),bg = "white", cex = CEX, lty = c(2,1,3))
legend('topleft', legend=c('Raw Returns','Vol Adj Returns'),bg = "white", cex = CEX, text.col = c(2,1))
grid()

######
plot(results_Raw_TwoStep$S,results_Raw_TwoStep$max ,ylim = c(1,5000),xlab='EWMA smooth (s)', ylab = 'Length (business days)',
	cex.axis = CEX, cex.lab = CEX, cex.main=CEX, log='xy', type='l', main='EWMA Heuristic - Two Step', col = 2, lty = 2)
lines(results_Raw_TwoStep$S,results_Raw_TwoStep$mean,col=2,lty=1)
lines(results_Raw_TwoStep$S,results_Raw_TwoStep$min,col=2,lty=3)
lines(results_Adj_TwoStep$S,results_Adj_TwoStep$max,col=1,lty=2)
lines(results_Adj_TwoStep$S,results_Adj_TwoStep$mean,col=1,lty=1)
lines(results_Adj_TwoStep$S,results_Adj_TwoStep$min,col=1,lty=3)
legend('bottomright', legend=c('Max', 'Mean', 'Min'),bg = "white", cex = CEX, lty = c(2,1,3))
legend('topleft', legend=c('Raw Returns','Vol Adj Returns'),bg = "white", cex = CEX, text.col = c(2,1))
grid()
dev.off()


##########################################################
### show impact of this on a backtest P&L

P = LabelFnDouble(DF = SPdata, S = SS, retType = 'Adj') # label data for s = 40

M = merge(P,SPdata, by.x='Date',by.y='ref_date')          # merge lables with SP500 data

RET = M$P*M$RetAdj						    # trading systems returns (positions are 0 when in downtrend)

RelRet = RET-M$RetAdj						    # returns relative to SP500

OP = round(mean(RelRet)*252,1)				    # average annulaised outperformance

CPno = sum(diff(M$P)!=0)					    # count of change points

######### - plot illustrative backtest
CEX = 1.1
###
pdf(file="2_illustrative_AdjRet_PL.pdf",width=10, height=3.33)
par(mar = c(2,4.5,0.1,0.1))
plot(M$Date,cumsum(M$RetAdj),type='l',col=4,xlab='',ylab='P & L (additive Vol Adj Returns)',
ylim = c(-50,1200),cex.axis = CEX, cex.lab = CEX)

abline(v = M$Date[M$P==0],col='grey89')
lines(M$Date,cumsum(M$RetAdj),col=1)

lines(M$Date,cumsum(RET),col=4)
legend('topleft', c('SP500 Avoiding Down Trends','SP500'),bg = "white", text.col = c(4,1), cex = CEX)
legend('top', paste(CPno, 'Change points established'),bg = "white", cex = CEX)
legend('bottomright', paste('Out Performance (annualised):',OP,' Vol Adj Rets'),bg = "white", cex = CEX)
box()
dev.off()


##################### - means of SP500 rets in up/down trends
##################### 
mean(M$RetAdj[M$P==1])
mean(M$RetAdj[M$P==0])

