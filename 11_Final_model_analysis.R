# ------------------------------------------------------------
# Detailed analysis of final model
# - have settled on logistic regression with 2 predictors (momentum 6 & 7)
# - carry out detailed analysis of it's performance on training and TEST datasets
# - Carry out backtest of a trading system based on it's predictions
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########
SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')

#######################################################

DATA = SPdata[,c('ref_date','RetAdj')]
 
LABEL = LabelFnDouble(DF = SPdata, S = SS) # label data

# offset by 1 day, so todays predictors are aligned with tomorrows trend
LABEL$P = shift(LABEL$P,-1)
tail(DATA)

DATA = merge(DATA,LABEL,by.x='ref_date',by.y = 'Date')

head(DATA)

################### calc momentum

DATA$mom6 = Momentum(DATA$RetAdj,6)
DATA$mom7 = Momentum(DATA$RetAdj,7)

head(DATA)

############################################################################################
############################################################################################
TRAIN = na.omit(DATA)
TESTorig = TRAIN[TRAIN$ref_date > DateCUTOFF,]
TRAIN = TRAIN[TRAIN$ref_date < DateCUTOFF,] # only look at early dates
TRAINorig = TRAIN # take a copy
## remove columns that we don't want to use as predictors
TRAIN$ref_date = NULL
TRAIN$RetAdj = NULL

mean(TRAIN$P) # balance check
table(TRAIN$P)

# Apply SMOTE
set.seed(123) # For reproducibility
smote_result <- SMOTE(X = TRAIN[,-1], target = TRAIN$P, K = 5, dup_size = 2.)
smote_result <- data.frame(smote_result$data)

table(smote_result$class) # check for post SMOTE balance


head(smote_result)

TRAIN = smote_result

str(TRAIN)


TRAINorig$P = as.factor(TRAINorig$P)
TRAIN$P = as.factor(TRAIN$class)
TRAIN$class = NULL

MOD = glm(P ~ 0+., data = TRAIN,  family = binomial(link = "logit"))
summary(MOD)


trainPREDbalanced <- predict(MOD, type = "response")
trainROCobj <- roc(TRAIN$P, trainPREDbalanced, ci = TRUE, auc = TRUE)

# determine threshold on balanced fit
thresh = coords(trainROCobj, "best", best.method = "youden")$threshold

# make continuous prediction on unbalanced training data
trainPRED = predict(MOD, type = "response", newdata = TRAINorig)

# make prediction binary using threshold
trainPREDbinary = 1*(trainPRED> thresh)




graphics.off()


plotFn = function(data = X, CEX = 1.)
{

  par(mfrow=c(2,1),mar = c(3, 5, 1.2, 0.2))

  plot(data$ref_date, cumsum(data$RetAdj), type='l', col=4, xlab='', ylab='Additive\nVolAdj Returns', 
  cex.lab = CEX, cex.axis = CEX, main = "Downtrends - Labelled `truth'")
    abline(v = data$ref_date[data$P == 0],col='grey89')
    lines(data$ref_date, cumsum(data$RetAdj),col=4)
    box()

  plot(data$ref_date, trainPRED , type='l', col=4, xlab='', ylab='Predicted Probabilites\n(Downtrends)',
      cex.lab = CEX, cex.axis = CEX, main = 'Downtrends - Model Predictions')
      abline(v = data$ref_date[trainPREDbinary == 0],col='grey89')
    lines(data$ref_date, trainPRED ,col=4)
    box()
    abline(h = thresh,col='grey19', lty = 2)
}

pdf(file="TrainingPrediction.pdf",width = 10, height = 5)
plotFn(data = TRAINorig)
dev.off()


#####################################

# ARL1 calculation
calculate_arl1 <- function(true_cpts, predicted_cpts, burn_in = 0) {
  delays <- c()
  for (i in seq_along(true_cpts)) {
    # Define valid window after burn-in
    start <- true_cpts[i] + burn_in
    end <- if (i < length(true_cpts)) true_cpts[i + 1] else Inf

    # Find first prediction after burn-in and before next true change
    detection <- predicted_cpts[predicted_cpts > start & predicted_cpts < end]

    if (length(detection) > 0) {
      delays <- c(delays, detection[1] - true_cpts[i])
    }
  }
  mean(delays)
}


true_cpts_TRAIN = which(diff(as.numeric(as.character(TRAINorig$P))) != 0)
predicted_cpts_TRAIN = which(diff(trainPREDbinary) != 0)

calculate_arl1(true_cpts_TRAIN, predicted_cpts_TRAIN, burn_in = 10) # length of time after a change until detection



CM = table(TRAINorig$P,trainPREDbinary)
Accuracy = round(sum(diag(CM))/sum(CM),2)
Accuracy 


G = as.numeric(as.character(TRAINorig$P))
sum(diff(G)!= 0) # 28
sum(diff(trainPREDbinary)!= 0) # 36


CEX = 1.1
data = TRAINorig

plot(data$ref_date, cumsum(data$RetAdj), type='l', col=4, xlab='', ylab='Additive VolAdj Returns', 
cex.lab = CEX, cex.axis = CEX, main = "Downtrends - Labelled `truth'")
abline(v = data$ref_date[trainPREDbinary== 0],col='grey89')
lines(data$ref_date, cumsum(data$RetAdj),col=4)
lines(data$ref_date, cumsum(trainPREDbinary*data$RetAdj),col=5)
box()


RET = trainPREDbinary*data$RetAdj

RelRet = RET-data$RetAdj

OP = round(mean(RelRet)*252, 1)


CEX = 1.0
###
pdf(file="TRAINING_illustrative_AdjRet_PL.pdf", width=10, height=2.5)
par(mar = c(3, 5, .2, 0.2))
plot(data$ref_date, cumsum(data$RetAdj),type='l',col=4,xlab='',ylab='Additive\nVolAdj Returns)',
ylim = c(-50,450),cex.axis = CEX, cex.lab = CEX)

abline(v = data$ref_date[trainPREDbinary == 0],col='grey89')
lines(data$ref_date, cumsum(data$RetAdj),col=1)

lines(data$ref_date, cumsum(trainPREDbinary*data$RetAdj),col=4)
legend('topleft', c('SP500 Avoiding Predicted Down Trends','SP500'),bg = "white", text.col = c(4,1), cex = CEX)
legend('bottomright', paste('Out Performance (annualised):',OP,' Vol Adj Rets'),bg = "white", cex = CEX)
box()

dev.off()


###### calc Covering Metric

LEN = nrow(data)
X = as.numeric(as.character(data$P))
true_cpts = as.vector(which(abs(X-shift(X))==1))
det_cpts = as.vector(which(abs(trainPREDbinary-shift(trainPREDbinary))==1))
###

covering_from_cpts(true_cpts, det_cpts, LEN) # 0.51





########### - generate 'block plot' of predictions and 'true' labels


CEX=1.0
pdf(file="Naive_block_labels_Training.pdf",width = 10, height = 1.8)

par(mgp = c(0.8, 1, 0),mar = c(2, 0.2, 0.2, 0.2))
plot(TRAINorig$ref_date, cumsum(TRAINorig$RetAdj),type='n',col=4,xlab='',ylab="",
ylim = c(-1,1),cex.axis = CEX, cex.lab = CEX, main='',yaxt = "n" )


x_vals = TRAINorig$ref_date[TRAINorig$P == 0]
y_starts = rep(0.02,length(x_vals))
y_ends = rep(1,length(x_vals))


segments(x0 = x_vals, y0 = y_starts,
         x1 = x_vals, y1 = y_ends,
         col = "grey89", lwd = 2, lty = 1)

x_vals = TRAINorig$ref_date[trainPREDbinary== 0]
y_starts = rep(-0.02,length(x_vals))
y_ends = rep(-1,length(x_vals))

segments(x0 = x_vals, y0 = y_starts,
         x1 = x_vals, y1 = y_ends,
         col = "grey89", lwd = 2, lty = 1)
abline(h = 0,lty=1,col=1)
legend("topright", legend = c("Up-trends", "Down-trends"), col = c(NA, NA), fill = c(0, 'grey89'), bg = "white", ncol=2, y.intersp = 0.8)#, inset=c(-0.2, 0))

text(min(TRAINorig$ref_date),-0.5,'prediction', pos=4, cex=1.2)
text(min(TRAINorig$ref_date),+0.5,"'True' Label", pos=4, cex=1.2)



dev.off()






##########################################################
##########################################################
########################## summary stats of training set
##########################################################
##########################################################

auc(trainROCobj)
calculate_arl1(true_cpts_TRAIN, predicted_cpts_TRAIN, burn_in = 5)
Accuracy 
covering_from_cpts(true_cpts_TRAIN, predicted_cpts_TRAIN, LEN) # 0.51
OP


###############################################################################################################
################################################# now do test analysis
###############################################################################################################


testPRED  = predict(MOD, type = "response", newdata = TESTorig)
testPREDbinary = 1*(testPRED > thresh)

testROCobj <- roc(TESTorig$P, testPREDbinary, ci = TRUE, auc = TRUE)


plotTest = function(data = X, CEX = 1.0)
{
  par(mfrow=c(2,1),mar = c(3, 5, 1.2, 0.2))

  plot(data$ref_date, cumsum(data$RetAdj), type='l', col=4, xlab='', ylab='Additive\nVolAdj Returns', 
  cex.lab = CEX, cex.axis = CEX, main = "Downtrends - Labelled `truth'")
    abline(v = data$ref_date[data$P == 0],col='grey89')
    lines(data$ref_date, cumsum(data$RetAdj),col=4)
    box()

  plot(data$ref_date, testPRED, type='l', col=4, xlab='', ylab='Predicted Probabilites\n(Downtrends)',
      cex.lab = CEX, cex.axis = CEX, main = 'Downtrends - Model Predictions')
      abline(v = data$ref_date[testPREDbinary== 0],col='grey89')
    lines(data$ref_date, testPRED, col=4)
    box()
    abline(h = thresh,col='grey19', lty = 2)

}


pdf(file="TestPrediction.pdf",width = 10, height = 5)
plotTest(data = TESTorig)
dev.off()

#######################

CM = table(TESTorig$P, testPREDbinary)
Accuracy = round(sum(diag(CM))/sum(CM),2)
Accuracy


G = as.numeric(as.character(TESTorig$P))
sum(diff(G)!= 0) # 14

sum(diff(testPREDbinary)!= 0) # 13

length(testPREDbinary)
length(data$RetAdj)
##################################

data = TESTorig

RET = testPREDbinary*data$RetAdj

RelRet = RET-data$RetAdj

OP = round(mean(RelRet)*252,1)

CEX = 1.1
###


pdf(file="TESTING_illustrative_AdjRet_PL.pdf", width=10, height=2.5)
par(mar = c(3, 5, .2, 0.2))
plot(data$ref_date, cumsum(data$RetAdj),type='l',col=4,xlab='',ylab='Additive\nVolAdj Returns)',
ylim = c(-50,200),cex.axis = CEX, cex.lab = CEX)

abline(v = data$ref_date[testPREDbinary== 0],col='grey89')
lines(data$ref_date, cumsum(data$RetAdj),col=1)

lines(data$ref_date, cumsum(testPREDbinary*data$RetAdj),col=4)
legend('topleft', c('SP500 Avoiding Predicted Down Trends','SP500'),bg = "white", text.col = c(4,1), cex = CEX)
legend('bottomright', paste('Out Performance (annualised):',OP,' Vol Adj Rets'),bg = "white", cex = CEX)
box()

dev.off()




########### - generate 'block plot' of predictions and 'true' labels


CEX=1.0
pdf(file="Naive_block_labels_Test.pdf",width = 10, height = 1.8)

par(mgp = c(0.8, 1, 0),mar = c(2, 0.2, 0.2, 0.2))
plot(TESTorig$ref_date, cumsum(TESTorig$RetAdj),type='n',col=4,xlab='',ylab="",
ylim = c(-1,1),cex.axis = CEX, cex.lab = CEX, main='',yaxt = "n" )



x_vals = TESTorig$ref_date[TESTorig$P == 0]
y_starts = rep(0.02,length(x_vals))
y_ends = rep(1,length(x_vals))


segments(x0 = x_vals, y0 = y_starts,
         x1 = x_vals, y1 = y_ends,
         col = "grey89", lwd = 2, lty = 1)

x_vals = TESTorig$ref_date[testPREDbinary== 0]
y_starts = rep(-0.02,length(x_vals))
y_ends = rep(-1,length(x_vals))

segments(x0 = x_vals, y0 = y_starts,
         x1 = x_vals, y1 = y_ends,
         col = "grey89", lwd = 2, lty = 1)
abline(h = 0,lty=1,col=1)
legend("topright", legend = c("Up-trends", "Down-trends"), col = c(NA, NA), fill = c(0, 'grey89'), bg = "white", ncol=2, y.intersp = 0.8)#, inset=c(-0.2, 0))

text(min(TESTorig$ref_date),-0.5,'prediction', pos=4, cex=1.2)
text(min(TESTorig$ref_date),+0.5,"'True' Label", pos=4, cex=1.2)

dev.off()




###### calc Covering Metric

LEN = nrow(data)
LEN
X = as.numeric(as.character(data$P))
true_cpts_TEST  = as.vector(which(abs(X-shift(X))==1))
predicted_cpts_TEST  = as.vector(which(abs(testPREDbinary -shift(testPREDbinary))==1))
###

covering_from_cpts(true_cpts_TEST, predicted_cpts_TEST, LEN) # 0.64



##### compare predictions to real at the start of uptrends and the start of downtrends

F = data.frame(date = data$ref_date,true = data$P,pred = testPREDbinary )

DFend = data.frame(true = which(diff(F$true)==1)[1:6],pred = which(diff(F$pred ) == 1))
DFend 

mean(DFend$pred-DFend$true)

DFstart = data.frame(true = which(diff(F$true)==-1),pred = which(diff(F$pred ) == -1))
DFstart 
mean(DFstart$pred-DFstart$true)

##########################################################
##########################################################
########################## summary stats of training set
##########################################################
##########################################################

auc(testROCobj)
calculate_arl1(true_cpts_TEST, predicted_cpts_TEST, burn_in = 5)
Accuracy 
covering_from_cpts(true_cpts_TEST, predicted_cpts_TEST, LEN) # 0.51
OP


