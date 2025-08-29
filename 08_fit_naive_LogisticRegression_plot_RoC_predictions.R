# ------------------------------------------------------------
# Fit naive logistic regression
# - Loads S&P 500 data
# - labels trends using heuristic (s=40)
# - Calculates all 12 predictors
# - carries out logistic regression (on training set), ignoring significance of predictors
# - generates ROC curves, calculates optimum threshold
# - makes 'predictions' on training set
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########
set.seed(123) # For reproducibility
SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')

################################
#######################################################

DATA = SPdata[,c('ref_date','RetAdj')]
 
LABEL = LabelFnDouble(DF = SPdata, S = SS) # label trends

# offset by 1 day, so todays predictors are aligned with tomorrows trend
LABEL$P = shift(LABEL$P,-1)                 

DATA = merge(DATA,LABEL,by.x='ref_date',by.y = 'Date')

################### calculate momentum

for (k in 2:7) {
  DATA[[paste0("mom", k)]] <- Momentum(DATA$RetAdj, k)
}

## calculate vol and drawdown
DATA$VOL = sqrt(EWMA(DATA$RetAdj^2,100))
DATA$DD = calculate_drawdown(cumsum(DATA$RetAdj))

############# 
############# - create sine and cosine terms for seasonality
doy = yday(DATA$ref_date) # day of month
dom = mday(DATA$ref_date) # day of year

DATA$doySINE = sin(2*pi*doy/(max(doy)))
DATA$domSINE = sin(2*pi*dom/(max(dom)))
DATA$doyCOS = cos(2*pi*doy/(max(doy)))
DATA$domCOS = cos(2*pi*dom/(max(dom)))

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

smote_result <- SMOTE(X = TRAIN[,-1], target = TRAIN$P, K = 5, dup_size = 2.)
smote_result <- data.frame(smote_result$data)

table(smote_result$class) # check for post SMOTE balance

TRAIN = smote_result # balanced dataset

TRAIN$P = as.factor(TRAIN$class)
TRAIN$class = NULL

MOD = glm(P ~ 0+., data = TRAIN,  family = binomial(link = "logit"))
summary(MOD)

# make predictions on balanced train set
predictions = predict(MOD, type = "response") 
roc_obj = roc(TRAIN$P, predictions, ci=TRUE, auc=TRUE) # generate ROC
thresh = coords(roc_obj, "best", best.method = "youden")$threshold # find youden threshold


## count number of CP's in training set
G = as.numeric(as.character(TRAINorig$P))
sum(diff(G)!= 0) # 28


pdf(file="RoC_ALL.pdf", width=10, height=5)
par(mar = c(4.,4,1.5,0.8),mfrow=c(1,2),mgp = c(2.2, 0.6, 0))


plot(roc_obj,print.auc=TRUE, col=4, grid = TRUE, main='Balanced Training Dataset', print.auc.x = 0.6, print.auc.y = 0.2)
# Get Youden-optimal threshold and its Se/Sp
yb <- coords(roc_obj, "best",
             best.method = "youden",
             ret = c("threshold", "sensitivity", "specificity"),
             transpose = FALSE)

# Add the point (x = specificity, y = sensitivity)
points(yb$specificity, yb$sensitivity, pch=19, cex=1.2, col="red")

# Optional: annotate
text(yb$specificity-0.05, yb$sensitivity+0.02, labels = sprintf("Youden thresh=%.3f", yb$threshold), pos = 4, cex = 0.9)

## now do plot on unbalanced training data
predictions <- predict(MOD, type = "response", newdata = TRAINorig)
roc_obj <- roc(TRAINorig$P, predictions, ci = TRUE, auc = TRUE)


plot(roc_obj,print.auc=TRUE, col=4, grid = TRUE, main = 'Unbalanced (Original) Training Dataset', print.auc.x = 0.6, print.auc.y = 0.2)
points(yb$specificity, yb$sensitivity, pch=19, cex=1.2, col="red")

# Optional: annotate
text(yb$specificity-0.05, yb$sensitivity+0.02, labels = sprintf("Youden thresh=%.3f", yb$threshold), pos = 4, cex = 0.9)

dev.off()


isPRED = 1*(predictions>thresh)

sum(diff(isPRED)!= 0) # 207

isPred = 1*(predictions > thresh)

################################
################################

plotTraining = function(data = X, CEX = 1.)
{

  par(mfrow=c(2,1),mar = c(3, 5, 1.2, 0.2))

  plot(data$ref_date, cumsum(data$RetAdj), type='l', col=4, xlab='', ylab='Additive\nVolAdj Returns', 
  cex.lab = CEX, cex.axis = CEX, main = "Downtrends - Labelled `truth'")
    abline(v = data$ref_date[data$P == 0],col='grey89')
    lines(data$ref_date, cumsum(data$RetAdj),col=4)
    box()

  plot(data$ref_date, predictions , type='l', col=4, xlab='', ylab='Predicted Probabilites\n(Downtrends)',
      cex.lab = CEX, cex.axis = CEX, main = 'Downtrends - Model')
      abline(v = data$ref_date[isPred == 0],col='grey89')
    lines(data$ref_date, predictions ,col=4)
    box()
    abline(h = thresh,col='grey19', lty = 2)
}

pdf(file="ALL_prediction_train_timeseries.pdf",width = 10, height = 5)
plotTraining(data = TRAINorig)
dev.off()



################### different version

CEX = 1.0
pdf(file="Naive_block_labels.pdf",width = 10, height = 1.8)

par(mgp = c(0.8, 1, 0),mar = c(2, 0.2, 0.2, 0.2))#,xpd=TRUE)
plot(TRAINorig$ref_date, cumsum(TRAINorig$RetAdj),type='n',col=4,xlab='',ylab="",
ylim = c(-1,1),cex.axis = CEX, cex.lab = CEX, main='',yaxt = "n" )


x_vals = TRAINorig$ref_date[TRAINorig$P == 0]
y_starts = rep(0.02,length(x_vals))
y_ends = rep(1,length(x_vals))


segments(x0 = x_vals, y0 = y_starts,
         x1 = x_vals, y1 = y_ends,
         col = "grey89", lwd = 2, lty = 1)

x_vals = TRAINorig$ref_date[isPred == 0]
y_starts = rep(-0.02,length(x_vals))
y_ends = rep(-1,length(x_vals))

segments(x0 = x_vals, y0 = y_starts,
         x1 = x_vals, y1 = y_ends,
         col = "grey89", lwd = 2, lty = 1)

legend("topright", legend = c("Up-trends", "Down-trends"), col = c(NA, NA), fill = c(0, 'grey89'), bg = "white", ncol=2, y.intersp = 0.8)

text(min(TRAINorig$ref_date),-0.5,'prediction', pos=4, cex=1.2)
text(min(TRAINorig$ref_date),+0.5,"'True' Label", pos=4, cex=1.2)

abline(h = 0,lty=1,col=1)

dev.off()


#######################
#### cover metric

true_cpts = which(diff(as.numeric(as.character(TRAINorig$P))) != 0)
predicted_cpts = which(diff(isPred) != 0)

covering_from_cpts(true_cpts, predicted_cpts, nrow(TRAINorig)) # 0.48
