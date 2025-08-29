# ------------------------------------------------------------
# - Loads S&P 500 data
# - labels trends using heuristic (s=40)
# - Calculates all 12 predictors
# - fits a NN model
# - makes predictions on train and test data
# - count the number of predicted change-points in each dataset
# - repeat N (50) times and plot distributions of CP count
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########


SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')

nrow(SPdata)

sd(SPdata$ret_adjusted_prices)/mean(SPdata$ret_adjusted_prices)

head(SPdata)

plot(SPdata$ref_date,SPdata$addPrices,type='l',col=4,xlab='',ylab='P & L (additive returns)',ylim = c(0,13))
################################
LabelFn 

######################################################

DATA = SPdata[,c('ref_date','RetAdj')]


LABEL = LabelFnDouble(DF = SPdata, S = SS) # HERE SPECIFY SMOOTH


LABEL$P = shift(LABEL$P,-1)
tail(DATA)

DATA = merge(DATA,LABEL,by.x='ref_date',by.y = 'Date')

head(DATA)

################### calc momentum

DATA$mom2 = Momentum(DATA$RetAdj,2)
DATA$mom3 = Momentum(DATA$RetAdj,3)
DATA$mom4 = Momentum(DATA$RetAdj,4)
DATA$mom5 = Momentum(DATA$RetAdj,5)
DATA$mom6 = Momentum(DATA$RetAdj,6)
DATA$mom7 = Momentum(DATA$RetAdj,7)

DATA$VOL  = sqrt(EWMA(DATA$RetAdj^2,100))

DATA$DD = calculate_drawdown(cumsum(DATA$RetAdj))

head(DATA)

############# 
############# - create sine and cosine terms for seasonality

doy = yday(DATA$ref_date) # day of month
dom = mday(DATA$ref_date) # day of year

DATA$doySINE = sin(2*pi*doy/(max(doy)))
DATA$domSINE = sin(2*pi*dom/(max(dom)))
DATA$doyCOS = cos(2*pi*doy/(max(doy)))
DATA$domCOS = cos(2*pi*dom/(max(dom)))


plot(DATA$ref_date[1:500],DATA$domSINE[1:500],type='l')
plot(DATA$ref_date[1:500],DATA$doySINE[1:500],type='l')

############################################################################################
############################################################################################
TRAIN = na.omit(DATA)
TESTorig = TRAIN[TRAIN$ref_date > DateCUTOFF,]
TRAINorig = TRAIN[TRAIN$ref_date < DateCUTOFF,] # only look at early dates
TRAIN = TRAINorig # take a copy
TEST = TESTorig

## remove columns that we don't want to use as predictors
TRAIN$ref_date = NULL
TRAIN$RetAdj = NULL
TEST$ref_date = NULL
TEST$RetAdj = NULL


mean(TRAIN$P) # balance check
table(TRAIN$P)

# Convert to a factor
TRAIN$P = as.factor(TRAIN$P)
TEST$P = as.factor(TEST$P)

# don't apply SMOTE, NN function can balance the dataset

##################
################## - now do deep learning
##################
head(TRAIN)
##################
# Install and load h2o package
# install.packages("h2o")


# Start H2O
h2o.init()

# --- Convert to H2O frames ---
train_h2o <- as.h2o(TRAIN)
test_h2o <- as.h2o(TEST)

# --- Specify target and predictors ---
target <- "P"
predictors <- setdiff(names(TRAIN), target)

# --- Ensure target is a factor (for classification) ---
train_h2o[[target]] <- as.factor(train_h2o[[target]])
test_h2o[[target]] <- as.factor(test_h2o[[target]])




N = 50

TRAINcount = rep(NA,N)
TESTcount = rep(NA,N)

for (i in 1:N)
{


# --- Train deep learning model ---
dl_model <- h2o.deeplearning(
  x = predictors,
  y = target,
  training_frame = train_h2o,
  validation_frame = test_h2o,
  activation = "RectifierWithDropout",         # ReLu with dropout
  #activation = "TanhWithDropout", 
  hidden = c(64, 32, 16, 4),              # model architecture
  input_dropout_ratio = 0.0,
  hidden_dropout_ratios = c(0.2, 0.2, 0.2, 0.2),
  balance_classes = TRUE,                 # address class imbalance
  epochs = 1000,                           # max no training cycles
  stopping_metric = "AUC",             # training stopping logic
  stopping_rounds = 5,
  stopping_tolerance = 0.01,
  score_each_iteration = TRUE,
  seed = 1234
)



# Function to calculate number of parameters in a NN
param_count <- function(input_size, layer_sizes, output_size = 1) {
  # Combine all layers including output
  sizes <- c(input_size, layer_sizes, output_size)
  
  # Calculate weights and biases for each connection
  weights <- sum(sapply(1:(length(sizes)-1), function(i) sizes[i] * sizes[i+1]))
  biases  <- sum(sapply(2:length(sizes), function(i) sizes[i]))
  
  total_params <- weights + biases
  return(list(weights = weights, biases = biases, total = total_params))
}

param_count(input_size = 11, layer_sizes = c(64, 32, 16, 8))

##########
# --- Evaluate on TEST set ---
TESTperf = h2o.performance(dl_model, valid = TRUE)

TRAINperf = h2o.performance(dl_model, valid = FALSE)

h2o.auc(TESTperf)   # 0.825
h2o.auc(TRAINperf)	# 0.863

#################### - extract predictions
#################### TRAINING

train_pred_h2o <- h2o.predict(dl_model, train_h2o)
train_scored_h2o <- h2o.cbind(train_h2o[target], train_pred_h2o)


trainPRED = as.numeric(as.vector(train_scored_h2o['predict']))

TRAINcount[i] = sum(diff(trainPRED)!=0) #  130

test_pred_h2o <- h2o.predict(dl_model, test_h2o)
test_scored_h2o <- h2o.cbind(test_h2o[target], test_pred_h2o)

testPRED = as.numeric(as.vector(test_scored_h2o['predict']))

TESTcount[i] = sum(diff(testPRED)!=0) # 54

}

trainREAL = as.numeric(as.vector(train_scored_h2o['P']))




graphics.off()


hist(TRAINcount,xlim = c(0,max(TRAINcount)), main='')
abline(v = sum(diff(trainREAL)!=0),lty=2,col=2)

testREAL = as.numeric(as.vector(test_scored_h2o['P']))

windows()
hist(TESTcount,xlim = c(0,max(TESTcount)), main='')
abline(v = sum(diff(testREAL)!=0),lty=2,col=2)


#####################################################################
#####################################################################
############## - now try and repeat with just a subset of predictors
#####################################################################
#####################################################################

DATA$mom2 = NULL
DATA$mom3 = NULL
DATA$mom4 = NULL
DATA$mom5 = NULL
DATA$DD = NULL
DATA$VOL = NULL
DATA$doySINE = NULL
DATA$doyCOS = NULL
DATA$domSINE = NULL
DATA$domCOS = NULL

###########################################################################################
############################################################################################
TRAIN = na.omit(DATA)
TESTorig = TRAIN[TRAIN$ref_date > DateCUTOFF,]
TRAINorig = TRAIN[TRAIN$ref_date < DateCUTOFF,] # only look at early dates
TRAIN = TRAINorig # take a copy
TEST = TESTorig

## remove columns that we don't want to use as predictors
TRAIN$ref_date = NULL
TRAIN$RetAdj = NULL
TEST$ref_date = NULL
TEST$RetAdj = NULL

mean(TRAIN$P) # balance check
table(TRAIN$P)

# Convert to a factor
TRAIN$P = as.factor(TRAIN$P)
TEST$P = as.factor(TEST$P)

# don't apply SMOTE, NN function can balance the dataset

##################
################## - now do deep learning
##################
head(TRAIN)
##################

# Start H2O
h2o.init()

# --- Convert to H2O frames ---
train_h2o <- as.h2o(TRAIN)
test_h2o <- as.h2o(TEST)

# --- Specify target and predictors ---
target <- "P"
predictors <- setdiff(names(TRAIN), target)

# --- Ensure target is a factor (for classification) ---
train_h2o[[target]] <- as.factor(train_h2o[[target]])
test_h2o[[target]] <- as.factor(test_h2o[[target]])



TRAINcount2 = rep(NA,N)
TESTcount2 = rep(NA,N)

for (i in 1:N)
{


# --- Train deep learning model ---
dl_model <- h2o.deeplearning(
  x = predictors,
  y = target,
  training_frame = train_h2o,
  validation_frame = test_h2o,
  activation = "RectifierWithDropout",         # ReLu with dropout
  #activation = "TanhWithDropout", 
  hidden = c(64, 32, 16, 4),              # model architecture
  input_dropout_ratio = 0.0,
  hidden_dropout_ratios = c(0.2, 0.2, 0.2, 0.2),
  balance_classes = TRUE,                 # address class imbalance
  epochs = 1000,                           # max no training cycles
  stopping_metric = "AUC",             # training stopping logic
  stopping_rounds = 5,
  stopping_tolerance = 0.01,
  score_each_iteration = TRUE,
  seed = 1234
)



# Function to calculate number of parameters in a NN
param_count <- function(input_size, layer_sizes, output_size = 1) {
  # Combine all layers including output
  sizes <- c(input_size, layer_sizes, output_size)
  
  # Calculate weights and biases for each connection
  weights <- sum(sapply(1:(length(sizes)-1), function(i) sizes[i] * sizes[i+1]))
  biases  <- sum(sapply(2:length(sizes), function(i) sizes[i]))
  
  total_params <- weights + biases
  return(list(weights = weights, biases = biases, total = total_params))
}

param_count(input_size = 11, layer_sizes = c(64, 32, 16, 8))

##########
# --- Evaluate on TEST set ---
TESTperf = h2o.performance(dl_model, valid = TRUE)

TRAINperf = h2o.performance(dl_model, valid = FALSE)

h2o.auc(TESTperf)   # 0.825
h2o.auc(TRAINperf)	# 0.863

#################### - extract predictions
#################### TRAINING

train_pred_h2o <- h2o.predict(dl_model, train_h2o)
train_scored_h2o <- h2o.cbind(train_h2o[target], train_pred_h2o)


trainPRED = as.numeric(as.vector(train_scored_h2o['predict']))

TRAINcount2[i] = sum(diff(trainPRED)!=0) #  130

test_pred_h2o <- h2o.predict(dl_model, test_h2o)
test_scored_h2o <- h2o.cbind(test_h2o[target], test_pred_h2o)

testPRED = as.numeric(as.vector(test_scored_h2o['predict']))



TESTcount2[i] = sum(diff(testPRED)!=0) # 54


}

trainREAL2 = as.numeric(as.vector(train_scored_h2o['P']))





hist(TRAINcount2,xlim = c(0,max(TRAINcount2)), main='')
abline(v = sum(diff(trainREAL2)!=0),lty=2,col=2)

testREAL2 = as.numeric(as.vector(test_scored_h2o['P']))

windows()
hist(TESTcount2,xlim = c(0,max(TESTcount2)), main='')
abline(v = sum(diff(testREAL2)!=0),lty=2,col=2)


###############

CEX = 1.1

pdf(file="DL_hist_CountNo.pdf", width=10, height=4.5)
par(mar = c(4.,4,1.5,0.8),mfrow=c(1,2),mgp = c(2.2, 0.6, 0))


###########
# Convert trainREAL2 and testREAL2 to numbers for vertical lines
trainREAL_num <- sum(diff(trainREAL) != 0)
testREAL_num <- sum(diff(testREAL) != 0)

# Create hist objects without plotting
h1 <- hist(TRAINcount, plot = FALSE, breaks = sqrt(N))  # training
h2 <- hist(TESTcount, plot = FALSE, breaks = sqrt(N))   # test

# Plot first histogram
plot(h1, col = rgb(1, 0, 0, 0.5), xlim = c(0, max(c(TRAINcount, TESTcount))), ylim = c(0,20),
     main = "Predictands: All", xlab = "No of Change-points")

# Overlay second histogram
plot(h2, col = rgb(0, 0, 1, 0.5), add = TRUE)

# Add vertical lines
abline(v = trainREAL_num, lty = 2, col = "red", lwd=2)
abline(v = testREAL_num, lty = 2, col = "blue", lwd=2)


# Add legend
legend("topright", legend = c("Training", "Test"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),bg = "white")

legend("topleft", legend = "Actual No of Change-points", lty=2,bg = "white")


###########
# Convert trainREAL2 and testREAL2 to numbers for vertical lines
trainREAL2_num <- sum(diff(trainREAL2) != 0)
testREAL2  <- as.numeric(as.vector(test_scored_h2o['P']))
testREAL2_num <- sum(diff(testREAL2) != 0)

# Create hist objects without plotting
h1 <- hist(TRAINcount2, plot = FALSE, breaks = sqrt(N))  # training
h2 <- hist(TESTcount2, plot = FALSE, breaks = sqrt(N))   # test

# Plot first histogram
plot(h1, col = rgb(1, 0, 0, 0.5), xlim = c(0, max(c(TRAINcount2, TESTcount2))), ylim = c(0,20),
     main = "Predictands: Momentum 6 & 7", xlab = "No of Change-points")

# Overlay second histogram
plot(h2, col = rgb(0, 0, 1, 0.5), add = TRUE)

# Add vertical lines
abline(v = trainREAL2_num, lty = 2, col = "red", lwd=2)
abline(v = testREAL2_num, lty = 2, col = "blue", lwd=2)

# Add legend
legend("topright", legend = c("Training", "Test"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),bg = "white")

legend("topleft", legend = "Actual No of Change-points", lty=2,bg = "white")

dev.off()





