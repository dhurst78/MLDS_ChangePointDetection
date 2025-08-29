# ------------------------------------------------------------
# Fit multiple logistic regression
# - Loads S&P 500 data
# - labels trends using heuristic (s=40)
# - Calculate all 12 predictors
# - carries out logistic regression (on training set), for all combinations of predcitors of:
# - individual, pairs, triplets, quartets, quintets
# - generates ROC curves, calculates optimum threshold
# - makes 'predictions' on training set, record AUC and no of predicted change-points
# ------------------------------------------------------------
rm(list=ls())
inDIR  = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\Codes'
outDIR = 'C:\\Users\\daryl\\OneDrive\\IC_course\\Year2\\FINAL_PROJECT\\MileStones\\Final_Writeup\\MScWriteup_template\\pics'
#########
source(paste0(inDIR,'\\00_functions.R'))
setwd(outDIR)
##########

SPdata = GetPrices(Ticker = '^SPX', startDate = '1957-03-04', endDate = '2025-08-01')

################################
#######################################################
DATA = SPdata[,c('ref_date','RetAdj')]
 
LABEL = LabelFnDouble(DF = SPdata, S = SS) # label trends

# offset by 1 day, so todays predictors are aligned with tomorrows trend
LABEL$P = shift(LABEL$P,-1)

DATA = merge(DATA,LABEL,by.x='ref_date',by.y = 'Date')

head(DATA)

################### calc momentum
for (k in 2:7) {
  DATA[[paste0("mom", k)]] <- Momentum(DATA$RetAdj, k)
}
DATA$Volatility  = sqrt(EWMA(DATA$RetAdj^2,100))
DATA$DrawDown = calculate_drawdown(cumsum(DATA$RetAdj))

head(DATA)

############# 
############# - create sine and cosine terms for seasonality

doy = yday(DATA$ref_date) # day of month
dom = mday(DATA$ref_date) # day of year

DATA$sineAnnual = sin(2*pi*doy/(max(doy)))
DATA$sineMonthly = sin(2*pi*dom/(max(dom)))
DATA$cosAnnual = cos(2*pi*doy/(max(doy)))
DATA$cosMonthly = cos(2*pi*dom/(max(dom)))

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

TRAIN = smote_result

str(TRAIN)

TRAIN$P = as.factor(TRAIN$class)
TRAIN$class = NULL


############## create a function, to return: AUC, threshold, no of change points
############## you pass into the function a list of predictors


#######################
# Example: Vector of predictors
predictors <- c( "mom2", "mom3")

# Build formula string without intercept
formula_str <- paste("P ~ 0 +", paste(predictors, collapse = " + "))

# Convert to formula
model_formula <- as.formula(formula_str)

# Fit the model
MOD <- glm(model_formula, data = TRAIN, family = binomial(link = "logit"))
predictions = predict(MOD, type = "response", newdata = TRAINorig)
roc_obj = roc(TRAINorig$P, predictions, ci=TRUE, auc=TRUE, quiet = TRUE)

thresh = coords(roc_obj, "best", best.method = "youden")$threshold

PredictBinary = 1*(predictions>thresh)
CPcount = sum(diff(PredictBinary) != 0) #  count number of predicted CP's
CPcount 


#############################################
##
##
ModelFunction = function(TRAIN = TRAIN, predictions = predictions)
{
# Build formula string without intercept
formula_str <- paste("P ~ 0 +", paste(predictors, collapse = " + "))

# Convert to formula
model_formula <- as.formula(formula_str)

# Fit the model
MOD <- glm(model_formula, data = TRAIN, family = binomial(link = "logit"))

## make predictions, generate RoC, sleect threshold based on balanced data
predictions = predict(MOD, type = "response", newdata = TRAIN)
roc_obj = roc(TRAIN$P, predictions, ci=TRUE, auc=TRUE, quiet = TRUE)
thresh = coords(roc_obj, "best", best.method = "youden")$threshold

### for purposes of counting change points, make predictions on raw unbalanced data
predictions = predict(MOD, type = "response", newdata = TRAINorig)
PredictBinary = 1*(predictions>thresh)
CPcount = sum(diff(PredictBinary) != 0) #  count number of predicted CP's

return(data.frame(AuC =roc_obj$auc[1], Thresh = thresh, CPcount = CPcount))

}
##
#############################################


z = ModelFunction(TRAIN = TRAIN, predictions = c('mom2','mom4'))
z


################### - loop over all single options
# Get list of predictor names
predictors <- setdiff(names(TRAIN), "P")

################### - loop over all pairwise options
ModelFunction <- function(varnames, data, interactions = FALSE) {
  # Build formula from vector of predictor names

if (interactions) {
  model_formula <- as.formula(paste("P ~ 0 +", paste(varnames, collapse = " * ")))
} else {
  model_formula <- as.formula(paste("P ~ 0 +", paste(varnames, collapse = " + ")))
}

  # Fit logistic regression
  MOD <- glm(model_formula, data = data, family = binomial(link = "logit"))


## make predictions, generate RoC, sleect threshold based on balanced data
predictions = predict(MOD, type = "response", newdata = TRAIN)
roc_obj = roc(TRAIN$P, predictions, ci=FALSE, auc=TRUE, quiet = TRUE)
thresh = coords(roc_obj, "best", best.method = "youden")$threshold

### for purposes of counting change points, make predictions on raw unbalanced data
predictions = predict(MOD, type = "response", newdata = TRAINorig)
PredictBinary = 1*(predictions>thresh)
CPcount = sum(diff(PredictBinary) != 0) #  count number of predicted CP's

  return(data.frame(Variables = paste(varnames, collapse = " + "),
                    AUC = roc_obj$auc[1],
                    Threshold = thresh,
                    CPcount = CPcount))
}


predictors <- setdiff(names(TRAIN), "P")

single <- combn(predictors, 1, simplify = FALSE)
pairs <- combn(predictors, 2, simplify = FALSE)
triplets <- combn(predictors, 3, simplify = FALSE)
quads <- combn(predictors, 4, simplify = FALSE)
quintile <- combn(predictors, 5, simplify = FALSE)


results_single <- do.call(rbind, lapply(single, function(variables) {
  ModelFunction(varnames = variables, data = TRAIN)
}))


results_pairs <- do.call(rbind, lapply(pairs, function(variables) {
  ModelFunction(varnames = variables, data = TRAIN)
}))


results_triplets <- do.call(rbind, lapply(triplets, function(variables) {
  ModelFunction(varnames = variables, data = TRAIN)
}))


results_quads <- do.call(rbind, lapply(quads, function(variables) {
  tryCatch(
    ModelFunction(varnames = variables, data = TRAIN),
    error = function(e) NULL  # skip combinations that fail (e.g. collinearity)
  )
}))

results_quintile <- do.call(rbind, lapply(quintile, function(variables) {
  tryCatch(
    ModelFunction(varnames = variables, data = TRAIN),
    error = function(e) NULL  # skip combinations that fail (e.g. collinearity)
  )
}))


##############################
############################## - now repeat but allow for interactions
##############################


results_single_int <- do.call(rbind, lapply(single, function(variables) {
  ModelFunction(varnames = variables, data = TRAIN)
}))


results_pairs_int <- do.call(rbind, lapply(pairs, function(variables) {
  ModelFunction(varnames = variables, data = TRAIN, interactions = TRUE)
}))


results_triplets_int <- do.call(rbind, lapply(triplets, function(variables) {
  ModelFunction(varnames = variables, data = TRAIN, interactions = TRUE)
}))


results_quads_int <- do.call(rbind, lapply(quads, function(variables) {
  tryCatch(
    ModelFunction(varnames = variables, data = TRAIN, interactions = TRUE),
    error = function(e) NULL  # skip combinations that fail (e.g. collinearity)
  )
}))

results_quintile_int <- do.call(rbind, lapply(quintile, function(variables) {
  tryCatch(
    ModelFunction(varnames = variables, data = TRAIN, interactions = TRUE),
    error = function(e) NULL  # skip combinations that fail (e.g. collinearity)
  )
}))

######### ######### ######### ######### ######### #########
######### ######### ######### ######### ######### #########
######### ######### ######### ######### ######### #########
####################################

Mquintile = merge(results_quintile,results_quintile_int, by = 'Variables')
Mquads = merge(results_quads,results_quads_int, by = 'Variables')
Mtriplets = merge(results_triplets,results_triplets_int, by = 'Variables')
Mpairs = merge(results_pairs,results_pairs_int, by = 'Variables')
Msingle = merge(results_single,results_single_int, by = 'Variables')



##################
#### - plot both in 2x2 grid


pdf(file="AuC_2x2_scatter_1.pdf", width = 10.0, height = 6.0)
par(mar = c(3, 4, 1.4, 0.8), mfrow=c(2,2), mgp = c(1.8, 0.6, 0))

plot(results_quintile$AUC,results_quintile$CPcount, pch=16,col=6, xlab='AUC',ylab='No of Change Points', log='y', 
		ylim = c(5,2000), main='Excluding Interactions', xlim = c(0.5,1.0))

points(results_quads$AUC,results_quads$CPcount, pch=16,col=4)
points(results_triplets$AUC,results_triplets$CPcount, pch=16,col=5)
points(results_pairs$AUC,results_pairs$CPcount, pch=16,col=2)
points(results_single$AUC,results_single$CPcount, pch=16,col=3)

abline(h = sum(abs(diff(TRAINorig$P))),lty=2,col='grey69', lwd=2)
grid()


plot(results_quintile_int$AUC,results_quintile_int$CPcount, pch=16,col=6, xlab='AUC',ylab='No of Change Points', log='y', 
		ylim = c(5,2000), main='Including Interactions', xlim = c(0.5,1.0))

points(results_quads_int$AUC,results_quads_int$CPcount, pch=16,col=4)
points(results_triplets_int$AUC,results_triplets_int$CPcount, pch=16,col=5)
points(results_pairs_int$AUC,results_pairs_int$CPcount, pch=16,col=2)
points(results_single_int$AUC,results_single_int$CPcount, pch=16,col=3)

abline(h = sum(abs(diff(TRAINorig$P))),lty=2,col='grey69', lwd=2)

legend("bottomright", legend = c("Quintet", "Quartet", "Triplets", "Pairs", "Singles"),
       col = c(6, 4, 5, 2, 3), text.col = c(6, 4, 5, 2, 3), bty = "n", cex = 1.1)

grid()

plot(Mquintile$AUC.x,Mquintile$AUC.y,pch=16,col=6,xlim = c(0.5,1),ylim = c(0.5,1),main='AUC', xlab='Excluding Interactions', ylab='Including Interactions')
points(Mquads$AUC.x,Mquads$AUC.y,pch=16,col=4)
points(Mtriplets$AUC.x,Mtriplets$AUC.y,pch=16,col=5)
points(Mpairs$AUC.x,Mpairs$AUC.y,pch=16,col=2)
points(Msingle$AUC.x,Msingle$AUC.y,pch=16,col=3)
grid()


plot(Mquintile$CPcount.x,Mquintile$CPcount.y,pch=16,col=6,xlim = c(5,1500),ylim = c(5,1500), log='xy',main='No of Change Points', xlab='Excluding Interactions', ylab='Including Interactions')
points(Mquads$CPcount.x,Mquads$CPcount.y,pch=16,col=4)
points(Mtriplets$CPcount.x,Mtriplets$CPcount.y,pch=16,col=5)
points(Mpairs$CPcount.x,Mpairs$CPcount.y,pch=16,col=2)
points(Msingle$CPcount.x,Msingle$CPcount.y,pch=16,col=3)
grid()

abline(h=sum(abs(diff(TRAINorig$P))),lty=2,col='grey69', lwd=2)
abline(v=sum(abs(diff(TRAINorig$P))),lty=2,col='grey69', lwd=2)


legend("bottomright",
       legend = c("Quintet", "Quartet", "Triplets", "Pairs", "Singles"),
       col = c(6, 4, 5, 2, 3),
       text.col = c(6, 4, 5, 2, 3),
       bty = "n")

grid()


dev.off()



################################
################################
################################
##


#####################
ExtractByVariable <- function(df) {
  # Extract all unique variable names from the Variables column
  variable_names <- unique(unlist(strsplit(df$Variables, " \\+ ")))

  # Loop through each variable and extract matching rows
  results_list <- lapply(variable_names, function(var) {
    matched_rows <- grep(var, df$Variables)
    if (length(matched_rows) == 0) return(NULL)

    temp <- df[matched_rows, ]
    temp$MatchedVariable <- var  # add column to tag which variable was matched
    return(temp)
  })

  # Combine all individual data frames into one
  result_df <- do.call(rbind, results_list)
  return(result_df)
}

#nrow(all_rows_by_variable)

all_rows_by_variable <- ExtractByVariable(Mquintile)
head(all_rows_by_variable)


############################################################################


plotBoxPlot = function(all_rows_by_variable)
{
A = all_rows_by_variable[,c('CPcount.x','MatchedVariable')]
B = all_rows_by_variable[,c('CPcount.y','MatchedVariable')]
B$Label = 'Including_Interactions'
A$Label = 'Excluding_Interactions'

names(A)[1] = 'CPcount'
names(B)[1] = 'CPcount'
SUB = rbind(A,B)
head(SUB)


# grouped boxplot
p = ggplot(SUB, aes(x=MatchedVariable, y=CPcount, fill=Label)) + 
    geom_boxplot()+
theme_bw(base_size = 13)+
theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),legend.position= c(0.85,0.92),legend.title = element_blank(),plot.margin = margin(b=1, r=5))+
scale_y_log10(limits = c(10, 2000), expand = c(0, 0))+
#scale_y_continuous(trans='log10')+
labs(x = "")+
geom_hline(yintercept = sum(abs(diff(TRAINorig$P))),linetype=2, col = 4)

return(p)

}
### put one one plot

all_rows_by_variable <- ExtractByVariable(Mpairs)
plot1 = plotBoxPlot(all_rows_by_variable)
all_rows_by_variable <- ExtractByVariable(Mtriplets)
plot2 = plotBoxPlot(all_rows_by_variable)
all_rows_by_variable <- ExtractByVariable(Mquads)
plot3 = plotBoxPlot(all_rows_by_variable)
all_rows_by_variable <- ExtractByVariable(Mquintile)
plot4 = plotBoxPlot(all_rows_by_variable)


combined_plot = plot_grid(plot1+ theme(legend.position="none"), plot2+ theme(legend.position="none"),
plot3 + theme(legend.position="none"), plot4+ theme(legend.position="none"), labels=c("Pairs", "Triplets",'Quads','Quintiles'),
	label_x = 0.3, ncol = 2, nrow = 2,label_y = 0.99)

# extract legend from plot1
legend <- get_legend(
  plot1 +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "top",legend.text = element_text(size = 13))
)



# Combine combined plot and legend using plot_grid()
pdf(file="predictor_boxplots_combined_1.pdf", width = 10, height = 6.0)

plot_grid(legend,combined_plot,ncol=1,rel_heights = c(0.05, 1))

dev.off()


#############################################################################

#### manually extract some data for report
X <- ExtractByVariable(Mpairs)
table(X$MatchedVariable)



sum(abs(diff(TRAINorig$P)))# number of labelled change points




results_pairs[results_pairs$AUC>0.7 & results_pairs$CPcount < 99,]
results_quads[results_quads$AUC>0.75 & results_quads$CPcount < 50, ]
results_single[results_quads$AUC>0.75 & results_quads$CPcount < 50, ]
results_triplets_int[results_triplets_int$AUC>0.8 & results_triplets_int$CPcount < 50, ]
