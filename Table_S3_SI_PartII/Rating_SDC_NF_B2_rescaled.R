###Clear the R environment
rm(list=ls());graphics.off();cat("\f");
library(ggplot2)
library(scales)
library(lmtest)
library(car)
# Load SDC Sample Non-Fiber data [n/m3] paired with Discharge [m3/s]
#where where LF has been rescaled with B2 alpha = 2.433978673; CF = 1.499146674))
C <- c(3.143688962, # rescaled LL 300um to 250um
       3.461970363, # rescaled LL 300um to 250um
       12.7909558, # rescaled LL 300um to 250um
       2.25835935, # rescaled LL 300um to 250um
       2.328013073, # rescaled LL 300um to 250um
       4.266439434, # rescaled LL 300um to 250um
       3.781903819, # rescaled LL 300um to 250um
               1863.333333,
               983.3032491,
               1735.205184,
               16.43192488,
               687.3102611)
Q <- c(0.139035719,
       0.188873369,
       0.113267388,
       0.197368424,
       0.146114931,
       0.086083215,
       0.054934683,
       16.70693973,
       14.75307729,
       21.83228904,
       0.643641932,
       16.48040495
)
df <- data.frame(Q,C) #save as a dataframe

#Test dataset for normality (#Null hypothesis is data is normally distributed)
shapiro.test(df$Q) # P< 0.05 --> not normally distributed
shapiro.test(df$C) # P< 0.05 --> not normally distributed
hist(df$Q) # histograms of Q data
hist(df$C) # histograms of C data

#Plot data in arithmetic space to see if linear
plot(df,xlab = "Stream Discharge (cms)", ylab = "Microplastic (Non-Fiber) Concentration (n/m3)", main = "Arithmetic Plot: C-Q at SDC (Non-Fibers only)")

#Create Linear Model and look at significance of coefficients
model <- lm(C ~ Q, data = df)
summary(model) #model not great
par(mfrow = c(2, 2))
# look at residual plots
plot(model) #Look for curvature, heteroskadasticity ....

# Test for normality in residuals of model
residuals<-resid(model) 
shapiro_xy <- shapiro.test(residuals) #Null hypothesis is data is normally distributed

# Perform the Non Constant Variance test
ncvTest_xy <- ncvTest(model) #Null hypothesis = homoscedasticity / No autocorrelation

#Print results
print("No Transformation")
cat("Test for normality of residuals:\n")
print(shapiro_xy)
cat("Test for constant variance of residuals:\n")
print(ncvTest_xy)

## If linear model is not a good fit/data is non-linear transform y, x, or both

###############################################################################
#Transform y only via ln(y)
##########################
#Create Model and look at significance and residual plots
model <- lm(log(C) ~ Q, data = df)
summary(model)
# residual plots
plot(model) #Look for curvature, heteroskadasticity ....

# Test for normality in residuals of model
residuals<-resid(model) 
shapiro_lny <- shapiro.test(residuals) #Null hypothesis is data is normally distributed

# Perform the Non Constant Variance test
ncvTest_lny <- ncvTest(model) #Null hypothesis = homoscedasticity / No autocorrelation

#Print results
print("Transform y only via ln(y)")
cat("Test for normality of residuals:\n")
print(shapiro_lny)
cat("Test for equal variance of residuals:\n")
print(ncvTest_lny)


###############################################################################
#Transform x and y  via ln()
##########################
#Create Model and look at significance and residual plots
model <- lm(log(C) ~ log(Q), data = df)
summary(model)
# residual plots
plot(model) #Look for curvature, heteroskadasticity ....

# Test for normality in residuals of model
residuals<-resid(model)
shapiro_lnxlny <- shapiro.test(residuals) #Null hypothesis is data is normally distributed

# Perform the Non Constant Variance test
ncvTest_lnxlny <- ncvTest(model) #Null hypothesis = homoscedasticity / No autocorrelation


#Print results
print("Transform x and y via ln()")
cat("Test for normality of residuals:\n")
print(shapiro_lnxlny)
cat("Test for equal variance of residuals:\n")
print(ncvTest_lnxlny)


###############################################################################
###############################################################################
#Below stats re-run with better plots
###############################################################################
###############################################################################

####SUMMARY PLOTS AND RERUN of STATS FOR EACH TYPE OF MODEL #####

#TYPE 1: Untransformed Model

par(mfrow = c(2, 2))
#Plot data in arithmetic space
plot(df, xlab="Discharge, in cubic meters per second",
     ylab="Non-Fiber microplastics per cubic meter", main="Untransformed C-Q")
#Model and look at significance and residual plots
model <- lm(C ~ Q, data = df)
summary(model)
#plot(model) Using below plots instead

# Test for normality in residuals of model
residuals<-resid(model)
qqnorm(residuals)
hist(residuals)
shapiro.test(residuals) #Null hypothesis is data is normally distributed

# Create a scatterplot of residuals vs. fitted values
plot(fitted(model), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
# Perform the Non Constant Variance test
ncvTest(model)



#TYPE 2: Natural Log C Transformed

par(mfrow = c(2, 2))
#Plot data in log-log space
plot(df, xlab="Discharge, in cubic meters per second",
     ylab="Non-Fiber microplastics per cubic meter", main="Natural Log Transformed C only", log = 'y')
#Model and look at significance and residual plots
model <- lm(log(C) ~ Q, data = df)
summary(model)
#plot(model) Using below plots instead

# Test for normality in residuals of model
residuals<-resid(model)
qqnorm(residuals)
hist(residuals)
shapiro.test(residuals) #Null hypothesis is data is normally distributed

# Create a scatterplot of residuals vs. fitted values
plot(fitted(model), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
# Perform the Non Constant Variance test
ncvTest(model)



#TYPE 3: Natural Log - Natural Log Transformed

par(mfrow = c(2, 2))
#Plot data in log-log space
plot(df, xlab="Discharge, in cubic meters per second",
     ylab="Non-Fiber microplastics per cubic meter", main="Natural Log Transformed C and Q", log = 'xy')
#Model and look at significance and residual plots
model <- lm(log(C) ~ log(Q), data = df)
summary(model)
#plot(model) Using below plots instead

# Test for normality in residuals of model
residuals<-resid(model)
qqnorm(residuals)
hist(residuals)
shapiro.test(residuals) #Null hypothesis is data is normally distributed

# Create a scatterplot of residuals vs. fitted values
plot(fitted(model), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
# Perform the Non Constant Variance test
ncvTest(model)

###############################################################################
###############################################################################
# Model selected: #TYPE 3: Natural Log Transformed C and Q
model <- lm(log(C) ~ log(Q), data = df)
###############################################################################
###############################################################################

###AFTER MODEL SELECTION#######

#Bias Correction Selection Following Helschel & Hirsch 2002 and 2020
#regression equations fit in log space result in a bias when back transformed. 
#When the objective is to compute Loads by regressing and summing over the record 
#a correction must be applied because this method assumes the equation produces an estimate of the mean
# the sum of the means = the mean of the sum
# However, applying the regression in orginal units is violates this because the
#regression in original units is the an estimate of the median not the mean 
#the sum of the medians is not the median of the sum
#A sum of the median values will underestimate the sum of the mean.
#Therefore if unaccounted for this will underestimate the long-term mean load.

#Two bias correction methods are commonly employed
#Ferguson' Maximum Likelihood Estimation: residuals must be normally distributed (n > 30)
#Duan: Nonparametric Smearing (does not require normality; requires independence and homoscedastic)

#Our residuals are normally distributed
#Even though our sample size is less than 30,
#We will calculate both following H&H 2020 rcode

# For comparison calculate the mean of the observed concentrations
meanC <- mean(df$C) 
meanC

#calculate the mean of the y fitted values for each Q
yhat <-exp(model$fitted.values)
meanyhat <- mean(yhat) #compare mean of fitted values to mean observed concentrations
100*(meanC-meanyhat)/meanC #this is an underestimate of the mean of the observed data by ~16%

#Ferguson' Maximum Likelihood Estimation (MLE)

#calculate the MLE bias adj
sse <- sum(model$residuals^2)
sSquared <- sse / (length(model$residuals)-2)
biasAdj <- exp(0.5*sSquared)
biasAdj

#Apply MLE bias adj to yhat and check mean
ymle <-yhat*biasAdj
meanmle<-mean(ymle)
meanmle
100*(meanC-meanmle)/meanC #this is an overestimate of the mean of the observed data by ~-11%

##
#Duan's Smearing Estimate of the Bias Correction Adjustment

#calculate the smearing estiamte
smearAdj <- sum(exp(model$residuals))/length(model$residuals)
smearAdj

#Apply Smearing adj to yhat and check mean
ysmear <-yhat*smearAdj
meanysmear<-mean(ysmear)
meanysmear
100*(meanC-meanysmear)/meanC #this is an overestimate of the mean of the observed data by ~-6.3%

#####################################################

#Print summary
print(paste("Mean of Observations:", meanC))
print(paste("Mean of Fit:", meanyhat))
print(paste("MLE Adjustmented Mean:", meanmle))
print(paste("DuanAdjusted Mean:", meanysmear))

##Going with Duan Smearing Correction Adjustment Because
#1.) the sample size is less than 30 
#2.) the smear adjustment produces a closer estimate of mean than MLE adj

#####################################################

#####################################################
#calculating the confidence intervals
####################################################

#Confidence Intervals by bootstrapping (Non-parametric approach)
set.seed(123)  # for reproducibility
data <- data.frame(x=df$Q,
                   y=df$C)

# Define a function to fit the linear model and extract coefficients
fit_model <- function(data, indices) {
  bootmodel <- lm(log(y) ~ log(x), data = data[indices, ])
  return(coef(bootmodel))
}

# Set the number of bootstrap samples
num_bootstraps <- 10000

# Perform bootstrapping to get coefficient distributions
set.seed(123)  # for reproducibility
bootstrap_results <- boot(data, fit_model, R = num_bootstraps)

# Remove rows with missing values in the bootstrap results
bootstrap_results$t <- na.omit(bootstrap_results$t)

# Calculate confidence intervals for coefficients
confidence_intervals <- t(sapply(1:ncol(bootstrap_results$t), function(i) {
  quantile(bootstrap_results$t[, i], probs = c(0.025, 0.975))
}))

# Create a matrix of coefficient names and confidence intervals
coef_names <- names(coefficients(model))
coef_intervals <- data.frame(Coefficient = coef_names,
                             Lower_CI = confidence_intervals[, 1],
                             Upper_CI = confidence_intervals[, 2])

# Print and plot the confidence intervals
print(coef_intervals)

#############################################
#confidence intervals (This is a parametric approach and therefore assumes normality)
confint(model, level = 0.95) 

#############################################
###Confidence Intervals For Plots

#Create Confidence intervals (Uncorrected)
nmodel<- data.frame(Q = seq(from = min(df$Q), to = max(df$Q), length= 100))
model_conf <- exp(predict(model, nmodel, type = "response", interval = "confidence",level=0.95))
nmodel <- cbind(nmodel,model_conf)
nmodel

#Create Confidence intervals (CORRECTED)
model_conf_cor <- model_conf*smearAdj
nmodel_cor<-cbind(nmodel,model_conf_cor)
nmodel_cor <- `colnames<-`(nmodel_cor,c('Q','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
nmodel_cor

#Create Prediction intervals (Uncorrected)
pmodel<- data.frame(Q = seq(from = min(df$Q), to = max(df$Q), length= 100))
model_preds <- exp(predict(model, pmodel, type = "response", interval = "prediction",level=0.95))
pmodel <- cbind(pmodel,model_preds)
pmodel

#Create Prediction intervals (CORRECTED)
model_preds_cor <- model_preds*smearAdj
pmodel_cor<-cbind(pmodel,model_preds_cor)
pmodel_cor <- `colnames<-`(pmodel_cor,c('Q','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
pmodel_cor


#Plot the uncorrected and corrected model and confidence intervals
plot <- ggplot()+ 
  geom_line(aes(x = Q, y = fit), col = "gray", data = pmodel)+
  geom_line(aes(x = Q, y = lwr), linetype=2, col = "green", data = nmodel)+
  geom_line(aes(x = Q, y = upr), linetype=2, col = "green", data = nmodel)+
  geom_line(aes(x = Q, y = lwr), linetype=2, col = "orange", data = pmodel)+
  geom_line(aes(x = Q, y = upr), linetype=2, col = "orange", data = pmodel)+
  geom_line(aes(x = Q, y = fit_cor), col = "black", data = pmodel_cor)+
  geom_line(aes(x = Q, y = lwr_cor), linetype=2, col = "blue", data = nmodel_cor)+
  geom_line(aes(x = Q, y = upr_cor), linetype=2, col = "blue", data = nmodel_cor)+
  geom_line(aes(x = Q, y = fit_cor), col = "transparent", data = pmodel_cor)+
  geom_line(aes(x = Q, y = lwr_cor), linetype=2, col = "purple", data = pmodel_cor)+
  geom_line(aes(x = Q, y = upr_cor), linetype=2, col = "purple", data = pmodel_cor)+
  geom_point(data = df, aes(x=Q, y=C))+
  ggtitle("MP Concentration-Discharge: SDC (Non-Fibers)") +
  xlab("Discharge, in cubic meters per second") +
  ylab("Non-Fiber microplastics per cubic meter")
plot
plot    +scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()+
  annotate(geom="text", x=0.5, y=8000, label="Ln(Concentration)= 3.739 + 1.135 × Ln(Discharge)
       Duan Smearing Correction Adjustment = 1.270",
           color="black")

plot+ theme_bw()+
  annotate(geom="text", x=7.5, y=15000, label="Concentration = 1.270 × (42.03 × Discharge^1.135)",
           color="black")


##Plot of Final
#Plot the uncorrected and corrected model and confidence intervals
plot <- ggplot()+ 
  geom_line(aes(x = Q, y = fit_cor), col = "black", data = pmodel_cor)+
  geom_line(aes(x = Q, y = lwr_cor), linetype=2, col = "blue", data = nmodel_cor)+
  geom_line(aes(x = Q, y = upr_cor), linetype=2, col = "blue", data = nmodel_cor)+
  geom_line(aes(x = Q, y = lwr_cor), linetype=2, col = "purple", data = pmodel_cor)+
  geom_line(aes(x = Q, y = upr_cor), linetype=2, col = "purple", data = pmodel_cor)+
  geom_point(data = df, aes(x=Q, y=C))+
  ggtitle("MP Concentration-Discharge: SDC (Non-Fibers)") +
  xlab("Discharge, in cubic meters per second") +
  ylab("Non-Fibers, number per cubic meter")
plot

plot    +scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotate(geom="text", x=0.5, y=8000, label="Ln(Concentration)= 3.739 + 1.135 × Ln(Discharge)
    With Duan Smearing Correction Adjustment = 1.270",
           color="black") +
theme(
  legend.text = element_text(size = 12),      # Increase legend text size
  legend.title = element_text(size = 14),     # Increase legend title size
  axis.title.x = element_text(size = 14),     # Increase x-axis title size
  axis.title.y = element_text(size = 14),     # Increase y-axis title size
  axis.text.x = element_text(size = 12, color= "black"),       # Increase x-axis label size
  axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
  plot.title = element_text(size = 16, face = "bold"), # Increase title size
  plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
  axis.ticks.y = element_line(color = "black"))+
  annotate("rect", xmin = 15.5, xmax = 19.5, ymin = 0.035, ymax = 0.01, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 17.5, y = 0.02, label = "b", size = 6.5, fontface = "bold") 

plot+ theme_bw()+
  annotate(geom="text", x=7.5, y=15000, label="Concentration = 1.270 × (42.03 × Discharge^1.135)",
           color="black")


