# Combinging Rating Plots v3
#Using only two colors (paired red blue)

setwd("~/Desktop/Papers/C-Q_v2/Response to Reviews_JOH_Rejection/Rescaled")

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

library(ggplot2)
library(scales)
library(lmtest)
library(car)
library(boot)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(grid)
library(tibble)
library(patchwork)
library(cowplot)


## Load and Create Models
# SAD Sample data
C_SAD <- c(20.17851768, # rescaled LL 300um to 250um
           12.48359388, # rescaled LL 300um to 250um
           22.46251594, # rescaled LL 300um to 250um
           51.05125154, # rescaled LL 300um to 250um
           1502.814216,  # rescaled LL 300um to 250um
           10365.90909,
           19424.24242,
           25228.31050)
Q_SAD <- c(0.0679604,
           0.1019406,
           0.0538020,
           0.1160991,
           1.0194065,
           10.1091144,
           7.3623802,
           9.2312921)
df_SAD <- data.frame(Q_SAD,C_SAD)


# SAD Non-Fiber Sample data
C_SADNF <- c(8.271190137, # rescaled LL 300um to 250um
             10.37482485, # rescaled LL 300um to 250um
             21.6614931, # rescaled LL 300um to 250um
             29.6761929, # rescaled LL 300um to 250um
             1260.31856, # rescaled LL 300um to 250um
             7077.272727,
             13775.75758,
             11221.46119)
Q_SADNF <- c(0.0679604,
             0.1019406,
             0.0538020,
             0.1160991,
             1.0194065,
             10.1091144,
             7.3623802,
             9.2312921)
df_SADNF <- data.frame(Q_SADNF,C_SADNF)


# SAD TRWP Sample data
C_SADTRWP <- c(1.516384859, # rescaled LL 300um to 250um
               3.21977323, # rescaled LL 300um to 250um
               5.570098225, # rescaled LL 300um to 250um
               19.51311314, # rescaled LL 300um to 250um
               105.3042477, # rescaled LL 300um to 250um
               2448,
               4921,
               4503)
Q_SADTRWP <- c(0.0679604,
               0.1019406,
               0.0538020,
               0.1160991,
               1.0194065,
               10.1091144,
               7.3623802,
               9.2312921)
df_SADTRWP <- data.frame(Q_SADTRWP,C_SADTRWP)


# SDC Sample data
C_SDC <- c(5.297148617,  # rescaled LL 300um to 250um
           5.882173475, # rescaled LL 300um to 250um
           15.8213591, # rescaled LL 300um to 250um  
           4.529772341, # rescaled LL 300um to 250um  
           2.861200866, # rescaled LL 300um to 250um  
           5.815344522, # rescaled LL 300um to 250um  
           6.626380413, # rescaled LL 300um to 250um  
           3407.222222,
           1405.415162,
           2632.829374,
           1302.367942,
           31.61189358)
Q_SDC <- c(0.139035719,
           0.188873369,
           0.113267388,
           0.197368424,
           0.146114931,
           0.086083215,
           0.054934683,
           16.70693973,
           14.75307729,
           21.83228904,
           16.48040495,
           0.643641932)
df_SDC <- data.frame(Q_SDC,C_SDC)


# Load SDC Sample Non-Fiber data 
C_SDCNF <- c(3.143688962, # rescaled LL 300um to 250um
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
Q_SDCNF <- c(0.139035719,
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
             16.48040495)
df_SDCNF <- data.frame(Q_SDCNF,C_SDCNF) #save as a dataframe


# SDC TRWP Sample data
C_SDCTRWP <- c(0.589441681, # rescaled LL 300um to 250um
               0.340521674, # rescaled LL 300um to 250um
               1.166348554, # rescaled LL 300um to 250um
               0.312130967, # rescaled LL 300um to 250um
               0.53122446,  # rescaled LL 300um to 250um
               0.447858283, # rescaled LL 300um to 250um
               0.907656916, # rescaled LL 300um to 250um
               650,
               19.0,
               405,
               2.03,
               55.3)
Q_SDCTRWP <- c(0.139035719,
               0.188873369,
               0.113267388,
               0.197368424,
               0.146114931,
               0.086083215,
               0.054934683,
               16.70693973,
               14.75307729,
               21.83228904,
               16.48040495,
               0.643641932)

df_SDCTRWP <- data.frame(Q_SDCTRWP,C_SDCTRWP) #save as a dataframe


# Model selected: #TYPE: Natural Log Transformed C and Q
model_SAD <- lm(log(C_SAD) ~ log(Q_SAD), data = df_SAD) # save as model_SAD
model_SADNF <- lm(log(C_SADNF) ~ log(Q_SADNF), data = df_SADNF) # save as model_SAD
model_SADTRWP <- lm(log(C_SADTRWP) ~ log(Q_SADTRWP), data = df_SADTRWP)
model_SDC <- lm(log(C_SDC) ~ log(Q_SDC), data = df_SDC) # save as model_SDC
model_SDCNF <- lm(log(C_SDCNF) ~ log(Q_SDCNF), data = df_SDCNF) # save as model_SDC
model_SDCTRWP <- lm(log(C_SDCTRWP) ~ log(Q_SDCTRWP), data = df_SDCTRWP) # save as model_SDC

#calculate Bias Correction Factors
#SAD
smearAdj_SAD <- sum(exp(model_SAD$residuals))/length(model_SAD$residuals)
smearAdj_SAD #Duan's Smearing Estimate of the Bias Correction Adjustment
#SADNF
smearAdj_SADNF <- sum(exp(model_SADNF$residuals))/length(model_SADNF$residuals)
smearAdj_SADNF #Duan's Smearing Estimate of the Bias Correction Adjustment
#SADTRWP
smearAdj_SADTRWP <- sum(exp(model_SADTRWP$residuals))/length(model_SADTRWP$residuals)
smearAdj_SADTRWP #Duan's Smearing Estimate of the Bias Correction Adjustment

#SDC
smearAdj_SDC <- sum(exp(model_SDC$residuals))/length(model_SDC$residuals)
smearAdj_SDC #Duan's Smearing Estimate of the Bias Correction Adjustment
#SDCNF
smearAdj_SDCNF <- sum(exp(model_SDCNF$residuals))/length(model_SDCNF$residuals)
smearAdj_SDCNF #Duan's Smearing Estimate of the Bias Correction Adjustment
#SDCTRWP
smearAdj_SDCTRWP <- sum(exp(model_SDCTRWP$residuals))/length(model_SDCTRWP$residuals)
smearAdj_SDCTRWP #Duan's Smearing Estimate of the Bias Correction Adjustment


#Add in MP Correction Factor
MPCF_SAD <- 0.66
MPCF_SAD_NF <- 0.62
MPCF_SDC <- 0.71
MPCF_SDC_NF <- 0.69

#Create Confidence intervals (Uncorrected)
#SAD
nmodel_SAD<- data.frame(Q_SAD = seq(from = min(df_SAD$Q_SAD), to = max(df_SAD$Q_SAD), length= 100))
model_SAD_conf <- exp(predict(model_SAD, nmodel_SAD, type = "response", interval = "confidence",level=0.95))
nmodel_SAD <- cbind(nmodel_SAD,model_SAD_conf)
nmodel_SAD

#SADNF
nmodel_SADNF<- data.frame(Q_SADNF = seq(from = min(df_SADNF$Q_SADNF), to = max(df_SADNF$Q_SADNF), length= 100))
model_SADNF_conf <- exp(predict(model_SADNF, nmodel_SADNF, type = "response", interval = "confidence",level=0.95))
nmodel_SADNF <- cbind(nmodel_SADNF,model_SADNF_conf)
nmodel_SADNF

#SADTRWP
nmodel_SADTRWP<- data.frame(Q_SADTRWP = seq(from = min(df_SADTRWP$Q_SADTRWP), to = max(df_SADTRWP$Q_SADTRWP), length= 100))
model_SADTRWP_conf <- exp(predict(model_SADTRWP, nmodel_SADTRWP, type = "response", interval = "confidence",level=0.95))
nmodel_SADTRWP <- cbind(nmodel_SADTRWP,model_SADTRWP_conf)
nmodel_SADTRWP


#SDC
nmodel_SDC<- data.frame(Q_SDC = seq(from = min(df_SDC$Q_SDC), to = max(df_SDC$Q_SDC), length= 100))
model_SDC_conf <- exp(predict(model_SDC, nmodel_SDC, type = "response", interval = "confidence",level=0.95))
nmodel_SDC <- cbind(nmodel_SDC,model_SDC_conf)
nmodel_SDC

#SDCNF
nmodel_SDCNF<- data.frame(Q_SDCNF = seq(from = min(df_SDCNF$Q_SDCNF), to = max(df_SDCNF$Q_SDCNF), length= 100))
model_SDCNF_conf <- exp(predict(model_SDCNF, nmodel_SDCNF, type = "response", interval = "confidence",level=0.95))
nmodel_SDCNF <- cbind(nmodel_SDCNF,model_SDCNF_conf)
nmodel_SDCNF

#SDCTRWP
nmodel_SDCTRWP<- data.frame(Q_SDCTRWP = seq(from = min(df_SDCTRWP$Q_SDCTRWP), to = max(df_SDCTRWP$Q_SDCTRWP), length= 100))
model_SDCTRWP_conf <- exp(predict(model_SDCTRWP, nmodel_SDCTRWP, type = "response", interval = "confidence",level=0.95))
nmodel_SDCTRWP <- cbind(nmodel_SDCTRWP,model_SDCTRWP_conf)
nmodel_SDCTRWP


#Correct Models and Confidence intervals with BCF and MPCF 
#SAD
model_SAD_conf_cor <- model_SAD_conf*smearAdj_SAD*MPCF_SAD
nmodel_SAD_cor<-cbind(nmodel_SAD,model_SAD_conf_cor)
nmodel_SAD_cor <- `colnames<-`(nmodel_SAD_cor,c('Q_SAD','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
nmodel_SAD_cor

#SADNF
model_SADNF_conf_cor <- model_SADNF_conf*smearAdj_SADNF*MPCF_SAD_NF
nmodel_SADNF_cor<-cbind(nmodel_SADNF,model_SADNF_conf_cor)
nmodel_SADNF_cor <- `colnames<-`(nmodel_SADNF_cor,c('Q_SADNF','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
nmodel_SADNF_cor

#SADTRWP
model_SADTRWP_conf_cor <- model_SADTRWP_conf*smearAdj_SADTRWP*MPCF_SAD_NF
nmodel_SADTRWP_cor<-cbind(nmodel_SADTRWP,model_SADTRWP_conf_cor)
nmodel_SADTRWP_cor <- `colnames<-`(nmodel_SADTRWP_cor,c('Q_SADTRWP','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
nmodel_SADTRWP_cor

#SDC
model_SDC_conf_cor <- model_SDC_conf*smearAdj_SDC*MPCF_SDC
nmodel_SDC_cor<-cbind(nmodel_SDC,model_SDC_conf_cor)
nmodel_SDC_cor <- `colnames<-`(nmodel_SDC_cor,c('Q_SDC','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
nmodel_SDC_cor

#SDCNF
model_SDCNF_conf_cor <- model_SDCNF_conf*smearAdj_SDCNF*MPCF_SDC_NF
nmodel_SDCNF_cor<-cbind(nmodel_SDCNF,model_SDCNF_conf_cor)
nmodel_SDCNF_cor <- `colnames<-`(nmodel_SDCNF_cor,c('Q_SDCNF','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
nmodel_SDCNF_cor

#SDCTRWP
model_SDCTRWP_conf_cor <- model_SDCTRWP_conf*smearAdj_SDCTRWP*MPCF_SDC_NF
nmodel_SDCTRWP_cor<-cbind(nmodel_SDCTRWP,model_SDCTRWP_conf_cor)
nmodel_SDCTRWP_cor <- `colnames<-`(nmodel_SDCTRWP_cor,c('Q_SDCTRWP','fit_uncor','lwr_uncor','upr_uncor','fit_cor','lwr_cor','upr_cor'))
nmodel_SDCTRWP_cor

#Create 1st Plot the corrected model and confidence intervals


p1 <- ggplot()+ 
  geom_point(data = df_SAD, aes(x=Q_SAD, y=C_SAD), shape=16, size=2, col = "#DC3220")+
  geom_point(data = df_SDC, aes(x=Q_SDC, y=C_SDC),shape=16, size=2, col = "#005AB5")+
  geom_line(aes(x = Q_SAD, y = fit_cor), linetype=1, linewidth=1.25,col = "#DC3220", data = nmodel_SAD_cor)+
  geom_line(aes(x = Q_SDC, y = fit_cor), linetype=1, linewidth=1.25, col = "#005AB5", data = nmodel_SDC_cor)+
  geom_ribbon(data = nmodel_SAD_cor, aes(x=Q_SAD, ymin = lwr_cor, ymax = upr_cor),fill="#DC3220", alpha = 0.4) +
  geom_ribbon(data = nmodel_SDC_cor, aes(x=Q_SDC, ymin = lwr_cor, ymax = upr_cor), fill="#005AB5", alpha = 0.4) +
  guides(linetype = "none", fill = "none") +
  labs(x = NULL, y = NULL)+
  theme(
    legend.text = element_text(size = 12),      # Increase legend text size
    legend.title = element_text(size = 14),     # Increase legend title size
    axis.title.x = element_text(size = 14),     # Increase x-axis title size
    axis.title.y = element_text(size = 14),     # Increase y-axis title size
    axis.text.x = element_text(size = 13, color= "black"),       # Increase x-axis label size
    axis.text.y = element_text(size = 13, color = "black"),      # Increase y-axis label size
    plot.title = element_text(size = 16, face = "bold"), # Increase title size
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
    axis.ticks.y = element_line(color = "black"))


#Create 2nd Plot the corrected model and confidence intervals


p2 <- ggplot()+ 
  geom_point(data = df_SADNF, aes(x=Q_SADNF, y=C_SADNF),shape=16, size=2,col = "#DC3220")+
  geom_point(data = df_SDCNF, aes(x=Q_SDCNF, y=C_SDCNF),shape=16, size=2, col = "#005AB5")+
  geom_line(aes(x = Q_SADNF, y = fit_cor), linetype=1, linewidth=1.25, col = "#DC3220", data = nmodel_SADNF_cor)+
  geom_line(aes(x = Q_SDCNF, y = fit_cor), linetype=1,linewidth=1.25, col = "#005AB5", data = nmodel_SDCNF_cor)+
  geom_ribbon(data = nmodel_SADNF_cor, aes(x=Q_SADNF, ymin = lwr_cor, ymax = upr_cor),fill="#DC3220", alpha = 0.4) +
  geom_ribbon(data = nmodel_SDCNF_cor, aes(x=Q_SDCNF, ymin = lwr_cor, ymax = upr_cor),fill="#005AB5", alpha = 0.4) +
  guides(linetype = "none", fill = "none") +
  labs(x = NULL, y = NULL)+
  theme(
    legend.text = element_text(size = 12),      # Increase legend text size
    legend.title = element_text(size = 14),     # Increase legend title size
    axis.title.x = element_text(size = 14),     # Increase x-axis title size
    axis.title.y = element_text(size = 14),     # Increase y-axis title size
    axis.text.x = element_text(size = 12, color= "black"),       # Increase x-axis label size
    axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
    plot.title = element_text(size = 16, face = "bold"), # Increase title size
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
    axis.ticks.y = element_line(color = "black"))


#Create 3rd Plot the corrected model and confidence intervals


p3 <- ggplot()+ 
  geom_point(data = df_SADTRWP, aes(x=Q_SADTRWP, y=C_SADTRWP), shape=16, size=2, col = "#DC3220")+
  geom_point(data = df_SDCTRWP, aes(x=Q_SDCTRWP, y=C_SDCTRWP),shape=16, size=2, col = "#005AB5")+
  geom_line(aes(x = Q_SADTRWP, y = fit_cor), linetype=1, linewidth=1.25, col = "#DC3220", data = nmodel_SADTRWP_cor)+
  geom_line(aes(x = Q_SDCTRWP, y = fit_cor), linetype=1,linewidth=1.25, col = "#005AB5", data = nmodel_SDCTRWP_cor)+
  geom_ribbon(data = nmodel_SADTRWP_cor, aes(x=Q_SADTRWP, ymin = lwr_cor, ymax = upr_cor),fill="#DC3220", alpha = 0.4) +
  geom_ribbon(data = nmodel_SDCTRWP_cor, aes(x=Q_SDCTRWP, ymin = lwr_cor, ymax = upr_cor),fill="#005AB5", alpha = 0.4) +
  guides(linetype = "none", fill = "none") +
  labs(x = NULL, y = NULL)+
  theme(
    legend.text = element_text(size = 12),      # Increase legend text size
    legend.title = element_text(size = 14),     # Increase legend title size
    axis.title.x = element_text(size = 14),     # Increase x-axis title size
    axis.title.y = element_text(size = 14),     # Increase y-axis title size
    axis.text.x = element_text(size = 12, color= "black"),       # Increase x-axis label size
    axis.text.y = element_text(size = 12, color = "black"),      # Increase y-axis label size
    plot.title = element_text(size = 16, face = "bold"), # Increase title size
    plot.subtitle = element_text(size = 12, face = "bold", hjust = 0, vjust = 1.5),
    axis.ticks.y = element_line(color = "black"))


y_limits = c(0.001, 1000000)

p1<- p1 + labs(
  x=expression("Q - Water Discharge [m"^ 3* "s"^-1*"]"),
  y = expression("C - Concentration [n"^1*"m"^-3*"]"), title = "All Microplastics" )+
  annotate(geom="text", x=0.05, y=110000, label=expression("SAD: C = 0.66 x 1.13 x (859 x Q"^1.40*")"),
           color="#DC3220", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=30000, label=expression("R"^2*"=0.970"),
           color="#DC3220", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=0.04, label=expression("SDC: C = 0.71 x 1.23 x (68.0 x Q"^1.15*")"),
           color="#005AB5", fontface="bold",hjust = 0, size=4.5)+
  annotate(geom="text", x=0.05, y=0.01, label=expression("R"^2*"=0.946"),
           color="#005AB5", fontface="bold",hjust = 0,size=4.5)
p1 <- p1+ 
  scale_y_log10(limits = y_limits, sec.axis = dup_axis(), 
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(sec.axis = dup_axis(),breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
p2<-p2+
  annotate(geom="text", x=0.05, y=110000, label= expression("SAD: C = 0.62 x 1.19 x (559 x Q"^1.38*")"),
           color="#DC3220", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=30000, label=expression("R"^2*"=0.963"),
           color="#DC3220", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=0.04, label=expression("SDC: C = 0.69 x 1.27 x (42.0 x Q"^1.13*")"),
           color="#005AB5", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=0.01, label=expression("R"^2*"=0.936"),
           color="#005AB5", fontface="bold",hjust = 0,size=4.5)

p2<- p2+labs(
  x=expression("Q - Water Discharge [m"^ 3* "s"^-1*"]"), title = "Non-Fibers Only" )+
  theme(axis.title.y = element_blank(),  axis.text.y = element_blank())

p2 <- p2+ 
  scale_y_log10(limits = y_limits, sec.axis = dup_axis(), 
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(sec.axis = dup_axis(),breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

p3 <- p3+labs(
  x=expression("Q - Water Discharge [m"^ 3* "s"^-1*"]"),
  y = expression("C - Concentration [n"^1*"m"^-3*"]"), title = "TRWPs only" )+
  annotate(geom="text", x=0.05, y=110000, label= expression("SAD: C = 0.62 x 1.24 x (160 x Q"^1.41*")"),
           color="#DC3220", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=30000, label=expression("R"^2*"=0.955"),
           color="#DC3220", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=0.04, label=expression("SDC: C = 0.69 x 3.21 x (5.04 x Q"^0.907*")"),
           color="#005AB5", fontface="bold",hjust = 0,size=4.5)+
  annotate(geom="text", x=0.05, y=0.01, label=expression("R"^2*"=0.610"),
           color="#005AB5", fontface="bold",hjust = 0,size=4.5)

p3 <- p3 +
  scale_y_log10(
    limits = y_limits, sec.axis = dup_axis(),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_x_log10(
    sec.axis = dup_axis(),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  )

####Adjust axis
p1 <- p1+ theme(plot.title = element_text(hjust = 0.5), axis.title.y.right= element_blank(),axis.text.y.right= element_blank(), axis.title.x.top = element_blank(),axis.title.x.bottom = element_blank())+
  annotate("rect", xmin = 14, xmax = 21, ymin = 0.001, ymax = 0.01, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 17.5, y = 0.0035, label = "a", size = 6.5, fontface = "bold") 


p2<- p2+ theme(plot.title = element_text(hjust = 0.5), axis.title.x.top = element_blank())+
  annotate("rect", xmin = 14, xmax = 21, ymin = 0.001, ymax = 0.01, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 17.5, y = 0.0035, label = "b", size = 6.5, fontface = "bold") 


p3 <- p3+ theme(plot.title = element_text(hjust = 0.5), axis.title.y.left = element_blank(), axis.text.y.left = element_blank(), axis.title.x.top = element_blank(), axis.title.x.bottom = element_blank())+
  annotate("rect", xmin = 14, xmax = 21, ymin = 0.001, ymax = 0.01, 
           fill = "white", color = "black") +  # Draw the white box with a black border
  annotate("text", x = 17.5, y = 0.0035, label = "c", size = 6.5, fontface = "bold") 

p1 <- p1+theme_bw()+
  theme(
    axis.text.x = element_text(size = 14),  # Adjust font size for x-axis text
    axis.text.y = element_text(size = 14),  # Adjust font size for y-axis text
    axis.title.y.left = element_text(size=14), # Adjust font size for y-axis title
    axis.title.y.right = element_text(size=14),
    axis.title.x.top = element_text(size = 14), # Adjust font size for x-axis title
    axis.title.x.bottom = element_text(size = 14) 
  )
p2 <- p2+theme_bw()+
  theme(
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14), 
    axis.title.x.top = element_text(size = 14),
    axis.title.x.bottom = element_text(size = 14)
  )
p3 <- p3+theme_bw()+
  theme(
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),
    axis.title.y.left = element_text(size=14),
    axis.title.y.right = element_text(size=14),
    axis.title.x.top = element_text(size = 14),
    axis.title.x.bottom = element_text(size = 14)
  )
# Create a list of plots to arrange
plots <- list(p1, p2, p3)

# Define the layout matrix
layout_matrix <- "
abc
"

# Combine plots with specified layout
combined_plot <- wrap_plots(plots, design = layout_matrix) +
  plot_layout(heights = c(2, 2, 2), # Heights of the two rows
              widths = c(2.5, 2.5,2.5)) 


# Print the combined plot
combined_plot

# Save the final combined plot
ggsave("RC_combinedv3.png", plot = combined_plot)
