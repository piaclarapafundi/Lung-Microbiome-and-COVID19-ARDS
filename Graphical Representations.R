# SET UP DIRECTORY

setwd("C:\\Users\\00654632\\Desktop\\USB SETTEMBRE 2022\\PROGETTI GSTeP\\De Pascale Anestesia - analisi\\Analisi Settembre 2022")

# LOAD REQUIRED PACKAGES
# Note, that if packages are not installed yet, you have to install them by using the code - install.packages("name of the package")

library(readxl)
library(LabRS)
library(survival)
library(survminer)
library(coxphw)
library(data.table)
library(coin)
library(foreign)
library(glmnet)
library(MASS)
library(RVAideMemoire)
library(ggpubr)
library(ggstatsplot)
library(ggplot2)
library(ggprism)

## LOAD REQUIRED DATASET
dati <- read_excel("dataset_ott_22 - KM.xlsx")
dim(dati)
colnames(dati)


# Code to reproduce Kaplan-Meier Curves and Cumulative Incidence Functions

##################################################
############SURVIVAL KAPLAN MODEL VentRatio#######
##################################################

####overall Survival

fit <- survfit(Surv(OS, death) ~ VR, data = dati)
print(fit)

ggsurvplot(fit,dati,
          pval = TRUE, conf.int = TRUE,
	     surv.scale="percent",#transform y-axis in percentage
          ylim=c(0,1), # set y-axis from 0% to 100%
          break.y.by = 0.2, # Y axis breaks
          risk.table = TRUE,fontsize=4, # Add risk table
          risk.table.col = "strata",risk.table.height = .20, # Change risk table color by groups
          linetype = "strata", # Change line type by groups
           break.time.by = 28, surv.median.line = "hv", # Specify median survival
          ggtheme = theme_classic(), # Change ggplot2 theme
          pval.method = TRUE, log.rank.weights = "1", pval.coord = c(13, 0.00), pval.size = 4,
          pval.method.coord = c(-7, 0.00), pval.method.size = 4,
          xlab = "time from BAL to discharge (days)",ylab = "Overall Survival, %",
font.caption = c(12, "bold"),
   font.x = c(12, "bold"),
   font.y = c(12, "bold"),
   font.tickslab = c(12, "bold"),
   palette = c("green4", "red2"))


####Cumulative risk of Weaning from Mechanical ventilation

fit <- survfit(Surv(TimetoWean, IMVweaning) ~ VR, data = dati)
print(fit)

ggsurvplot(fit,dati,fun = "event",pval = TRUE, conf.int = TRUE,
	     surv.scale="percent",#transform y-axis in percentage
          ylim=c(0,1), # set y-axis from 0% to 100%
          break.y.by = 0.2, # Y axis breaks
          risk.table = TRUE,fontsize=4, # Add risk table
          risk.table.col = "strata",risk.table.height = .20, # Change risk table color by groups
          linetype = "strata", # Change line type by groups
           break.time.by = 7, surv.median.line = "hv", # Specify median survival
          ggtheme = theme_classic(), # Change ggplot2 theme
          pval.method = TRUE, log.rank.weights = "1", pval.coord = c(3.6, 0.97), pval.size = 4,
          pval.method.coord = c(0.0, 0.97), pval.method.size = 4,
          xlab = "time between BAL and wean from MV (days)",ylab = "Cumulative risk of weaning from MV, %",
          font.caption = c(12, "bold"),
          font.x = c(12, "bold"),
         font.y = c(12, "bold"),
          font.tickslab = c(12, "bold"),
          palette = c("green4", "red2"))



##################################################
############SURVIVAL KAPLAN MODEL Compliance######
##################################################

####overall Survival

fit <- survfit(Surv(OS, death) ~ CRS_PBW, data = dati)
print(fit)

ggsurvplot(fit,dati,
          pval = TRUE, conf.int = TRUE,
	     surv.scale="percent",#transform y-axis in percentage
          ylim=c(0,1), # set y-axis from 0% to 100%
          break.y.by = 0.2, # Y axis breaks
          risk.table = TRUE,fontsize=4, # Add risk table
          risk.table.col = "strata",risk.table.height = .20, # Change risk table color by groups
          linetype = "strata", # Change line type by groups
           break.time.by = 28, surv.median.line = "hv", # Specify median survival
          ggtheme = theme_classic(), # Change ggplot2 theme
          pval.method = TRUE, log.rank.weights = "1", pval.coord = c(13, 0.00), pval.size = 4,
          pval.method.coord = c(-7, 0.00), pval.method.size = 4,
          xlab = "time from BAL to discharge (days)",ylab = "Overall Survival, %",
font.caption = c(12, "bold"),
   font.x = c(12, "bold"),
   font.y = c(12, "bold"),
   font.tickslab = c(12, "bold"),
   palette = c("green4", "red2"))


####Cumulative risk of Weaning from Mechanical ventilation

fit <- survfit(Surv(TimetoWean, IMVweaning) ~ CRS_PBW, data = dati)
print(fit)

ggsurvplot(fit,dati,fun = "event",pval = TRUE, conf.int = TRUE,
	     surv.scale="percent",#transform y-axis in percentage
          ylim=c(0,1), # set y-axis from 0% to 100%
          break.y.by = 0.2, # Y axis breaks
          risk.table = TRUE,fontsize=4, # Add risk table
          risk.table.col = "strata",risk.table.height = .20, # Change risk table color by groups
          linetype = "strata", # Change line type by groups
           break.time.by = 7, surv.median.line = "hv", # Specify median survival
          ggtheme = theme_classic(), # Change ggplot2 theme
          pval.method = TRUE, log.rank.weights = "1", pval.coord = c(4, 0.97), pval.size = 4,
          pval.method.coord = c(0.0, 0.97), pval.method.size = 4,
          xlab = "time between BAL and wean from MV (days)",ylab = "Cumulative risk of weaning from MV, %",
          font.caption = c(12, "bold"),
          font.x = c(12, "bold"),
         font.y = c(12, "bold"),
          font.tickslab = c(12, "bold"),
          palette = c("green4", "red2"))



##############################################
############SURVIVAL KAPLAN MODEL CRS_VR######
##############################################

####overall Survival

fit <- survfit(Surv(OS, death) ~ CRS_VR, data = dati)
print(fit)

ggsurvplot(fit,dati,
          pval = TRUE, conf.int = TRUE,
	     surv.scale="percent",#transform y-axis in percentage
          ylim=c(0,1), # set y-axis from 0% to 100%
          break.y.by = 0.2, # Y axis breaks
          risk.table = TRUE,fontsize=4, # Add risk table
          risk.table.col = "strata",risk.table.height = .20, # Change risk table color by groups
          linetype = "strata", # Change line type by groups
           break.time.by = 28, surv.median.line = "hv", # Specify median survival
          ggtheme = theme_classic(), # Change ggplot2 theme
          pval.method = TRUE, log.rank.weights = "1", pval.coord = c(13, 0.00), pval.size = 4,
          pval.method.coord = c(-7, 0.00), pval.method.size = 4,
          xlab = "time from BAL to discharge (days)",ylab = "Overall Survival, %",
font.caption = c(12, "bold"),
   font.x = c(12, "bold"),
   font.y = c(12, "bold"),
   font.tickslab = c(12, "bold"),
   palette = c("green4", "red2", "orange4", "brown"))


####Cumulative risk of Weaning from Mechanical ventilation

fit <- survfit(Surv(TimetoWean, IMVweaning) ~ CRS_VR, data = dati)
print(fit)

ggsurvplot(fit,dati,fun = "event",pval = TRUE, conf.int = TRUE,
	     surv.scale="percent",#transform y-axis in percentage
          ylim=c(0,1), # set y-axis from 0% to 100%
          break.y.by = 0.2, # Y axis breaks
          risk.table = TRUE,fontsize=4, # Add risk table
          risk.table.col = "strata",risk.table.height = .20, # Change risk table color by groups
          linetype = "strata", # Change line type by groups
           break.time.by = 7, surv.median.line = "hv", # Specify median survival
          ggtheme = theme_classic(), # Change ggplot2 theme
          pval.method = TRUE, log.rank.weights = "1", pval.coord = c(4, 0.97), pval.size = 4,
          pval.method.coord = c(0.0, 0.97), pval.method.size = 4,
          xlab = "time between BAL and wean from MV (days)",ylab = "Cumulative risk of weaning from MV, %",
          font.caption = c(12, "bold"),
          font.x = c(12, "bold"),
         font.y = c(12, "bold"),
          font.tickslab = c(12, "bold"),
          palette = c("green4", "red2", "orange4", "brown"))
