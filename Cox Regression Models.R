# SET THE DIRECTORY
setwd("D:\\PROGETTI GSTeP\\De Pascale Anestesia - analisi\\Analisi Ottobre 2022")

# LOAD THE REQUIRE PACKAGES
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
library(rstatix)

# LOAD THE DATASET

dati <- read_excel("dataset_ott_22.xlsx")
dim(dati)
colnames(dati)

# R Code Data Analysis for Univariable Model on Overall Mortality
# The same code will reproduce the analysis even on the "weaning from mechanical ventilation, by replacing the "time" and "outcome" variables with
# "TimetoWean" and "IMVweaning""

#######################################
###UNIVARIABLE COX MODEL SURVIVAL######
#######################################

###AGE

byf.shapiro(dati$Age~dati$death)
tapply(dati$Age, dati$death, summary)
tapply(dati$Age, dati$death, sd,na.rm=T)

m <- coxph(Surv(dati$OS, dati$death) ~ Age, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###SEX

table(dati$Sex,dati$death)
percent(table(dati$Sex,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS,death) ~ factor(Sex), data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###SAPSII

byf.shapiro(dati$SAPSII~dati$death)
tapply(dati$SAPSII, dati$death, summary)
tapply(dati$SAPSII, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ SAPSII, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###CCI

byf.shapiro(dati$CCI~dati$death)
tapply(dati$CCI, dati$death, summary)

m <- coxph(Surv(OS, death) ~ CCI, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###hospitalization

table(dati$hospitalization,dati$death)
percent(table(dati$hospitalization,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ hospitalization, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Arterial Hypertension

table(dati$AH,dati$death)
percent(table(dati$AH,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ AH, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###CHD

table(dati$CHD,dati$death)
percent(table(dati$CHD,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death)~ CHD, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###CAD

table(dati$CAD,dati$death)
percent(table(dati$CAD,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ CAD, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

m <- coxphw(Surv(OS, death) ~ CAD, data=dati, template="AHR")
summary(m)

###COPD

table(dati$COPD,dati$death)
percent(table(dati$COPD,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ COPD, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Chronic Vasculopathy

table(dati$ChronicVasculopathy,dati$death)
percent(table(dati$ChronicVasculopathy,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ ChronicVasculopathy, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Diabetes

table(dati$Diabete,dati$death)
percent(table(dati$Diabete,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ Diabete, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### Neoplasms

table(dati$Neo,dati$death)
percent(table(dati$Neo,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ Neo, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Immunosuppression

table(dati$Immunosuo,dati$death)
percent(table(dati$Immunosuo,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ Immunosuo, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### Time from symptoms

byf.shapiro(dati$TimefromSymptoms~dati$death)
tapply(dati$TimefromSymptoms, dati$death, summary)
tapply(dati$TimefromSymptoms, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ dati$TimefromSymptoms, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Ongoing steroid therapy

table(dati$OngoingSteroids,dati$death)
percent(table(dati$OngoingSteroids,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ OngoingSteroids, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### IL6 Inhibitors administration

table(dati$IL6_i_somm,dati$death)
percent(table(dati$IL6_i_somm,dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ IL6_i_somm, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### White Blood Cells

byf.shapiro(dati$WBC~dati$death)
tapply(dati$WBC, dati$death, summary)
tapply(dati$WBC, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ WBC, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### Neutrophils

byf.shapiro(dati$Neu~dati$death)
tapply(dati$Neu, dati$death, summary)
tapply(dati$Neu, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ Neu, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### Lymphocytes

byf.shapiro(dati$Lympho~dati$death)
tapply(dati$Lympho, dati$death, summary)

m <- coxph(Surv(OS, death) ~ Lympho, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### D dimer

byf.shapiro(dati$Ddimer~dati$death)
tapply(dati$Ddimer, dati$death, summary)

m <- coxph(Surv(OS, death) ~ dati$Ddimer, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### Fibrinogen

byf.shapiro(dati$Fibrinogen~dati$death)
tapply(dati$Fibrinogen, dati$death, summary)
tapply(dati$Fibrinogen, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ Fibrinogen, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### LDH

byf.shapiro(dati$LDH~dati$death)
tapply(dati$LDH, dati$death, summary)
tapply(dati$LDH, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ LDH, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### Procalcitonin

byf.shapiro(dati$PCT~dati$death)
tapply(dati$PCT, dati$death, summary)

m <- coxph(Surv(OS, death) ~ PCT, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### C-reactive protein

byf.shapiro(dati$CRP~dati$death)
tapply(dati$CRP, dati$death, summary)

m <- coxph(Surv(OS, death) ~ CRP, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

#### P/F

byf.shapiro(dati$P_F~dati$death)
tapply(dati$P_F, dati$death, summary)
tapply(dati$P_F, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ P_F, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### PEEP

byf.shapiro(dati$PEEP~dati$death)
tapply(dati$PEEP, dati$death, summary)

m <- coxph(Surv(OS, death) ~ PEEP, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### Pplat

byf.shapiro(dati$Pplat~dati$death)
tapply(dati$Pplat, dati$death, summary)
tapply(dati$Pplat, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ Pplat, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### RR

byf.shapiro(dati$RR~dati$death)
tapply(dati$RR, dati$death, summary)
tapply(dati$RR, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ RR, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###PaCO2

byf.shapiro(dati$PaCO2~dati$death)
tapply(dati$PaCO2, dati$death, summary)
tapply(dati$PaCO2, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ PaCO2, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### Vt/PBW

byf.shapiro(dati$Vt_PBW~dati$death)
tapply(dati$Vt_PBW, dati$death, summary)
tapply(dati$Vt_PBW, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ Vt_PBW, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### Driving Pressure

byf.shapiro(dati$DrivingPress~dati$death)
tapply(dati$DrivingPress, dati$death, summary)
tapply(dati$DrivingPress, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ DrivingPress, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### OSI

byf.shapiro(dati$OSI~dati$death)
tapply(dati$OSI, dati$death, summary)

m <- coxph(Surv(OS, death) ~ OSI, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### CRS

byf.shapiro(dati$CRS~dati$death)
tapply(dati$CRS, dati$death, summary)
tapply(dati$CRS, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ CRS, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### High-Low CRS/PBW 

table(dati$CRS_PBW_dic, dati$death)
percent(table(dati$CRS_PBW_dic, dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ factor(CRS_PBW_dic), data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### ERS/PBW

byf.shapiro(dati$Elrs_PBW~dati$death)
tapply(dati$Elrs_PBW, dati$death, summary)

m <- coxph(Surv(OS, death) ~ Elrs_PBW, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### R/I

byf.shapiro(dati$R_I~dati$death)
tapply(dati$R_I, dati$death, summary)
tapply(dati$R_I, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ R_I, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### CMV

byf.shapiro(dati$MinutVentCorr~dati$death)
tapply(dati$MinutVentCorr, dati$death, summary)

m <- coxph(Surv(OS, death) ~ MinutVentCorr, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


### High-Low Ventilatory Ratio

table(dati$VentRatio_dic, dati$death)
percent(table(dati$VentRatio_dic, dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ factor(VentRatio_dic), data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### CRS_VR

table(dati$CRS_VR, dati$death)
percent(table(dati$CRS_VR, dati$death), margin=2, digits=1)

m <- coxph(Surv(OS, death) ~ factor(CRS_VR), data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### 4*Driving Pressure+RR

byf.shapiro(dati$DP4_RR~dati$death)
tapply(dati$DP4_RR, dati$d28_death, summary)
tapply(dati$DP4_RR, dati$d28_death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ dati$DP4_RR, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

####MV_PreBAL

byf.shapiro(dati$MV_PreBAL ~ dati$death)
tapply(dati$MV_PreBAL, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ MV_PreBAL, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###DurationICU

byf.shapiro(dati$DurationICU ~ dati$death)
tapply(dati$DurationICU, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ DurationICU, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

m <- coxphw(Surv(OS, death) ~ DurationICU, data=dati, template="AHR")
summary(m)

###DurationHosp

byf.shapiro(dati$DurationHosp ~ dati$death)
tapply(dati$DurationHosp, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ DurationHosp, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

m <- coxphw(Surv(OS, death) ~ DurationHosp, data=dati, template="AHR")
summary(m)

############################
###MICROBIOME ANALYSIS######
############################

###Observed

byf.shapiro(dati$observed ~ dati$death)
tapply(dati$observed, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ observed, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###shannon diversity

byf.shapiro(dati$diversity_shannon ~ dati$death)
tapply(dati$diversity_shannon, dati$death, summary)
tapply(dati$diversity_shannon, dati$death, sd,na.rm=T)

m <- coxph(Surv(OS, death) ~ diversity_shannon, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

### evenness_pielou

byf.shapiro(dati$evenness_pielou ~ dati$death)
tapply(dati$evenness_pielou, dati$death, summary)
tapply(dati$evenness_pielou, dati$death, sd,na.rm=T)            

m <- coxph(Surv(OS, death) ~ evenness_pielou, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

############################
###PHYLA ABUNDANCY##########
############################

###Actinobacteria

byf.shapiro(dati$actinobacteria ~ dati$death)
tapply(dati$actinobacteria, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ actinobacteria, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Bacteroides

byf.shapiro(dati$bacteroides ~ dati$death)
tapply(dati$bacteroides, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ bacteroides, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Firmicutes

byf.shapiro(dati$firmicutes ~ dati$death)
tapply(dati$firmicutes, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ firmicutes, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Fusobacteria

byf.shapiro(dati$fusobacteria ~ dati$death)
tapply(dati$fusobacteria, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ fusobacteria, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Proteobacteria

byf.shapiro(dati$proteobacteria ~ dati$death)
tapply(dati$proteobacteria, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ proteobacteria, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

############################
###GENERA ABUNDANCY#########
############################

###Acinetobacter

byf.shapiro(dati$acinetobacter ~ dati$death)
tapply(dati$acinetobacter, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ acinetobacter, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Bacteroides

byf.shapiro(dati$bacteroides ~ dati$death)
tapply(dati$bacteroides, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ bacteroides, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Bifidobacterium

byf.shapiro(dati$bifidobacterium ~ dati$death)
tapply(dati$bifidobacterium, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ bifidobacterium, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Corynebacterium_1

byf.shapiro(dati$corynebacterium_1 ~ dati$death)
tapply(dati$corynebacterium_1, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ corynebacterium_1, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Cutibacterium

byf.shapiro(dati$cutibacterium ~ dati$death)
tapply(dati$cutibacterium, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ cutibacterium, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Escherichia-Shigella

byf.shapiro(dati$escherichia_shigella ~ dati$death)
tapply(dati$escherichia_shigella, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ escherichia_shigella, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Faecalibacterium

byf.shapiro(dati$faecalibacterium ~ dati$death)
tapply(dati$faecalibacterium, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ faecalibacterium, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

###Finegoldia

byf.shapiro(dati$finegoldia ~ dati$death)
tapply(dati$finegoldia, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ finegoldia, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Granulicatella

byf.shapiro(dati$granulicatella ~ dati$death)
tapply(dati$granulicatella, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ granulicatella, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Haemophilus

byf.shapiro(dati$haemophilus ~ dati$death)
tapply(dati$haemophilus, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ haemophilus, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Klebsiella

byf.shapiro(dati$klebsiella ~ dati$death)
tapply(dati$klebsiella, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ klebsiella, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)



###Lactobacillus

byf.shapiro(dati$lactobacillus ~ dati$death)
tapply(dati$lactobacillus, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ lactobacillus, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)



###Novosphingobium

byf.shapiro(dati$novosphingobium ~ dati$death)
tapply(dati$novosphingobium, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ novosphingobium, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Paenibacillus

byf.shapiro(dati$paenibacillus ~ dati$death)
tapply(dati$paenibacillus, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ paenibacillus, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

m <- coxphw(Surv(OS, death) ~ paenibacillus, data=dati, template="AHR")
summary(m)

###Peptostreptococcus

byf.shapiro(dati$peptostreptococcus ~ dati$death)
tapply(dati$bifidobacterium, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ peptostreptococcus, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Prevotella

byf.shapiro(dati$prevotella ~ dati$death)
tapply(dati$prevotella, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ prevotella, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Prevotella_7

byf.shapiro(dati$prevotella_7 ~ dati$death)
tapply(dati$prevotella_7, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ prevotella_7, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Pseudomonas

byf.shapiro(dati$pseudomonas ~ dati$death)
tapply(dati$pseudomonas, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ pseudomonas, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)

m <- coxphw(Surv(OS, death) ~ pseudomonas, data=dati,template="AHR")
summary(m)

###Staphylococccus

byf.shapiro(dati$staphylococcus ~ dati$death)
tapply(dati$staphylococcus, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ staphylococcus, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)


###Streptococcus

byf.shapiro(dati$streptococcus ~ dati$death)
tapply(dati$streptococcus, dati$death, summary)
            
m <- coxph(Surv(OS, death) ~ streptococcus, data=dati)
summary(m)
test.ph <- cox.zph(m); test.ph
library(survminer)
ggcoxzph(test.ph)
