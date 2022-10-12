# Set the R space
setwd("D:\\PROGETTI GSTeP\\De Pascale Anestesia - analisi\\Analisi Ottobre 2022")

# Load the packages needed for the analysis

library(readxl)
library(LabRS)
library(survival)
library(survminer)
library(coxphw)
library(data.table)
library(coin)
library(foreign)
library(MASS)
library(RVAideMemoire)
library(rstatix)

# Load the dataset and verify the dimension of the dataset and the names of the variables

dati <- read_excel("dataset_ott_22.xlsx")
dim(dati)
colnames(dati)

# This is the code to reproduce the output obtained 

###############################
######DESCRIPTIVES OVERALL#####
###############################

table(dati$Cluster)
percent(table(dati$Cluster))

shapiro.test(dati$Age)
summary(dati$Age)
sd(dati$Age,na.rm=T)

table(dati$Sex)
percent(table(dati$Sex))

shapiro.test(dati$SAPSII)
summary(dati$SAPSII)
sd(dati$SAPSII,na.rm=T)

shapiro.test(dati$CCI)
summary(dati$CCI)

table(dati$hospitalization)
percent(table(dati$hospitalization))

table(dati$AH)
percent(table(dati$AH))

table(dati$CHD)
percent(table(dati$CHD))

table(dati$CAD)
percent(table(dati$CAD))

table(dati$COPD)
percent(table(dati$COPD))

table(dati$ChronicVasculopathy)
percent(table(dati$ChronicVasculopathy))

table(dati$Diabete)
percent(table(dati$Diabete))

table(dati$Neo)
percent(table(dati$Neo))

table(dati$Immunosuo)
percent(table(dati$Immunosuo))

shapiro.test(dati$TimefromSymptoms)
summary(dati$TimefromSymptoms)
sd(dati$TimefromSymptoms, na.rm=T)

table(dati$OngoingSteroids)
percent(table(dati$OngoingSteroids))

table(dati$IL6_i_somm)
percent(table(dati$IL6_i_somm))

shapiro.test(dati$WBC)
summary(dati$WBC)
sd(dati$WBC,na.rm=T)

shapiro.test(dati$Neu)
summary(dati$Neu)
sd(dati$Neu,na.rm=T)

shapiro.test(dati$Lympho)
summary(dati$Lympho)

shapiro.test(dati$Ddimer)
summary(dati$Ddimer)

shapiro.test(dati$Fibrinogen)
summary(dati$Fibrinogen)
sd(dati$Fibrinogen,na.rm=T)

shapiro.test(dati$LDH)
summary(dati$LDH)

shapiro.test(dati$PCT)
summary(dati$PCT)

shapiro.test(dati$CRP)
summary(dati$CRP)
sd(dati$CRP, na.rm=T)

shapiro.test(dati$P_F)
summary(dati$P_F)

shapiro.test(dati$PEEP)
summary(dati$PEEP)

shapiro.test(dati$Pplat)
summary(dati$Pplat)
sd(dati$Pplat,na.rm=T)

shapiro.test(dati$RR)
summary(dati$RR)
sd(dati$RR, na.rm=T)

shapiro.test(dati$PaCO2)
summary(dati$PaCO2)
sd(dati$PaCO2, na.rm=T)

shapiro.test(dati$Vt_PBW)
summary(dati$Vt_PBW)

shapiro.test(dati$DrivingPress)
summary(dati$DrivingPress)
sd(dati$DrivingPress, na.rm=T)

shapiro.test(dati$OSI)
summary(dati$OSI)

shapiro.test(dati$CRS)
summary(dati$CRS)

shapiro.test(dati$CRS_PBW)
summary(dati$CRS_PBW)
sd(dati$CRS_PBW,na.rm=T)

table(dati$CRS_PBW_dic)
percent(table(dati$CRS_PBW_dic))

shapiro.test(dati$Elrs_PBW)
summary(dati$Elrs_PBW)

shapiro.test(dati$R_I)
summary(dati$R_I)
sd(dati$R_I, na.rm=T)

shapiro.test(dati$MinutVentCorr)
summary(dati$MinutVentCorr)
sd(dati$MinutVentCorr, na.rm=T)

shapiro.test(dati$VentRatio)
summary(dati$VentRatio)

table(dati$VentRatio_dic)
percent(table(dati$VentRatio_dic))

table(dati$CRS_VR)
percent(table(dati$CRS_VR))

shapiro.test(dati$DP4_RR)
summary(dati$DP4_RR)
sd(dati$DP4_RR,na.rm=T)

table(dati$d28_death)
percent(table(dati$d28_death))

table(dati$death)
percent(table(dati$death))

shapiro.test(dati$OS)
summary(dati$OS)

table(dati$d28_IMVweaning)
percent(table(dati$d28_IMVweaning))

table(dati$IMVweaning)
percent(table(dati$IMVweaning))

shapiro.test(dati$TimetoWean)
summary(dati$TimetoWean)

shapiro.test(dati$MV_PreBAL)
summary(dati$MV_PreBAL)
            
shapiro.test(dati$DurationICU)
summary(dati$DurationICU)
            
shapiro.test(dati$DurationHosp)
summary(dati$DurationHosp)

###############################
###DESCRIPTIVES MICROBIOLOGY###
###############################

table(dati$cephalosporin)
percent(table(dati$cephalosporin))

table(dati$beta_lactam_inhib)
percent(table(dati$beta_lactam_inhib))

table(dati$macrolides)
percent(table(dati$macrolides))

table(dati$fluoroquinolones)
percent(table(dati$fluoroquinolones))

table(dati$others_antib)
percent(table(dati$others_antib))

table(dati$BAL_Aspergillus)
percent(table(dati$BAL_Aspergillus))

table(dati$BAL_Candida)
percent(table(dati$BAL_Candida))

table(dati$BAL_Bacterial)
percent(table(dati$BAL_Bacterial))

table(dati$Saureus)
percent(table(dati$Saureus))

table(dati$Kl_pneum)
percent(table(dati$Kl_pneum))

table(dati$Ps_aerug)
percent(table(dati$Ps_aerug))

table(dati$MRSA)
percent(table(dati$MRSA))

table(dati$CRE)
percent(table(dati$CRE))

table(dati$other_germs)
percent(table(dati$other_germs))

table(dati$other_type)
percent(table(dati$other_type))

table(dati$antibiotic_type)
percent(table(dati$antibiotic_type))

###############################
###DESCRIPTIVES MICROBIOME#####
###############################

shapiro.test(dati$observed)
summary(dati$observed)
sd(dati$observed, na.rm=T)

shapiro.test(dati$diversity_shannon)
summary(dati$diversity_shannon)
sd(dati$diversity_shannon, na.rm=T)

shapiro.test(dati$evenness_pielou)
summary(dati$evenness_pielou)
sd(dati$evenness_pielou, na.rm=T)


############################################
####DESCRIPTIVES per High/Low VentRatio#####
############################################

###################
######Clinical#####
###################

byf.shapiro(dati$Age ~ dati$VentRatio_dic)
tapply(dati$Age, dati$VentRatio_dic, summary)
tapply(dati$Age, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$Age ~ dati$VentRatio_dic)

table(dati$Sex, dati$VentRatio_dic)
percent(table(dati$Sex, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$Sex, dati$VentRatio_dic)

byf.shapiro(dati$SAPSII ~ dati$VentRatio_dic)
tapply(dati$SAPSII, dati$VentRatio_dic, summary)
tapply(dati$SAPSII, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$SAPSII ~ dati$VentRatio_dic)

byf.shapiro(dati$CCI ~ dati$VentRatio_dic)
tapply(dati$CCI, dati$VentRatio_dic, summary)
wilcox.test(dati$CCI ~ dati$VentRatio_dic)
wilcoxsign_test(dati$CCI ~ dati$VentRatio_dic)

table(dati$hospitalization, dati$VentRatio_dic)
percent(table(dati$hospitalization, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$hospitalization, dati$VentRatio_dic)

table(dati$AH, dati$VentRatio_dic)
percent(table(dati$AH, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$AH, dati$VentRatio_dic)

table(dati$CHD, dati$VentRatio_dic)
percent(table(dati$CHD, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$CHD, dati$VentRatio_dic)

table(dati$CAD, dati$VentRatio_dic)
percent(table(dati$CAD, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$CAD, dati$VentRatio_dic)

table(dati$COPD, dati$VentRatio_dic)
percent(table(dati$COPD, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$COPD, dati$VentRatio_dic)

table(dati$ChronicVasculopathy, dati$VentRatio_dic)
percent(table(dati$ChronicVasculopathy, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$ChronicVasculopathy, dati$VentRatio_dic)

table(dati$Diabete, dati$VentRatio_dic)
percent(table(dati$Diabete, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$Diabete, dati$VentRatio_dic)

table(dati$Neo, dati$VentRatio_dic)
percent(table(dati$Neo, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$Neo, dati$VentRatio_dic)

table(dati$Immunosuo, dati$VentRatio_dic)
percent(table(dati$Immunosuo, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$Immunosuo, dati$VentRatio_dic)

byf.shapiro(dati$TimefromSymptoms ~ dati$VentRatio_dic)
tapply(dati$TimefromSymptoms, dati$VentRatio_dic, summary)
tapply(dati$TimefromSymptoms, dati$VentRatio_dic, sd,na.rm=T)
t.test(dati$TimefromSymptoms ~ dati$VentRatio_dic)

table(dati$OngoingSteroids, dati$VentRatio_dic)
percent(table(dati$OngoingSteroids, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$OngoingSteroids, dati$VentRatio_dic)

table(dati$IL6_i_somm, dati$VentRatio_dic)
percent(table(dati$IL6_i_somm, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$IL6_i_somm, dati$VentRatio_dic)

byf.shapiro(dati$WBC ~ dati$VentRatio_dic)
tapply(dati$WBC, dati$VentRatio_dic, summary)
tapply(dati$WBC, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$WBC ~ dati$VentRatio_dic)

byf.shapiro(dati$Neu ~ dati$VentRatio_dic)
tapply(dati$Neu, dati$VentRatio_dic, summary)
tapply(dati$Neu, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$Neu ~ dati$VentRatio_dic)

byf.shapiro(dati$Lympho ~ dati$VentRatio_dic)
tapply(dati$Lympho, dati$VentRatio_dic, summary)
wilcox.test(dati$Lympho ~ dati$VentRatio_dic)
wilcoxsign_test(dati$Lympho ~ dati$VentRatio_dic)

byf.shapiro(dati$Ddimer ~ dati$VentRatio_dic)
tapply(dati$Ddimer, dati$VentRatio_dic, summary)
wilcox.test(dati$Ddimer ~ dati$VentRatio_dic)
wilcoxsign_test(dati$Ddimer ~ dati$VentRatio_dic)

byf.shapiro(dati$Fibrinogen ~ dati$VentRatio_dic)
tapply(dati$Fibrinogen, dati$VentRatio_dic, summary)
tapply(dati$Fibrinogen, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$Fibrinogen ~ dati$VentRatio_dic)

byf.shapiro(dati$LDH ~ dati$VentRatio_dic)
tapply(dati$LDH, dati$VentRatio_dic, summary)
wilcox.test(dati$LDH ~ dati$VentRatio_dic)
wilcoxsign_test(dati$LDH ~ dati$VentRatio_dic)

byf.shapiro(dati$PCT ~ dati$VentRatio_dic)
tapply(dati$PCT, dati$VentRatio_dic, summary)
wilcox.test(dati$PCT ~ dati$VentRatio_dic)
wilcoxsign_test(dati$PCT ~ dati$VentRatio_dic)

byf.shapiro(dati$CRP ~ dati$VentRatio_dic)
tapply(dati$CRP, dati$VentRatio_dic, summary)
tapply(dati$CRP, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$CRP ~ dati$VentRatio_dic)

byf.shapiro(dati$P_F ~ dati$VentRatio_dic)
tapply(dati$P_F, dati$VentRatio_dic, summary)
wilcox.test(dati$P_F ~ dati$VentRatio_dic)
wilcoxsign_test(dati$P_F ~ dati$VentRatio_dic)

byf.shapiro(dati$PEEP ~ dati$VentRatio_dic)
tapply(dati$PEEP, dati$VentRatio_dic, summary)
wilcox.test(dati$PEEP ~ dati$VentRatio_dic)
wilcoxsign_test(dati$PEEP ~ dati$VentRatio_dic)

byf.shapiro(dati$Pplat ~ dati$VentRatio_dic)
tapply(dati$Pplat, dati$VentRatio_dic, summary)
tapply(dati$Pplat,dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$Pplat ~ dati$VentRatio_dic)

byf.shapiro(dati$RR ~ dati$VentRatio_dic)
tapply(dati$RR, dati$VentRatio_dic, summary)
tapply(dati$RR, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$RR ~ dati$VentRatio_dic)

byf.shapiro(dati$PaCO2 ~ dati$VentRatio_dic)
tapply(dati$PaCO2, dati$VentRatio_dic, summary)
tapply(dati$PaCO2, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$PaCO2 ~ dati$VentRatio_dic)

byf.shapiro(dati$Vt_PBW ~ dati$VentRatio_dic)
tapply(dati$Vt_PBW, dati$VentRatio_dic, summary)
wilcox.test(dati$Vt_PBW ~ dati$VentRatio_dic)
wilcoxsign_test(dati$Vt_PBW ~ dati$VentRatio_dic)

byf.shapiro(dati$DrivingPress ~ dati$VentRatio_dic)
tapply(dati$DrivingPress, dati$VentRatio_dic, summary)
tapply(dati$DrivingPress, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$DrivingPress ~ dati$VentRatio_dic)

byf.shapiro(dati$OSI ~ dati$VentRatio_dic)
tapply(dati$OSI, dati$VentRatio_dic, summary)
wilcox.test(dati$OSI ~ dati$VentRatio_dic)
wilcoxsign_test(dati$OSI ~ dati$VentRatio_dic)

byf.shapiro(dati$CRS ~ dati$VentRatio_dic)
tapply(dati$CRS, dati$VentRatio_dic, summary)
tapply(dati$CRS, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$CRS ~ dati$VentRatio_dic)

table(dati$CRS_PBW_dic, dati$VentRatio_dic)
percent(table(dati$CRS_PBW_dic, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$CRS_PBW_dic, dati$VentRatio_dic)

byf.shapiro(dati$Elrs_PBW ~ dati$VentRatio_dic)
tapply(dati$Elrs_PBW, dati$VentRatio_dic, summary)
wilcox.test(dati$Elrs_PBW ~ dati$VentRatio_dic)
wilcoxsign_test(dati$Elrs_PBW ~ dati$VentRatio_dic)

byf.shapiro(dati$R_I ~ dati$VentRatio_dic)
tapply(dati$R_I, dati$VentRatio_dic, summary)
tapply(dati$R_I, dati$VentRatio_dic, sd,na.rm=T)
t.test(dati$R_I ~ dati$VentRatio_dic)

byf.shapiro(dati$MinutVentCorr ~ dati$VentRatio_dic)
tapply(dati$MinutVentCorr, dati$VentRatio_dic, summary)
tapply(dati$MinutVentCorr, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$MinutVentCorr ~ dati$VentRatio_dic)

byf.shapiro(dati$DP4_RR ~ dati$VentRatio_dic)
tapply(dati$DP4_RR, dati$VentRatio_dic, summary)
tapply(dati$DP4_RR, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$DP4_RR ~ dati$VentRatio_dic)

table(dati$d28_death, dati$VentRatio_dic)
percent(table(dati$d28_death, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$d28_death, dati$VentRatio_dic)

table(dati$death, dati$VentRatio_dic)
percent(table(dati$death, dati$VentRatio_dic), margin=2, digits=1)
survfit(Surv(OS, death) ~ VentRatio_dic, data = dati)
survdiff(Surv(OS, death) ~ VentRatio_dic, data = dati)

byf.shapiro(dati$OS ~ dati$VentRatio_dic)
tapply(dati$OS, dati$VentRatio_dic, summary)
wilcox.test(dati$OS ~ dati$VentRatio_dic)
wilcoxsign_test(dati$OS ~ dati$VentRatio_dic)

table(dati$d28_IMVweaning, dati$VentRatio_dic)
percent(table(dati$d28_IMVweaning, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$d28_IMVweaning, dati$VentRatio_dic)

table(dati$IMVweaning, dati$VentRatio_dic)
percent(table(dati$IMVweaning, dati$VentRatio_dic), margin=2, digits=1)
survfit(Surv(TimetoWean, IMVweaning) ~ VentRatio_dic, data = dati)
survdiff(Surv(TimetoWean, IMVweaning) ~ VentRatio_dic, data = dati)

byf.shapiro(dati$TimetoWean ~ dati$VentRatio_dic)
tapply(dati$TimetoWean, dati$VentRatio_dic, summary)
wilcox.test(dati$TimetoWean ~ dati$VentRatio_dic)
wilcoxsign_test(dati$TimetoWean ~ dati$VentRatio_dic)

byf.shapiro(dati$MV_PreBAL ~ dati$VentRatio_dic)
tapply(dati$MV_PreBAL, dati$VentRatio_dic, summary)
wilcox.test(dati$MV_PreBAL ~ dati$VentRatio_dic)
wilcoxsign_test(dati$MV_PreBAL ~ dati$VentRatio_dic)            

byf.shapiro(dati$DurationICU ~ dati$VentRatio_dic)
tapply(dati$DurationICU, dati$VentRatio_dic, summary)
wilcox.test(dati$DurationICU ~ dati$VentRatio_dic)
wilcoxsign_test(dati$DurationICU ~ dati$VentRatio_dic)

byf.shapiro(dati$DurationHosp ~ dati$VentRatio_dic)
tapply(dati$DurationHosp, dati$VentRatio_dic, summary)
wilcox.test(dati$DurationHosp ~ dati$VentRatio_dic)
wilcoxsign_test(dati$DurationHosp ~ dati$VentRatio_dic)

#################################
#######MICROBIOLOGY DATA#########
#################################

table(dati$cephalosporin, dati$VentRatio_dic)
percent(table(dati$cephalosporin, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$cephalosporin, dati$VentRatio_dic)

table(dati$beta_lactam_inhib, dati$VentRatio_dic)
percent(table(dati$beta_lactam_inhib, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$beta_lactam_inhib, dati$VentRatio_dic)

table(dati$macrolides, dati$VentRatio_dic)
percent(table(dati$macrolides, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$macrolides, dati$VentRatio_dic)

table(dati$fluoroquinolones, dati$VentRatio_dic)
percent(table(dati$fluoroquinolones, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$fluoroquinolones, dati$VentRatio_dic)

table(dati$others_antib, dati$VentRatio_dic)
percent(table(dati$others_antib, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$others_antib, dati$VentRatio_dic)

table(dati$BAL_Aspergillus, dati$VentRatio_dic)
percent(table(dati$BAL_Aspergillus, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$BAL_Aspergillus, dati$VentRatio_dic)

table(dati$BAL_Candida, dati$VentRatio_dic)
percent(table(dati$BAL_Candida, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$BAL_Candida, dati$VentRatio_dic)

table(dati$BAL_Bacterial, dati$VentRatio_dic)
percent(table(dati$BAL_Bacterial, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$BAL_Bacterial, dati$VentRatio_dic)

table(dati$Saureus, dati$VentRatio_dic)
percent(table(dati$Saureus, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$Saureus, dati$VentRatio_dic)

table(dati$Kl_pneum, dati$VentRatio_dic)
percent(table(dati$Kl_pneum, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$Kl_pneum, dati$VentRatio_dic)

table(dati$Ps_aerug, dati$VentRatio_dic)
percent(table(dati$Ps_aerug, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$Ps_aerug, dati$VentRatio_dic)

table(dati$MRSA, dati$VentRatio_dic)
percent(table(dati$MRSA, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$MRSA, dati$VentRatio_dic)

table(dati$CRE, dati$VentRatio_dic)
percent(table(dati$CRE, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$CRE, dati$VentRatio_dic)

table(dati$other_germs, dati$VentRatio_dic)
percent(table(dati$other_germs, dati$VentRatio_dic), margin=2, digits=1)
fisher.test(dati$other_germs, dati$VentRatio_dic)

##################
###MICROBIOME#####
##################

byf.shapiro(dati$observed ~ dati$VentRatio_dic)
tapply(dati$observed, dati$VentRatio_dic, summary)
tapply(dati$observed, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$observed ~ dati$VentRatio_dic)

byf.shapiro(dati$diversity_shannon ~ dati$VentRatio_dic)
tapply(dati$diversity_shannon, dati$VentRatio_dic, summary)
tapply(dati$diversity_shannon, dati$VentRatio_dic, sd,na.rm=T)
t.test(dati$diversity_shannon ~ dati$VentRatio_dic)

byf.shapiro(dati$evenness_pielou ~ dati$VentRatio_dic)
tapply(dati$evenness_pielou, dati$VentRatio_dic, summary)
tapply(dati$evenness_pielou, dati$VentRatio_dic, sd, na.rm=T)
t.test(dati$evenness_pielou ~ dati$VentRatio_dic)




############################################
####DESCRIPTIVES per High/Low Compliance####
############################################

###################
######Clinical#####
###################

byf.shapiro(dati$Age ~ dati$CRS_PBW_dic)
tapply(dati$Age, dati$CRS_PBW_dic, summary)
tapply(dati$Age, dati$CRS_PBW_dic, sd, na.rm=T)
t.test(dati$Age ~ dati$CRS_PBW_dic)

table(dati$Sex, dati$CRS_PBW_dic)
percent(table(dati$Sex, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$Sex, dati$CRS_PBW_dic)

byf.shapiro(dati$SAPSII ~ dati$CRS_PBW_dic)
tapply(dati$SAPSII, dati$CRS_PBW_dic, summary)
tapply(dati$SAPSII, dati$CRS_PBW_dic, sd, na.rm=T)
t.test(dati$SAPSII ~ dati$CRS_PBW_dic)

byf.shapiro(dati$CCI ~ dati$CRS_PBW_dic)
tapply(dati$CCI, dati$CRS_PBW_dic, summary)
wilcox.test(dati$CCI ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$CCI ~ dati$CRS_PBW_dic)

table(dati$hospitalization, dati$CRS_PBW_dic)
percent(table(dati$hospitalization, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$hospitalization, dati$CRS_PBW_dic)

table(dati$AH, dati$CRS_PBW_dic)
percent(table(dati$AH, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$AH, dati$CRS_PBW_dic)

table(dati$CHD, dati$CRS_PBW_dic)
percent(table(dati$CHD, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$CHD, dati$CRS_PBW_dic)

table(dati$CAD, dati$CRS_PBW_dic)
percent(table(dati$CAD, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$CAD, dati$CRS_PBW_dic)

table(dati$COPD, dati$CRS_PBW_dic)
percent(table(dati$COPD, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$COPD, dati$CRS_PBW_dic)

table(dati$ChronicVasculopathy, dati$CRS_PBW_dic)
percent(table(dati$ChronicVasculopathy, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$ChronicVasculopathy, dati$CRS_PBW_dic)

table(dati$Diabete, dati$CRS_PBW_dic)
percent(table(dati$Diabete, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$Diabete, dati$CRS_PBW_dic)

table(dati$Neo, dati$CRS_PBW_dic)
percent(table(dati$Neo, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$Neo, dati$CRS_PBW_dic)

table(dati$Immunosuo, dati$CRS_PBW_dic)
percent(table(dati$Immunosuo, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$Immunosuo, dati$CRS_PBW_dic)

byf.shapiro(dati$TimefromSymptoms ~ dati$CRS_PBW_dic)
tapply(dati$TimefromSymptoms, dati$CRS_PBW_dic, summary)
tapply(dati$TimefromSymptoms, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$TimefromSymptoms ~ dati$CRS_PBW_dic)

table(dati$OngoingSteroids, dati$CRS_PBW_dic)
percent(table(dati$OngoingSteroids, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$OngoingSteroids, dati$CRS_PBW_dic)

table(dati$IL6_i_somm, dati$CRS_PBW_dic)
percent(table(dati$IL6_i_somm, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$IL6_i_somm, dati$CRS_PBW_dic)

byf.shapiro(dati$WBC ~ dati$CRS_PBW_dic)
tapply(dati$WBC, dati$CRS_PBW_dic, summary)
tapply(dati$WBC, dati$CRS_PBW_dic, sd, na.rm=T)
t.test(dati$WBC ~ dati$CRS_PBW_dic)

byf.shapiro(dati$Neu ~ dati$CRS_PBW_dic)
tapply(dati$Neu, dati$CRS_PBW_dic, summary)
tapply(dati$Neu, dati$CRS_PBW_dic, sd, na.rm=T)
t.test(dati$Neu ~ dati$CRS_PBW_dic)

byf.shapiro(dati$Lympho ~ dati$CRS_PBW_dic)
tapply(dati$Lympho, dati$CRS_PBW_dic, summary)
wilcox.test(dati$Lympho ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$Lympho ~ dati$CRS_PBW_dic)

byf.shapiro(dati$Ddimer ~ dati$CRS_PBW_dic)
tapply(dati$Ddimer, dati$CRS_PBW_dic, summary)
wilcox.test(dati$Ddimer ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$Ddimer ~ dati$CRS_PBW_dic)

byf.shapiro(dati$Fibrinogen ~ dati$CRS_PBW_dic)
tapply(dati$Fibrinogen, dati$CRS_PBW_dic, summary)
tapply(dati$Fibrinogen, dati$CRS_PBW_dic, sd, na.rm=T)
t.test(dati$Fibrinogen ~ dati$CRS_PBW_dic)

byf.shapiro(dati$LDH ~ dati$CRS_PBW_dic)
tapply(dati$LDH, dati$CRS_PBW_dic, summary)
wilcox.test(dati$LDH ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$LDH ~ dati$CRS_PBW_dic)

byf.shapiro(dati$PCT ~ dati$CRS_PBW_dic)
tapply(dati$PCT, dati$CRS_PBW_dic, summary)
wilcox.test(dati$PCT ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$PCT ~ dati$CRS_PBW_dic)

byf.shapiro(dati$CRP ~ dati$CRS_PBW_dic)
tapply(dati$CRP, dati$CRS_PBW_dic, summary)
tapply(dati$CRP, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$CRP ~ dati$CRS_PBW_dic)

byf.shapiro(dati$P_F ~ dati$CRS_PBW_dic)
tapply(dati$P_F, dati$CRS_PBW_dic, summary)
wilcox.test(dati$P_F ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$P_F ~ dati$CRS_PBW_dic)

byf.shapiro(dati$PEEP ~ dati$CRS_PBW_dic)
tapply(dati$PEEP, dati$CRS_PBW_dic, summary)
wilcox.test(dati$PEEP ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$PEEP ~ dati$CRS_PBW_dic)

byf.shapiro(dati$Pplat ~ dati$CRS_PBW_dic)
tapply(dati$Pplat, dati$CRS_PBW_dic, summary)
tapply(dati$Pplat,dati$CRS_PBW_dic, sd, na.rm=T)
t.test(dati$Pplat ~ dati$CRS_PBW_dic)

byf.shapiro(dati$RR ~ dati$CRS_PBW_dic)
tapply(dati$RR, dati$CRS_PBW_dic, summary)
tapply(dati$RR, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$RR ~ dati$CRS_PBW_dic)

byf.shapiro(dati$PaCO2 ~ dati$CRS_PBW_dic)
tapply(dati$PaCO2, dati$CRS_PBW_dic, summary)
tapply(dati$PaCO2, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$PaCO2 ~ dati$CRS_PBW_dic)

byf.shapiro(dati$Vt_PBW ~ dati$CRS_PBW_dic)
tapply(dati$Vt_PBW, dati$CRS_PBW_dic, summary)
wilcox.test(dati$Vt_PBW ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$Vt_PBW ~ dati$CRS_PBW_dic)

byf.shapiro(dati$DrivingPress ~ dati$CRS_PBW_dic)
tapply(dati$DrivingPress, dati$CRS_PBW_dic, summary)
tapply(dati$DrivingPress, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$DrivingPress ~ dati$CRS_PBW_dic)

byf.shapiro(dati$OSI ~ dati$CRS_PBW_dic)
tapply(dati$OSI, dati$CRS_PBW_dic, summary)
wilcox.test(dati$OSI ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$OSI ~ dati$CRS_PBW_dic)

byf.shapiro(dati$CRS ~ dati$CRS_PBW_dic)
tapply(dati$CRS, dati$CRS_PBW_dic, summary)
tapply(dati$CRS, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$CRS ~ dati$CRS_PBW_dic)

byf.shapiro(dati$Elrs_PBW ~ dati$CRS_PBW_dic)
tapply(dati$Elrs_PBW, dati$CRS_PBW_dic, summary)
wilcox.test(dati$Elrs_PBW ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$Elrs_PBW ~ dati$CRS_PBW_dic)

byf.shapiro(dati$R_I ~ dati$CRS_PBW_dic)
tapply(dati$R_I, dati$CRS_PBW_dic, summary)
tapply(dati$R_I, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$R_I ~ dati$CRS_PBW_dic)

byf.shapiro(dati$MinutVentCorr ~ dati$CRS_PBW_dic)
tapply(dati$MinutVentCorr, dati$CRS_PBW_dic, summary)
tapply(dati$MinutVentCorr, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$MinutVentCorr ~ dati$CRS_PBW_dic)

table(dati$VentRatio_dic, dati$CRS_PBW_dic)
percent(table(dati$VentRatio_dic, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$VentRatio_dic, dati$CRS_PBW_dic)

byf.shapiro(dati$DP4_RR ~ dati$CRS_PBW_dic)
tapply(dati$DP4_RR, dati$CRS_PBW_dic, summary)
tapply(dati$DP4_RR, dati$CRS_PBW_dic, sd, na.rm=T)
t.test(dati$DP4_RR ~ dati$CRS_PBW_dic)

table(dati$d28_death, dati$CRS_PBW_dic)
percent(table(dati$d28_death, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$d28_death, dati$CRS_PBW_dic)

table(dati$death, dati$CRS_PBW_dic)
percent(table(dati$death, dati$CRS_PBW_dic), margin=2, digits=1)
survfit(Surv(OS, death) ~ CRS_PBW_dic, data = dati)
survdiff(Surv(OS, death) ~ CRS_PBW_dic, data = dati)

byf.shapiro(dati$OS ~ dati$CRS_PBW_dic)
tapply(dati$OS, dati$CRS_PBW_dic, summary)
wilcox.test(dati$OS ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$OS ~ dati$CRS_PBW_dic)

table(dati$d28_IMVweaning, dati$CRS_PBW_dic)
percent(table(dati$d28_IMVweaning, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$d28_IMVweaning, dati$CRS_PBW_dic)

table(dati$IMVweaning, dati$CRS_PBW_dic)
percent(table(dati$IMVweaning, dati$CRS_PBW_dic), margin=2, digits=1)
survfit(Surv(TimetoWean, IMVweaning) ~ CRS_PBW_dic, data = dati)
survdiff(Surv(TimetoWean, IMVweaning) ~ CRS_PBW_dic, data = dati)

byf.shapiro(dati$TimetoWean ~ dati$CRS_PBW_dic)
tapply(dati$TimetoWean, dati$CRS_PBW_dic, summary)
wilcox.test(dati$TimetoWean ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$TimetoWean ~ dati$CRS_PBW_dic)

byf.shapiro(dati$MV_PreBAL ~ dati$CRS_PBW_dic)
tapply(dati$MV_PreBAL, dati$CRS_PBW_dic, summary)
wilcox.test(dati$MV_PreBAL ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$MV_PreBAL ~ dati$CRS_PBW_dic)
            
byf.shapiro(dati$DurationICU ~ dati$CRS_PBW_dic)
tapply(dati$DurationICU, dati$CRS_PBW_dic, summary)
wilcox.test(dati$DurationICU ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$DurationICU ~ dati$CRS_PBW_dic)
            
byf.shapiro(dati$DurationHosp ~ dati$CRS_PBW_dic)
tapply(dati$DurationHosp, dati$CRS_PBW_dic, summary)
wilcox.test(dati$DurationHosp ~ dati$CRS_PBW_dic)
wilcoxsign_test(dati$DurationHosp ~ dati$CRS_PBW_dic)

#################################
#######MICROBIOLOGY DATA#########
#################################

table(dati$cephalosporin, dati$CRS_PBW_dic)
percent(table(dati$cephalosporin, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$cephalosporin, dati$CRS_PBW_dic)

table(dati$beta_lactam_inhib, dati$CRS_PBW_dic)
percent(table(dati$beta_lactam_inhib, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$beta_lactam_inhib, dati$CRS_PBW_dic)

table(dati$macrolides, dati$CRS_PBW_dic)
percent(table(dati$macrolides, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$macrolides, dati$CRS_PBW_dic)

table(dati$fluoroquinolones, dati$CRS_PBW_dic)
percent(table(dati$fluoroquinolones, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$fluoroquinolones, dati$CRS_PBW_dic)

table(dati$others_antib, dati$CRS_PBW_dic)
percent(table(dati$others_antib, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$others_antib, dati$CRS_PBW_dic)

table(dati$BAL_Aspergillus, dati$CRS_PBW_dic)
percent(table(dati$BAL_Aspergillus, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$BAL_Aspergillus, dati$CRS_PBW_dic)

table(dati$BAL_Candida, dati$CRS_PBW_dic)
percent(table(dati$BAL_Candida, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$BAL_Candida, dati$CRS_PBW_dic)

table(dati$BAL_Bacterial, dati$CRS_PBW_dic)
percent(table(dati$BAL_Bacterial, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$BAL_Bacterial, dati$CRS_PBW_dic)

table(dati$Saureus, dati$CRS_PBW_dic)
percent(table(dati$Saureus, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$Saureus, dati$CRS_PBW_dic)

table(dati$Kl_pneum, dati$CRS_PBW_dic)
percent(table(dati$Kl_pneum, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$Kl_pneum, dati$CRS_PBW_dic)

table(dati$Ps_aerug, dati$CRS_PBW_dic)
percent(table(dati$Ps_aerug, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$Ps_aerug, dati$CRS_PBW_dic)

table(dati$MRSA, dati$CRS_PBW_dic)
percent(table(dati$MRSA, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$MRSA, dati$CRS_PBW_dic)

table(dati$CRE, dati$CRS_PBW_dic)
percent(table(dati$CRE, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$CRE, dati$CRS_PBW_dic)

table(dati$other_germs, dati$CRS_PBW_dic)
percent(table(dati$other_germs, dati$CRS_PBW_dic), margin=2, digits=1)
fisher.test(dati$other_germs, dati$CRS_PBW_dic)

##################
###MICROBIOME#####
##################

byf.shapiro(dati$observed ~ dati$CRS_PBW_dic)
tapply(dati$observed, dati$CRS_PBW_dic, summary)
tapply(dati$observed, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$observed ~ dati$CRS_PBW_dic)

byf.shapiro(dati$diversity_shannon ~ dati$CRS_PBW_dic)
tapply(dati$diversity_shannon, dati$CRS_PBW_dic, summary)
tapply(dati$diversity_shannon, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$diversity_shannon ~ dati$CRS_PBW_dic)

byf.shapiro(dati$evenness_pielou ~ dati$CRS_PBW_dic)
tapply(dati$evenness_pielou, dati$CRS_PBW_dic, summary)
tapply(dati$evenness_pielou, dati$CRS_PBW_dic, sd,na.rm=T)
t.test(dati$evenness_pielou ~ dati$CRS_PBW_dic)
