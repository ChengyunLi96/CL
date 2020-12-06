library(dplyr)
library(lubridate)
library(pastecs)
patient_VI <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/vitiligo/patientInfo_vitiligo.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_VI <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/vitiligo/vitiligo_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/VI_new_clinic.RData")
#Ethnic: Caucasian = 0, African American = 1
#Clinic: Dermatology clinic = 1, other clinic = 0
#Gender: Female = 1, male = 0
#yearBirth: date of birth (year)
#obesity: Obesity = 1 (BMI>= 30), otherwise = 0 
#Ethnic*Clinic: interaction term

#Modeling: Comorbidity ~ Ethnic + Clinic + Demographics (gender/age of the skin disease diagnosis) + Obesity+ Ethnic*Clinic
Cau_DERM_data <- data.frame("PATID" = VI_DERM_Cau_ID, "Ethnic" = rep(0, length(VI_DERM_Cau_ID)), "Clinic" = rep(1, length(VI_DERM_Cau_ID)))
Cau_OC_data <- data.frame("PATID" = VI_OC_Cau_ID, "Ethnic" = rep(0, length(VI_OC_Cau_ID)), "Clinic" = rep(0, length(VI_OC_Cau_ID)))
AA_DERM_data <- data.frame("PATID" = VI_DERM_AA_ID, "Ethnic" = rep(1, length(VI_DERM_AA_ID)), "Clinic" = rep(1, length(VI_DERM_AA_ID)))
AA_OC_data <- data.frame("PATID" = VI_OC_AA_ID, "Ethnic" = rep(1, length(VI_OC_AA_ID)), "Clinic" = rep(0, length(VI_OC_AA_ID)))

Eth_Cli_data <- bind_rows(Cau_DERM_data, Cau_OC_data, AA_DERM_data, AA_OC_data)

VI_m <- c(VI_DERM_Cau_ID, VI_OC_Cau_ID, VI_DERM_AA_ID, VI_OC_AA_ID)
VI_DERM <- c(VI_DERM_Cau_ID, VI_DERM_AA_ID)
VI_OC <- c(VI_OC_Cau_ID, VI_OC_AA_ID)
demo <- demo_VI %>% filter(X...PatientID %in% VI_m)
Gender_data <- data.frame("PATID" = demo$X...PatientID, "Gender" = demo$GenderCode)
Gender_data$Gender_F <- ifelse(Gender_data$Gender=="F",1,0)

total_number <- nrow(demo)
num_female <- sum(Gender_data$Gender_F)
num_male <- total_number - num_female


#obesity
OB_data <- bind_rows(data.frame("PATID"= VI_OB, "OB" = c(rep(1,length(VI_OB)))), data.frame("PATID"= setdiff(VI_m,VI_OB), "OB" = c(rep(0,length(setdiff(VI_m,VI_OB))))))

##yearBirth
patient <- patient_VI %>% filter(X...PatientID %in% VI_m)
yearBirth_data <- data.frame("PATID" = patient$X...PatientID, "yearBirth" = year(mdy_hm(patient$DOB)))

VI_demo_DATA <- inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), 
                                      yearBirth_data, by= "PATID"), OB_data, by="PATID")
save(VI_demo_DATA, file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/VI_demo_table.RData")


#18 comorbidities
T2D_data <- bind_rows(data.frame("PATID"= VI_T2D, "T2D" = c(rep(1,length(VI_T2D)))), data.frame("PATID"= setdiff(VI_m,VI_T2D), "T2D" = c(rep(0,length(setdiff(VI_m,VI_T2D))))))
HTN_data <- bind_rows(data.frame("PATID"= VI_HTN, "HTN" = c(rep(1,length(VI_HTN)))), data.frame("PATID"= setdiff(VI_m,VI_HTN), "HTN" = c(rep(0,length(setdiff(VI_m,VI_HTN))))))
RA_data <- bind_rows(data.frame("PATID"= VI_RA, "RA" = c(rep(1,length(VI_RA)))), data.frame("PATID"= setdiff(VI_m,VI_RA), "RA" = c(rep(0,length(setdiff(VI_m,VI_RA))))))
IBD_data <- bind_rows(data.frame("PATID"= VI_IBD, "IBD" = c(rep(1,length(VI_IBD)))), data.frame("PATID"= setdiff(VI_m,VI_IBD), "IBD" = c(rep(0,length(setdiff(VI_m,VI_IBD))))))
PARK_data <- bind_rows(data.frame("PATID"= VI_PARK, "PARK" = c(rep(1,length(VI_PARK)))), data.frame("PATID"= setdiff(VI_m,VI_PARK), "PARK" = c(rep(0,length(setdiff(VI_m,VI_PARK))))))
T1D_data <- bind_rows(data.frame("PATID"= VI_T1D, "T1D" = c(rep(1,length(VI_T1D)))), data.frame("PATID"= setdiff(VI_m,VI_T1D), "T1D" = c(rep(0,length(setdiff(VI_m,VI_T1D))))))
UVE_data <- bind_rows(data.frame("PATID"= VI_UVE, "UVE" = c(rep(1,length(VI_UVE)))), data.frame("PATID"= setdiff(VI_m,VI_UVE), "UVE" = c(rep(0,length(setdiff(VI_m,VI_UVE))))))
GOUT_data <- bind_rows(data.frame("PATID"= VI_GOUT, "GOUT" = c(rep(1,length(VI_GOUT)))), data.frame("PATID"= setdiff(VI_m,VI_GOUT), "GOUT" = c(rep(0,length(setdiff(VI_m,VI_GOUT))))))
OST_data <- bind_rows(data.frame("PATID"= VI_OST, "OST" = c(rep(1,length(VI_OST)))), data.frame("PATID"= setdiff(VI_m,VI_OST), "OST" = c(rep(0,length(setdiff(VI_m,VI_OST))))))
CKD_data <- bind_rows(data.frame("PATID"= VI_CKD, "CKD" = c(rep(1,length(VI_CKD)))), data.frame("PATID"= setdiff(VI_m,VI_CKD), "CKD" = c(rep(0,length(setdiff(VI_m,VI_CKD))))))
ATF_data <- bind_rows(data.frame("PATID"= VI_ATF, "ATF" = c(rep(1,length(VI_ATF)))), data.frame("PATID"= setdiff(VI_m,VI_ATF), "ATF" = c(rep(0,length(setdiff(VI_m,VI_ATF))))))
PNEU_data <- bind_rows(data.frame("PATID"= VI_PNEU, "PNEU" = c(rep(1,length(VI_PNEU)))), data.frame("PATID"= setdiff(VI_m,VI_PNEU), "PNEU" = c(rep(0,length(setdiff(VI_m,VI_PNEU))))))
HIV_data <- bind_rows(data.frame("PATID"= VI_HIV, "HIV" = c(rep(1,length(VI_HIV)))), data.frame("PATID"= setdiff(VI_m,VI_HIV), "HIV" = c(rep(0,length(setdiff(VI_m,VI_HIV))))))
SCHI_data <- bind_rows(data.frame("PATID"= VI_SCHI, "SCHI" = c(rep(1,length(VI_SCHI)))), data.frame("PATID"= setdiff(VI_m,VI_SCHI), "SCHI" = c(rep(0,length(setdiff(VI_m,VI_SCHI))))))
ALZH_data <- bind_rows(data.frame("PATID"= VI_ALZH, "ALZH" = c(rep(1,length(VI_ALZH)))), data.frame("PATID"= setdiff(VI_m,VI_ALZH), "ALZH" = c(rep(0,length(setdiff(VI_m,VI_ALZH))))))
ASTH_data <- bind_rows(data.frame("PATID"= VI_ASTH, "ASTH" = c(rep(1,length(VI_ASTH)))), data.frame("PATID"= setdiff(VI_m,VI_ASTH), "ASTH" = c(rep(0,length(setdiff(VI_m,VI_ASTH))))))
DEP_data <- bind_rows(data.frame("PATID"= VI_DEP, "DEP" = c(rep(1,length(VI_DEP)))), data.frame("PATID"= setdiff(VI_m,VI_DEP), "DEP" = c(rep(0,length(setdiff(VI_m,VI_DEP))))))
ANX_data <- bind_rows(data.frame("PATID"= VI_ANX, "ANX" = c(rep(1,length(VI_ANX)))), data.frame("PATID"= setdiff(VI_m,VI_ANX), "ANX" = c(rep(0,length(setdiff(VI_m,VI_ANX))))))

COM <- inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(T2D_data,HTN_data, by="PATID"),RA_data, by="PATID"), IBD_data, by="PATID")
                                                   , PARK_data, by ="PATID"), UVE_data, by ="PATID"), T1D_data, by ="PATID"), GOUT_data, by ="PATID")
COM_9 <- inner_join(COM, OST_data, by ="PATID")
COM_10 <- inner_join(COM_9, CKD_data, by ="PATID")
COM_11 <- inner_join(COM_10, ATF_data, by ="PATID")
COM_12 <- inner_join(COM_11, PNEU_data, by ="PATID")
COM_13 <- inner_join(COM_12, HIV_data, by ="PATID")
COM_14 <- inner_join(COM_13, SCHI_data, by ="PATID")
COM_15 <- inner_join(COM_14, ALZH_data, by ="PATID")
COM_16 <- inner_join(COM_15, ASTH_data, by ="PATID")
COM_17 <- inner_join(COM_16, ANX_data, by ="PATID")
COM_18 <- inner_join(COM_17, DEP_data, by ="PATID")

VI_DATA <- inner_join(inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), COM_18, by="PATID"), yearBirth_data, by= "PATID"), OB_data, by="PATID")

glm.VI.T2D <- glm(data=VI_DATA, T2D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.T2D))
glm.VI.HTN <- glm(data=VI_DATA, HTN~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.HTN))
glm.VI.RA <- glm(data=VI_DATA, RA~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.RA))
glm.VI.IBD <- glm(data=VI_DATA, IBD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.IBD))
glm.VI.PARK <- glm(data=VI_DATA, PARK~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.PARK))
glm.VI.T1D <- glm(data=VI_DATA, T1D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.T1D))
glm.VI.UVE <- glm(data=VI_DATA, UVE~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.UVE))
glm.VI.GOUT <- glm(data=VI_DATA, GOUT~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.GOUT))
glm.VI.OST <- glm(data=VI_DATA, OST~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.OST))
glm.VI.CKD <- glm(data=VI_DATA, CKD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.CKD))
glm.VI.ATF<- glm(data=VI_DATA, ATF~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.ATF))
glm.VI.PNEU <- glm(data=VI_DATA, PNEU~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.PNEU))
glm.VI.HIV <- glm(data=VI_DATA, HIV~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.HIV))
glm.VI.SCHI <- glm(data=VI_DATA, SCHI~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.SCHI))
glm.VI.ALZH<- glm(data=VI_DATA, ALZH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.ALZH))
glm.VI.ASTH <- glm(data=VI_DATA, ASTH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.ASTH))
glm.VI.ANX <- glm(data=VI_DATA, ANX~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.ANX))
glm.VI.DEP <- glm(data=VI_DATA, DEP~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.VI.DEP))

save(glm.VI.T2D, glm.VI.HTN, glm.VI.RA, glm.VI.IBD, glm.VI.PARK, glm.VI.T1D, glm.VI.UVE, glm.VI.GOUT, glm.VI.OST,
     glm.VI.CKD, glm.VI.ATF, glm.VI.PNEU, glm.VI.HIV, glm.VI.SCHI, glm.VI.ALZH, glm.VI.ASTH, glm.VI.ANX, glm.VI.DEP, 
     file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/VI_new_model.RData")

print(paste("total # of patients: ", total_number))
print(paste("# of female: ", num_female))
print(paste("# of male: ", num_male))