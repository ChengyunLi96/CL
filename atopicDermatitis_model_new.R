library(dplyr)
library(lubridate)
library(pastecs)
patient_AD <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/atopicDermatitis/patientInfo_atopicDermatitis.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_AD <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/atopicDermatitis/atopicDermatitis_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AD_new_clinic.RData")
#Ethnic: Caucasian = 0, African American = 1
#Clinic: Dermatology clinic = 1, other clinic = 0
#Gender: Female = 1, male = 0
#yearBirth: date of birth (year)
#obesity: Obesity = 1 (BMI>= 30), otherwise = 0 
#Ethnic*Clinic: interaction term

#Modeling: Comorbidity ~ Ethnic + Clinic + Demographics (gender/age of the skin disease diagnosis) + Obesity+ Ethnic*Clinic
Cau_DERM_data <- data.frame("PATID" = AD_DERM_Cau_ID, "Ethnic" = rep(0, length(AD_DERM_Cau_ID)), "Clinic" = rep(1, length(AD_DERM_Cau_ID)))
Cau_OC_data <- data.frame("PATID" = AD_OC_Cau_ID, "Ethnic" = rep(0, length(AD_OC_Cau_ID)), "Clinic" = rep(0, length(AD_OC_Cau_ID)))
AA_DERM_data <- data.frame("PATID" = AD_DERM_AA_ID, "Ethnic" = rep(1, length(AD_DERM_AA_ID)), "Clinic" = rep(1, length(AD_DERM_AA_ID)))
AA_OC_data <- data.frame("PATID" = AD_OC_AA_ID, "Ethnic" = rep(1, length(AD_OC_AA_ID)), "Clinic" = rep(0, length(AD_OC_AA_ID)))

Eth_Cli_data <- bind_rows(Cau_DERM_data, Cau_OC_data, AA_DERM_data, AA_OC_data)

AD_m <- c(AD_DERM_Cau_ID, AD_OC_Cau_ID, AD_DERM_AA_ID, AD_OC_AA_ID)
AD_DERM <- c(AD_DERM_Cau_ID, AD_DERM_AA_ID)
AD_OC <- c(AD_OC_Cau_ID, AD_OC_AA_ID)
demo <- demo_AD %>% filter(X...PatientID %in% AD_m)
Gender_data <- data.frame("PATID" = demo$X...PatientID, "Gender" = demo$GenderCode)
Gender_data$Gender_F <- ifelse(Gender_data$Gender=="F",1,0)

total_number <- nrow(demo)
num_female <- sum(Gender_data$Gender_F)
num_male <- total_number - num_female


#obesity
OB_data <- bind_rows(data.frame("PATID"= AD_OB, "OB" = c(rep(1,length(AD_OB)))), data.frame("PATID"= setdiff(AD_m,AD_OB), "OB" = c(rep(0,length(setdiff(AD_m,AD_OB))))))

##yearBirth
patient <- patient_AD %>% filter(X...PatientID %in% AD_m)
yearBirth_data <- data.frame("PATID" = patient$X...PatientID, "yearBirth" = year(mdy_hm(patient$DOB)))

AD_demo_DATA <- inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), 
                                      yearBirth_data, by= "PATID"), OB_data, by="PATID")
save(AD_demo_DATA, file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AD_demo_table.RData")


#18 comorbidities
T2D_data <- bind_rows(data.frame("PATID"= AD_T2D, "T2D" = c(rep(1,length(AD_T2D)))), data.frame("PATID"= setdiff(AD_m,AD_T2D), "T2D" = c(rep(0,length(setdiff(AD_m,AD_T2D))))))
HTN_data <- bind_rows(data.frame("PATID"= AD_HTN, "HTN" = c(rep(1,length(AD_HTN)))), data.frame("PATID"= setdiff(AD_m,AD_HTN), "HTN" = c(rep(0,length(setdiff(AD_m,AD_HTN))))))
RA_data <- bind_rows(data.frame("PATID"= AD_RA, "RA" = c(rep(1,length(AD_RA)))), data.frame("PATID"= setdiff(AD_m,AD_RA), "RA" = c(rep(0,length(setdiff(AD_m,AD_RA))))))
IBD_data <- bind_rows(data.frame("PATID"= AD_IBD, "IBD" = c(rep(1,length(AD_IBD)))), data.frame("PATID"= setdiff(AD_m,AD_IBD), "IBD" = c(rep(0,length(setdiff(AD_m,AD_IBD))))))
PARK_data <- bind_rows(data.frame("PATID"= AD_PARK, "PARK" = c(rep(1,length(AD_PARK)))), data.frame("PATID"= setdiff(AD_m,AD_PARK), "PARK" = c(rep(0,length(setdiff(AD_m,AD_PARK))))))
T1D_data <- bind_rows(data.frame("PATID"= AD_T1D, "T1D" = c(rep(1,length(AD_T1D)))), data.frame("PATID"= setdiff(AD_m,AD_T1D), "T1D" = c(rep(0,length(setdiff(AD_m,AD_T1D))))))
UVE_data <- bind_rows(data.frame("PATID"= AD_UVE, "UVE" = c(rep(1,length(AD_UVE)))), data.frame("PATID"= setdiff(AD_m,AD_UVE), "UVE" = c(rep(0,length(setdiff(AD_m,AD_UVE))))))
GOUT_data <- bind_rows(data.frame("PATID"= AD_GOUT, "GOUT" = c(rep(1,length(AD_GOUT)))), data.frame("PATID"= setdiff(AD_m,AD_GOUT), "GOUT" = c(rep(0,length(setdiff(AD_m,AD_GOUT))))))
OST_data <- bind_rows(data.frame("PATID"= AD_OST, "OST" = c(rep(1,length(AD_OST)))), data.frame("PATID"= setdiff(AD_m,AD_OST), "OST" = c(rep(0,length(setdiff(AD_m,AD_OST))))))
CKD_data <- bind_rows(data.frame("PATID"= AD_CKD, "CKD" = c(rep(1,length(AD_CKD)))), data.frame("PATID"= setdiff(AD_m,AD_CKD), "CKD" = c(rep(0,length(setdiff(AD_m,AD_CKD))))))
ATF_data <- bind_rows(data.frame("PATID"= AD_ATF, "ATF" = c(rep(1,length(AD_ATF)))), data.frame("PATID"= setdiff(AD_m,AD_ATF), "ATF" = c(rep(0,length(setdiff(AD_m,AD_ATF))))))
PNEU_data <- bind_rows(data.frame("PATID"= AD_PNEU, "PNEU" = c(rep(1,length(AD_PNEU)))), data.frame("PATID"= setdiff(AD_m,AD_PNEU), "PNEU" = c(rep(0,length(setdiff(AD_m,AD_PNEU))))))
HIV_data <- bind_rows(data.frame("PATID"= AD_HIV, "HIV" = c(rep(1,length(AD_HIV)))), data.frame("PATID"= setdiff(AD_m,AD_HIV), "HIV" = c(rep(0,length(setdiff(AD_m,AD_HIV))))))
SCHI_data <- bind_rows(data.frame("PATID"= AD_SCHI, "SCHI" = c(rep(1,length(AD_SCHI)))), data.frame("PATID"= setdiff(AD_m,AD_SCHI), "SCHI" = c(rep(0,length(setdiff(AD_m,AD_SCHI))))))
ALZH_data <- bind_rows(data.frame("PATID"= AD_ALZH, "ALZH" = c(rep(1,length(AD_ALZH)))), data.frame("PATID"= setdiff(AD_m,AD_ALZH), "ALZH" = c(rep(0,length(setdiff(AD_m,AD_ALZH))))))
ASTH_data <- bind_rows(data.frame("PATID"= AD_ASTH, "ASTH" = c(rep(1,length(AD_ASTH)))), data.frame("PATID"= setdiff(AD_m,AD_ASTH), "ASTH" = c(rep(0,length(setdiff(AD_m,AD_ASTH))))))
DEP_data <- bind_rows(data.frame("PATID"= AD_DEP, "DEP" = c(rep(1,length(AD_DEP)))), data.frame("PATID"= setdiff(AD_m,AD_DEP), "DEP" = c(rep(0,length(setdiff(AD_m,AD_DEP))))))
ANX_data <- bind_rows(data.frame("PATID"= AD_ANX, "ANX" = c(rep(1,length(AD_ANX)))), data.frame("PATID"= setdiff(AD_m,AD_ANX), "ANX" = c(rep(0,length(setdiff(AD_m,AD_ANX))))))

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

AD_DATA <- inner_join(inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), COM_18, by="PATID"), yearBirth_data, by= "PATID"), OB_data, by="PATID")

glm.AD.T2D <- glm(data=AD_DATA, T2D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.T2D))
glm.AD.HTN <- glm(data=AD_DATA, HTN~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.HTN))
glm.AD.RA <- glm(data=AD_DATA, RA~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.RA))
glm.AD.IBD <- glm(data=AD_DATA, IBD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.IBD))
glm.AD.PARK <- glm(data=AD_DATA, PARK~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.PARK))
glm.AD.T1D <- glm(data=AD_DATA, T1D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.T1D))
glm.AD.UVE <- glm(data=AD_DATA, UVE~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.UVE))
glm.AD.GOUT <- glm(data=AD_DATA, GOUT~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.GOUT))
glm.AD.OST <- glm(data=AD_DATA, OST~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.OST))
glm.AD.CKD <- glm(data=AD_DATA, CKD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.CKD))
glm.AD.ATF<- glm(data=AD_DATA, ATF~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.ATF))
glm.AD.PNEU <- glm(data=AD_DATA, PNEU~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.PNEU))
glm.AD.HIV <- glm(data=AD_DATA, HIV~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.HIV))
glm.AD.SCHI <- glm(data=AD_DATA, SCHI~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.SCHI))
glm.AD.ALZH<- glm(data=AD_DATA, ALZH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.ALZH))
glm.AD.ASTH <- glm(data=AD_DATA, ASTH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.ASTH))
glm.AD.ANX <- glm(data=AD_DATA, ANX~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.ANX))
glm.AD.DEP <- glm(data=AD_DATA, DEP~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AD.DEP))

save(glm.AD.T2D, glm.AD.HTN, glm.AD.RA, glm.AD.IBD, glm.AD.PARK, glm.AD.T1D, glm.AD.UVE, glm.AD.GOUT, glm.AD.OST,
     glm.AD.CKD, glm.AD.ATF, glm.AD.PNEU, glm.AD.HIV, glm.AD.SCHI, glm.AD.ALZH, glm.AD.ASTH, glm.AD.ANX, glm.AD.DEP, 
     file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AD_new_model.RData")

print(paste("total # of patients: ", total_number))
print(paste("# of female: ", num_female))
print(paste("# of male: ", num_male))