library(dplyr)
library(lubridate)
library(pastecs)
patient_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/patientInfo_acne.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/acne_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AC_new_clinic.RData")
#Ethnic: Caucasian = 0, African American = 1
#Clinic: Dermatology clinic = 1, other clinic = 0
#Gender: Female = 1, male = 0
#yearBirth: date of birth (year)
#obesity: Obesity = 1 (BMI>= 30), otherwise = 0 
#Ethnic*Clinic: interaction term

#Modeling: Comorbidity ~ Ethnic + Clinic + Demographics (gender/age of the skin disease diagnosis) + Obesity+ Ethnic*Clinic
Cau_DERM_data <- data.frame("PATID" = AC_DERM_Cau_ID, "Ethnic" = rep(0, length(AC_DERM_Cau_ID)), "Clinic" = rep(1, length(AC_DERM_Cau_ID)))
Cau_OC_data <- data.frame("PATID" = AC_OC_Cau_ID, "Ethnic" = rep(0, length(AC_OC_Cau_ID)), "Clinic" = rep(0, length(AC_OC_Cau_ID)))
AA_DERM_data <- data.frame("PATID" = AC_DERM_AA_ID, "Ethnic" = rep(1, length(AC_DERM_AA_ID)), "Clinic" = rep(1, length(AC_DERM_AA_ID)))
AA_OC_data <- data.frame("PATID" = AC_OC_AA_ID, "Ethnic" = rep(1, length(AC_OC_AA_ID)), "Clinic" = rep(0, length(AC_OC_AA_ID)))

Eth_Cli_data <- bind_rows(Cau_DERM_data, Cau_OC_data, AA_DERM_data, AA_OC_data)

AC_m <- c(AC_DERM_Cau_ID, AC_OC_Cau_ID, AC_DERM_AA_ID, AC_OC_AA_ID)
AC_DERM <- c(AC_DERM_Cau_ID, AC_DERM_AA_ID)
AC_OC <- c(AC_OC_Cau_ID, AC_OC_AA_ID)
demo <- demo_AC %>% filter(X...PatientID %in% AC_m)
Gender_data <- data.frame("PATID" = demo$X...PatientID, "Gender" = demo$GenderCode)
Gender_data$Gender_F <- ifelse(Gender_data$Gender=="F",1,0)

total_number <- nrow(demo)
num_female <- sum(Gender_data$Gender_F)
num_male <- total_number - num_female


#obesity
OB_data <- bind_rows(data.frame("PATID"= AC_OB, "OB" = c(rep(1,length(AC_OB)))), data.frame("PATID"= setdiff(AC_m,AC_OB), "OB" = c(rep(0,length(setdiff(AC_m,AC_OB))))))

##yearBirth
patient <- patient_AC %>% filter(X...PatientID %in% AC_m)
yearBirth_data <- data.frame("PATID" = patient$X...PatientID, "yearBirth" = year(mdy_hm(patient$DOB)))

AC_demo_DATA <- inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), 
                                      yearBirth_data, by= "PATID"), OB_data, by="PATID")
save(AC_demo_DATA, file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AC_demo_table.RData")


#18 comorbidities
T2D_data <- bind_rows(data.frame("PATID"= AC_T2D, "T2D" = c(rep(1,length(AC_T2D)))), data.frame("PATID"= setdiff(AC_m,AC_T2D), "T2D" = c(rep(0,length(setdiff(AC_m,AC_T2D))))))
HTN_data <- bind_rows(data.frame("PATID"= AC_HTN, "HTN" = c(rep(1,length(AC_HTN)))), data.frame("PATID"= setdiff(AC_m,AC_HTN), "HTN" = c(rep(0,length(setdiff(AC_m,AC_HTN))))))
RA_data <- bind_rows(data.frame("PATID"= AC_RA, "RA" = c(rep(1,length(AC_RA)))), data.frame("PATID"= setdiff(AC_m,AC_RA), "RA" = c(rep(0,length(setdiff(AC_m,AC_RA))))))
IBD_data <- bind_rows(data.frame("PATID"= AC_IBD, "IBD" = c(rep(1,length(AC_IBD)))), data.frame("PATID"= setdiff(AC_m,AC_IBD), "IBD" = c(rep(0,length(setdiff(AC_m,AC_IBD))))))
PARK_data <- bind_rows(data.frame("PATID"= AC_PARK, "PARK" = c(rep(1,length(AC_PARK)))), data.frame("PATID"= setdiff(AC_m,AC_PARK), "PARK" = c(rep(0,length(setdiff(AC_m,AC_PARK))))))
T1D_data <- bind_rows(data.frame("PATID"= AC_T1D, "T1D" = c(rep(1,length(AC_T1D)))), data.frame("PATID"= setdiff(AC_m,AC_T1D), "T1D" = c(rep(0,length(setdiff(AC_m,AC_T1D))))))
UVE_data <- bind_rows(data.frame("PATID"= AC_UVE, "UVE" = c(rep(1,length(AC_UVE)))), data.frame("PATID"= setdiff(AC_m,AC_UVE), "UVE" = c(rep(0,length(setdiff(AC_m,AC_UVE))))))
GOUT_data <- bind_rows(data.frame("PATID"= AC_GOUT, "GOUT" = c(rep(1,length(AC_GOUT)))), data.frame("PATID"= setdiff(AC_m,AC_GOUT), "GOUT" = c(rep(0,length(setdiff(AC_m,AC_GOUT))))))
OST_data <- bind_rows(data.frame("PATID"= AC_OST, "OST" = c(rep(1,length(AC_OST)))), data.frame("PATID"= setdiff(AC_m,AC_OST), "OST" = c(rep(0,length(setdiff(AC_m,AC_OST))))))
CKD_data <- bind_rows(data.frame("PATID"= AC_CKD, "CKD" = c(rep(1,length(AC_CKD)))), data.frame("PATID"= setdiff(AC_m,AC_CKD), "CKD" = c(rep(0,length(setdiff(AC_m,AC_CKD))))))
ATF_data <- bind_rows(data.frame("PATID"= AC_ATF, "ATF" = c(rep(1,length(AC_ATF)))), data.frame("PATID"= setdiff(AC_m,AC_ATF), "ATF" = c(rep(0,length(setdiff(AC_m,AC_ATF))))))
PNEU_data <- bind_rows(data.frame("PATID"= AC_PNEU, "PNEU" = c(rep(1,length(AC_PNEU)))), data.frame("PATID"= setdiff(AC_m,AC_PNEU), "PNEU" = c(rep(0,length(setdiff(AC_m,AC_PNEU))))))
HIV_data <- bind_rows(data.frame("PATID"= AC_HIV, "HIV" = c(rep(1,length(AC_HIV)))), data.frame("PATID"= setdiff(AC_m,AC_HIV), "HIV" = c(rep(0,length(setdiff(AC_m,AC_HIV))))))
SCHI_data <- bind_rows(data.frame("PATID"= AC_SCHI, "SCHI" = c(rep(1,length(AC_SCHI)))), data.frame("PATID"= setdiff(AC_m,AC_SCHI), "SCHI" = c(rep(0,length(setdiff(AC_m,AC_SCHI))))))
ALZH_data <- bind_rows(data.frame("PATID"= AC_ALZH, "ALZH" = c(rep(1,length(AC_ALZH)))), data.frame("PATID"= setdiff(AC_m,AC_ALZH), "ALZH" = c(rep(0,length(setdiff(AC_m,AC_ALZH))))))
ASTH_data <- bind_rows(data.frame("PATID"= AC_ASTH, "ASTH" = c(rep(1,length(AC_ASTH)))), data.frame("PATID"= setdiff(AC_m,AC_ASTH), "ASTH" = c(rep(0,length(setdiff(AC_m,AC_ASTH))))))
DEP_data <- bind_rows(data.frame("PATID"= AC_DEP, "DEP" = c(rep(1,length(AC_DEP)))), data.frame("PATID"= setdiff(AC_m,AC_DEP), "DEP" = c(rep(0,length(setdiff(AC_m,AC_DEP))))))
ANX_data <- bind_rows(data.frame("PATID"= AC_ANX, "ANX" = c(rep(1,length(AC_ANX)))), data.frame("PATID"= setdiff(AC_m,AC_ANX), "ANX" = c(rep(0,length(setdiff(AC_m,AC_ANX))))))

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

AC_DATA <- inner_join(inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), COM_18, by="PATID"), yearBirth_data, by= "PATID"), OB_data, by="PATID")

glm.AC.T2D <- glm(data=AC_DATA, T2D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.T2D))
glm.AC.HTN <- glm(data=AC_DATA, HTN~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.HTN))
glm.AC.RA <- glm(data=AC_DATA, RA~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.RA))
glm.AC.IBD <- glm(data=AC_DATA, IBD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.IBD))
glm.AC.PARK <- glm(data=AC_DATA, PARK~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.PARK))
glm.AC.T1D <- glm(data=AC_DATA, T1D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.T1D))
glm.AC.UVE <- glm(data=AC_DATA, UVE~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.UVE))
glm.AC.GOUT <- glm(data=AC_DATA, GOUT~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.GOUT))
glm.AC.OST <- glm(data=AC_DATA, OST~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.OST))
glm.AC.CKD <- glm(data=AC_DATA, CKD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.CKD))
glm.AC.ATF<- glm(data=AC_DATA, ATF~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.ATF))
glm.AC.PNEU <- glm(data=AC_DATA, PNEU~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.PNEU))
glm.AC.HIV <- glm(data=AC_DATA, HIV~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.HIV))
glm.AC.SCHI <- glm(data=AC_DATA, SCHI~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.SCHI))
glm.AC.ALZH<- glm(data=AC_DATA, ALZH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.ALZH))
glm.AC.ASTH <- glm(data=AC_DATA, ASTH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.ASTH))
glm.AC.ANX <- glm(data=AC_DATA, ANX~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.ANX))
glm.AC.DEP <- glm(data=AC_DATA, DEP~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AC.DEP))

save(glm.AC.T2D, glm.AC.HTN, glm.AC.RA, glm.AC.IBD, glm.AC.PARK, glm.AC.T1D, glm.AC.UVE, glm.AC.GOUT, glm.AC.OST,
     glm.AC.CKD, glm.AC.ATF, glm.AC.PNEU, glm.AC.HIV, glm.AC.SCHI, glm.AC.ALZH, glm.AC.ASTH, glm.AC.ANX, glm.AC.DEP, 
     file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AC_new_model.RData")

print(paste("total # of patients: ", total_number))
print(paste("# of female: ", num_female))
print(paste("# of male: ", num_male))