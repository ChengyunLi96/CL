library(dplyr)
library(lubridate)
library(pastecs)
patient_AL <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/alopeciaAreata/patientInfo_alopeciaAreata.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_AL <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/alopeciaAreata/alopeciaAreata_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AL_new_clinic.RData")
#Ethnic: Caucasian = 0, African American = 1
#Clinic: Dermatology clinic = 1, other clinic = 0
#Gender: Female = 1, male = 0
#yearBirth: date of birth (year)
#obesity: Obesity = 1 (BMI>= 30), otherwise = 0 
#Ethnic*Clinic: interaction term

#Modeling: Comorbidity ~ Ethnic + Clinic + Demographics (gender/age of the skin disease diagnosis) + Obesity+ Ethnic*Clinic
Cau_DERM_data <- data.frame("PATID" = AL_DERM_Cau_ID, "Ethnic" = rep(0, length(AL_DERM_Cau_ID)), "Clinic" = rep(1, length(AL_DERM_Cau_ID)))
Cau_OC_data <- data.frame("PATID" = AL_OC_Cau_ID, "Ethnic" = rep(0, length(AL_OC_Cau_ID)), "Clinic" = rep(0, length(AL_OC_Cau_ID)))
AA_DERM_data <- data.frame("PATID" = AL_DERM_AA_ID, "Ethnic" = rep(1, length(AL_DERM_AA_ID)), "Clinic" = rep(1, length(AL_DERM_AA_ID)))
AA_OC_data <- data.frame("PATID" = AL_OC_AA_ID, "Ethnic" = rep(1, length(AL_OC_AA_ID)), "Clinic" = rep(0, length(AL_OC_AA_ID)))

Eth_Cli_data <- bind_rows(Cau_DERM_data, Cau_OC_data, AA_DERM_data, AA_OC_data)

AL_m <- c(AL_DERM_Cau_ID, AL_OC_Cau_ID, AL_DERM_AA_ID, AL_OC_AA_ID)
AL_DERM <- c(AL_DERM_Cau_ID, AL_DERM_AA_ID)
AL_OC <- c(AL_OC_Cau_ID, AL_OC_AA_ID)
demo <- demo_AL %>% filter(X...PatientID %in% AL_m)
Gender_data <- data.frame("PATID" = demo$X...PatientID, "Gender" = demo$GenderCode)
Gender_data$Gender_F <- ifelse(Gender_data$Gender=="F",1,0)

total_number <- nrow(demo)
num_female <- sum(Gender_data$Gender_F)
num_male <- total_number - num_female


#obesity
OB_data <- bind_rows(data.frame("PATID"= AL_OB, "OB" = c(rep(1,length(AL_OB)))), data.frame("PATID"= setdiff(AL_m,AL_OB), "OB" = c(rep(0,length(setdiff(AL_m,AL_OB))))))

##yearBirth
patient <- patient_AL %>% filter(X...PatientID %in% AL_m)
yearBirth_data <- data.frame("PATID" = patient$X...PatientID, "yearBirth" = year(mdy_hm(patient$DOB)))

AL_demo_DATA <- inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), 
                                      yearBirth_data, by= "PATID"), OB_data, by="PATID")
save(AL_demo_DATA, file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AL_demo_table.RData")


#18 comorbidities
T2D_data <- bind_rows(data.frame("PATID"= AL_T2D, "T2D" = c(rep(1,length(AL_T2D)))), data.frame("PATID"= setdiff(AL_m,AL_T2D), "T2D" = c(rep(0,length(setdiff(AL_m,AL_T2D))))))
HTN_data <- bind_rows(data.frame("PATID"= AL_HTN, "HTN" = c(rep(1,length(AL_HTN)))), data.frame("PATID"= setdiff(AL_m,AL_HTN), "HTN" = c(rep(0,length(setdiff(AL_m,AL_HTN))))))
RA_data <- bind_rows(data.frame("PATID"= AL_RA, "RA" = c(rep(1,length(AL_RA)))), data.frame("PATID"= setdiff(AL_m,AL_RA), "RA" = c(rep(0,length(setdiff(AL_m,AL_RA))))))
IBD_data <- bind_rows(data.frame("PATID"= AL_IBD, "IBD" = c(rep(1,length(AL_IBD)))), data.frame("PATID"= setdiff(AL_m,AL_IBD), "IBD" = c(rep(0,length(setdiff(AL_m,AL_IBD))))))
PARK_data <- bind_rows(data.frame("PATID"= AL_PARK, "PARK" = c(rep(1,length(AL_PARK)))), data.frame("PATID"= setdiff(AL_m,AL_PARK), "PARK" = c(rep(0,length(setdiff(AL_m,AL_PARK))))))
T1D_data <- bind_rows(data.frame("PATID"= AL_T1D, "T1D" = c(rep(1,length(AL_T1D)))), data.frame("PATID"= setdiff(AL_m,AL_T1D), "T1D" = c(rep(0,length(setdiff(AL_m,AL_T1D))))))
UVE_data <- bind_rows(data.frame("PATID"= AL_UVE, "UVE" = c(rep(1,length(AL_UVE)))), data.frame("PATID"= setdiff(AL_m,AL_UVE), "UVE" = c(rep(0,length(setdiff(AL_m,AL_UVE))))))
GOUT_data <- bind_rows(data.frame("PATID"= AL_GOUT, "GOUT" = c(rep(1,length(AL_GOUT)))), data.frame("PATID"= setdiff(AL_m,AL_GOUT), "GOUT" = c(rep(0,length(setdiff(AL_m,AL_GOUT))))))
OST_data <- bind_rows(data.frame("PATID"= AL_OST, "OST" = c(rep(1,length(AL_OST)))), data.frame("PATID"= setdiff(AL_m,AL_OST), "OST" = c(rep(0,length(setdiff(AL_m,AL_OST))))))
CKD_data <- bind_rows(data.frame("PATID"= AL_CKD, "CKD" = c(rep(1,length(AL_CKD)))), data.frame("PATID"= setdiff(AL_m,AL_CKD), "CKD" = c(rep(0,length(setdiff(AL_m,AL_CKD))))))
ATF_data <- bind_rows(data.frame("PATID"= AL_ATF, "ATF" = c(rep(1,length(AL_ATF)))), data.frame("PATID"= setdiff(AL_m,AL_ATF), "ATF" = c(rep(0,length(setdiff(AL_m,AL_ATF))))))
PNEU_data <- bind_rows(data.frame("PATID"= AL_PNEU, "PNEU" = c(rep(1,length(AL_PNEU)))), data.frame("PATID"= setdiff(AL_m,AL_PNEU), "PNEU" = c(rep(0,length(setdiff(AL_m,AL_PNEU))))))
HIV_data <- bind_rows(data.frame("PATID"= AL_HIV, "HIV" = c(rep(1,length(AL_HIV)))), data.frame("PATID"= setdiff(AL_m,AL_HIV), "HIV" = c(rep(0,length(setdiff(AL_m,AL_HIV))))))
SCHI_data <- bind_rows(data.frame("PATID"= AL_SCHI, "SCHI" = c(rep(1,length(AL_SCHI)))), data.frame("PATID"= setdiff(AL_m,AL_SCHI), "SCHI" = c(rep(0,length(setdiff(AL_m,AL_SCHI))))))
ALZH_data <- bind_rows(data.frame("PATID"= AL_ALZH, "ALZH" = c(rep(1,length(AL_ALZH)))), data.frame("PATID"= setdiff(AL_m,AL_ALZH), "ALZH" = c(rep(0,length(setdiff(AL_m,AL_ALZH))))))
ASTH_data <- bind_rows(data.frame("PATID"= AL_ASTH, "ASTH" = c(rep(1,length(AL_ASTH)))), data.frame("PATID"= setdiff(AL_m,AL_ASTH), "ASTH" = c(rep(0,length(setdiff(AL_m,AL_ASTH))))))
DEP_data <- bind_rows(data.frame("PATID"= AL_DEP, "DEP" = c(rep(1,length(AL_DEP)))), data.frame("PATID"= setdiff(AL_m,AL_DEP), "DEP" = c(rep(0,length(setdiff(AL_m,AL_DEP))))))
ANX_data <- bind_rows(data.frame("PATID"= AL_ANX, "ANX" = c(rep(1,length(AL_ANX)))), data.frame("PATID"= setdiff(AL_m,AL_ANX), "ANX" = c(rep(0,length(setdiff(AL_m,AL_ANX))))))

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

AL_DATA <- inner_join(inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), COM_18, by="PATID"), yearBirth_data, by= "PATID"), OB_data, by="PATID")

glm.AL.T2D <- glm(data=AL_DATA, T2D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.T2D))
glm.AL.HTN <- glm(data=AL_DATA, HTN~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.HTN))
glm.AL.RA <- glm(data=AL_DATA, RA~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.RA))
glm.AL.IBD <- glm(data=AL_DATA, IBD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.IBD))
glm.AL.PARK <- glm(data=AL_DATA, PARK~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.PARK))
glm.AL.T1D <- glm(data=AL_DATA, T1D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.T1D))
glm.AL.UVE <- glm(data=AL_DATA, UVE~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.UVE))
glm.AL.GOUT <- glm(data=AL_DATA, GOUT~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.GOUT))
glm.AL.OST <- glm(data=AL_DATA, OST~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.OST))
glm.AL.CKD <- glm(data=AL_DATA, CKD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.CKD))
glm.AL.ATF<- glm(data=AL_DATA, ATF~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.ATF))
glm.AL.PNEU <- glm(data=AL_DATA, PNEU~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.PNEU))
glm.AL.HIV <- glm(data=AL_DATA, HIV~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.HIV))
glm.AL.SCHI <- glm(data=AL_DATA, SCHI~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.SCHI))
glm.AL.ALZH<- glm(data=AL_DATA, ALZH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.ALZH))
glm.AL.ASTH <- glm(data=AL_DATA, ASTH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.ASTH))
glm.AL.ANX <- glm(data=AL_DATA, ANX~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.ANX))
glm.AL.DEP <- glm(data=AL_DATA, DEP~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.AL.DEP))

save(glm.AL.T2D, glm.AL.HTN, glm.AL.RA, glm.AL.IBD, glm.AL.PARK, glm.AL.T1D, glm.AL.UVE, glm.AL.GOUT, glm.AL.OST,
     glm.AL.CKD, glm.AL.ATF, glm.AL.PNEU, glm.AL.HIV, glm.AL.SCHI, glm.AL.ALZH, glm.AL.ASTH, glm.AL.ANX, glm.AL.DEP, 
     file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AL_new_model.RData")

print(paste("total # of patients: ", total_number))
print(paste("# of female: ", num_female))
print(paste("# of male: ", num_male))