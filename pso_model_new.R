library(dplyr)
library(lubridate)
library(pastecs)
patient_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/patientInfo_psoriasis.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/psoriasis_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/PS_new_clinic.RData")
#Ethnic: Caucasian = 0, African American = 1
#Clinic: Dermatology clinic = 1, other clinic = 0
#Gender: Female = 1, male = 0
#yearBirth: date of birth (year)
#obesity: Obesity = 1 (BMI>= 30), otherwise = 0 
#Ethnic*Clinic: interaction term

#Modeling: Comorbidity ~ Ethnic + Clinic + Demographics (gender/age of the skin disease diagnosis) + Obesity+ Ethnic*Clinic
Cau_DERM_data <- data.frame("PATID" = PS_DERM_Cau_ID, "Ethnic" = rep(0, length(PS_DERM_Cau_ID)), "Clinic" = rep(1, length(PS_DERM_Cau_ID)))
Cau_OC_data <- data.frame("PATID" = PS_OC_Cau_ID, "Ethnic" = rep(0, length(PS_OC_Cau_ID)), "Clinic" = rep(0, length(PS_OC_Cau_ID)))
AA_DERM_data <- data.frame("PATID" = PS_DERM_AA_ID, "Ethnic" = rep(1, length(PS_DERM_AA_ID)), "Clinic" = rep(1, length(PS_DERM_AA_ID)))
AA_OC_data <- data.frame("PATID" = PS_OC_AA_ID, "Ethnic" = rep(1, length(PS_OC_AA_ID)), "Clinic" = rep(0, length(PS_OC_AA_ID)))

Eth_Cli_data <- bind_rows(Cau_DERM_data, Cau_OC_data, AA_DERM_data, AA_OC_data)

PS_m <- c(PS_DERM_Cau_ID, PS_OC_Cau_ID, PS_DERM_AA_ID, PS_OC_AA_ID)
PS_DERM <- c(PS_DERM_Cau_ID, PS_DERM_AA_ID)
PS_OC <- c(PS_OC_Cau_ID, PS_OC_AA_ID)
demo <- demo_PS %>% filter(X...PatientID %in% PS_m)
Gender_data <- data.frame("PATID" = demo$X...PatientID, "Gender" = demo$GenderCode)
Gender_data$Gender_F <- ifelse(Gender_data$Gender=="F",1,0)

total_number <- nrow(demo)
num_female <- sum(Gender_data$Gender_F)
num_male <- total_number - num_female


#obesity
OB_data <- bind_rows(data.frame("PATID"= PS_OB, "OB" = c(rep(1,length(PS_OB)))), 
                     data.frame("PATID"= setdiff(PS_m,PS_OB), "OB" = c(rep(0,length(setdiff(PS_m,PS_OB))))))

##yearBirth
patient <- patient_PS %>% filter(X...PatientID %in% PS_m)
yearBirth_data <- data.frame("PATID" = patient$X...PatientID, "yearBirth" = year(mdy_hm(patient$DOB)))

PS_demo_DATA <- inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), 
                                      yearBirth_data, by= "PATID"), OB_data, by="PATID")
save(PS_demo_DATA, file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/PS_demo_table.RData")

#18 comorbidities
T2D_data <- bind_rows(data.frame("PATID"= PS_T2D, "T2D" = c(rep(1,length(PS_T2D)))), data.frame("PATID"= setdiff(PS_m,PS_T2D), "T2D" = c(rep(0,length(setdiff(PS_m,PS_T2D))))))
HTN_data <- bind_rows(data.frame("PATID"= PS_HTN, "HTN" = c(rep(1,length(PS_HTN)))), data.frame("PATID"= setdiff(PS_m,PS_HTN), "HTN" = c(rep(0,length(setdiff(PS_m,PS_HTN))))))
RA_data <- bind_rows(data.frame("PATID"= PS_RA, "RA" = c(rep(1,length(PS_RA)))), data.frame("PATID"= setdiff(PS_m,PS_RA), "RA" = c(rep(0,length(setdiff(PS_m,PS_RA))))))
IBD_data <- bind_rows(data.frame("PATID"= PS_IBD, "IBD" = c(rep(1,length(PS_IBD)))), data.frame("PATID"= setdiff(PS_m,PS_IBD), "IBD" = c(rep(0,length(setdiff(PS_m,PS_IBD))))))
PARK_data <- bind_rows(data.frame("PATID"= PS_PARK, "PARK" = c(rep(1,length(PS_PARK)))), data.frame("PATID"= setdiff(PS_m,PS_PARK), "PARK" = c(rep(0,length(setdiff(PS_m,PS_PARK))))))
T1D_data <- bind_rows(data.frame("PATID"= PS_T1D, "T1D" = c(rep(1,length(PS_T1D)))), data.frame("PATID"= setdiff(PS_m,PS_T1D), "T1D" = c(rep(0,length(setdiff(PS_m,PS_T1D))))))
UVE_data <- bind_rows(data.frame("PATID"= PS_UVE, "UVE" = c(rep(1,length(PS_UVE)))), data.frame("PATID"= setdiff(PS_m,PS_UVE), "UVE" = c(rep(0,length(setdiff(PS_m,PS_UVE))))))
GOUT_data <- bind_rows(data.frame("PATID"= PS_GOUT, "GOUT" = c(rep(1,length(PS_GOUT)))), data.frame("PATID"= setdiff(PS_m,PS_GOUT), "GOUT" = c(rep(0,length(setdiff(PS_m,PS_GOUT))))))
OST_data <- bind_rows(data.frame("PATID"= PS_OST, "OST" = c(rep(1,length(PS_OST)))), data.frame("PATID"= setdiff(PS_m,PS_OST), "OST" = c(rep(0,length(setdiff(PS_m,PS_OST))))))
CKD_data <- bind_rows(data.frame("PATID"= PS_CKD, "CKD" = c(rep(1,length(PS_CKD)))), data.frame("PATID"= setdiff(PS_m,PS_CKD), "CKD" = c(rep(0,length(setdiff(PS_m,PS_CKD))))))
ATF_data <- bind_rows(data.frame("PATID"= PS_ATF, "ATF" = c(rep(1,length(PS_ATF)))), data.frame("PATID"= setdiff(PS_m,PS_ATF), "ATF" = c(rep(0,length(setdiff(PS_m,PS_ATF))))))
PNEU_data <- bind_rows(data.frame("PATID"= PS_PNEU, "PNEU" = c(rep(1,length(PS_PNEU)))), data.frame("PATID"= setdiff(PS_m,PS_PNEU), "PNEU" = c(rep(0,length(setdiff(PS_m,PS_PNEU))))))
HIV_data <- bind_rows(data.frame("PATID"= PS_HIV, "HIV" = c(rep(1,length(PS_HIV)))), data.frame("PATID"= setdiff(PS_m,PS_HIV), "HIV" = c(rep(0,length(setdiff(PS_m,PS_HIV))))))
SCHI_data <- bind_rows(data.frame("PATID"= PS_SCHI, "SCHI" = c(rep(1,length(PS_SCHI)))), data.frame("PATID"= setdiff(PS_m,PS_SCHI), "SCHI" = c(rep(0,length(setdiff(PS_m,PS_SCHI))))))
ALZH_data <- bind_rows(data.frame("PATID"= PS_ALZH, "ALZH" = c(rep(1,length(PS_ALZH)))), data.frame("PATID"= setdiff(PS_m,PS_ALZH), "ALZH" = c(rep(0,length(setdiff(PS_m,PS_ALZH))))))
ASTH_data <- bind_rows(data.frame("PATID"= PS_ASTH, "ASTH" = c(rep(1,length(PS_ASTH)))), data.frame("PATID"= setdiff(PS_m,PS_ASTH), "ASTH" = c(rep(0,length(setdiff(PS_m,PS_ASTH))))))
DEP_data <- bind_rows(data.frame("PATID"= PS_DEP, "DEP" = c(rep(1,length(PS_DEP)))), data.frame("PATID"= setdiff(PS_m,PS_DEP), "DEP" = c(rep(0,length(setdiff(PS_m,PS_DEP))))))
ANX_data <- bind_rows(data.frame("PATID"= PS_ANX, "ANX" = c(rep(1,length(PS_ANX)))), data.frame("PATID"= setdiff(PS_m,PS_ANX), "ANX" = c(rep(0,length(setdiff(PS_m,PS_ANX))))))

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

PS_DATA <- inner_join(inner_join(inner_join(inner_join(Eth_Cli_data,Gender_data, by ="PATID"), COM_18, by="PATID"), yearBirth_data, by= "PATID"), OB_data, by="PATID")

glm.PS.T2D <- glm(data=PS_DATA, T2D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.T2D))
glm.PS.HTN <- glm(data=PS_DATA, HTN~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.HTN))
glm.PS.RA <- glm(data=PS_DATA, RA~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.RA))
glm.PS.IBD <- glm(data=PS_DATA, IBD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.IBD))
glm.PS.PARK <- glm(data=PS_DATA, PARK~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.PARK))
glm.PS.T1D <- glm(data=PS_DATA, T1D~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.T1D))
glm.PS.UVE <- glm(data=PS_DATA, UVE~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.UVE))
glm.PS.GOUT <- glm(data=PS_DATA, GOUT~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.GOUT))
glm.PS.OST <- glm(data=PS_DATA, OST~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.OST))
glm.PS.CKD <- glm(data=PS_DATA, CKD~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.CKD))
glm.PS.ATF<- glm(data=PS_DATA, ATF~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.ATF))
glm.PS.PNEU <- glm(data=PS_DATA, PNEU~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.PNEU))
glm.PS.HIV <- glm(data=PS_DATA, HIV~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.HIV))
glm.PS.SCHI <- glm(data=PS_DATA, SCHI~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.SCHI))
glm.PS.ALZH<- glm(data=PS_DATA, ALZH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.ALZH))
glm.PS.ASTH <- glm(data=PS_DATA, ASTH~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.ASTH))
glm.PS.ANX <- glm(data=PS_DATA, ANX~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.ANX))
glm.PS.DEP <- glm(data=PS_DATA, DEP~Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit))
print(summary(glm.PS.DEP))


save(glm.PS.T2D, glm.PS.HTN, glm.PS.RA, glm.PS.IBD, glm.PS.PARK, glm.PS.T1D, glm.PS.UVE, glm.PS.GOUT, glm.PS.OST,
     glm.PS.CKD, glm.PS.ATF, glm.PS.PNEU, glm.PS.HIV, glm.PS.SCHI, glm.PS.ALZH, glm.PS.ASTH, glm.PS.ANX, glm.PS.DEP, 
     file = "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/PS_new_model.RData")

print(paste("total # of patients: ", total_number))
print(paste("# of female: ", num_female))
print(paste("# of male: ", num_male))

            
            
            
            
            
            
            
            
            