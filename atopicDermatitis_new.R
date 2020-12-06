library(dplyr)
library(readxl)
skin_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "skin_diseases")
comorbidities_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "comorbidities")
diagnose_AD <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/atopicDermatitis/diagnose_1_atopicDermatitis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)

patient_AD <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/atopicDermatitis/patientInfo_atopicDermatitis.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_AD <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/atopicDermatitis/atopicDermatitis_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)

derm_AD <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/atopicDermatitis/atopicDermatitis_derm.csv", sep=',' , 
                    colClasses=c(rep("character",1)), header= TRUE)

AD_PAID <- unique(demo_AD$X...PatientID)
DERM_AD_PAID <- unique(derm_AD$X...PatientID)
OC_AD_PAID <- setdiff(AD_PAID, DERM_AD_PAID)

print(paste0("Number of atopicDermatitis patients: ", length(AD_PAID)))
print(paste0("Number of atopicDermatitis patients diagnosed in Dermatology clinics: ", length(DERM_AD_PAID)))
print(paste0("Number of atopicDermatitis patients diagnosed in non-Dermatology clinics: ", length(OC_AD_PAID)))

#PatientID of Caucasian
Caucasian_ID <- demo_AD[which(grepl("Caucasian",demo_AD$RaceName)),]$X...PatientID
#PatientID of Caucasian diagnosed as atopicDermatitis
AD_DERM_Cau_ID <- intersect(Caucasian_ID, DERM_AD_PAID)
AD_OC_Cau_ID <- intersect(Caucasian_ID, OC_AD_PAID)
print(paste0("Number of Caucasian atopicDermatitis patients diagnosed in Dermatology clinics: ", length(AD_DERM_Cau_ID)))
print(paste0("Number of Caucasian atopicDermatitis patients diagnosed in non-Dermatology clinics: ", length(AD_OC_Cau_ID)))

#PatientID of AA
AA_ID <- demo_AD[which(grepl("African American",demo_AD$RaceName)),]$X...PatientID
#PatientID of AA diagnosed as atopicDermatitis
AD_DERM_AA_ID <- intersect(AA_ID, DERM_AD_PAID)
AD_OC_AA_ID <- intersect(AA_ID, OC_AD_PAID)

print(paste0("Number of AA atopicDermatitis patients diagnosed in Dermatology clinics: ", length(AD_DERM_AA_ID)))
print(paste0("Number of CauAAcasian atopicDermatitis patients diagnosed in non-Dermatology clinics: ", length(AD_OC_AA_ID)))

#Diagnose table of AD is separated into four sub tables.
AD_Caucasian_DERM <- diagnose_AD %>% filter(X...PatientID %in% AD_DERM_Cau_ID)
AD_Caucasian_OC <- diagnose_AD %>% filter(X...PatientID %in% AD_OC_Cau_ID)
AD_AA_DERM <- diagnose_AD %>% filter(X...PatientID %in% AD_DERM_AA_ID)
AD_AA_OC <- diagnose_AD %>%  filter(X...PatientID %in% AD_OC_AA_ID)

comorbidity <- c("OB", "T2D", "HTN", "RA", "IBD", "PARK", "T1D", "CKD", "UVE", "GOUT", "OST", "ATF", "PNEU",
                 "HIV", "SCHI", "ALZH", "ASTH", "ANX", "DEP")

##number of atopicDermatitis patients with Obesity.
#create vector contains ICD 9/10 for OB
OB_code <- comorbidities_ICD[2166:2210, 3][[1]]
T2D_code <- comorbidities_ICD[149:284, 3][[1]]
HTN_code <- comorbidities_ICD[392:398, 3][[1]]
RA_code <- comorbidities_ICD[1678:2097, 3][[1]]
CD_code1 <- comorbidities_ICD[285:289, 3][[1]]
CD_code2 <- comorbidities_ICD[290:326, 3][[1]]
UC_code1 <- comorbidities_ICD[327:336, 3][[1]]
UC_code2 <- comorbidities_ICD[337:391, 3][[1]]
IBD_code <- c(CD_code1, CD_code2, UC_code1, UC_code2)
PARK_code <- c("332.0", "G20")
T1D_code <- comorbidities_ICD[17:148, 3][[1]]
CKD_code <- comorbidities_ICD[1:16, 3][[1]]
UVE_code <- comorbidities_ICD[2104:2165, 3][[1]]
GOUT_code <- comorbidities_ICD[878:1458, 3][[1]]
OST_code <- comorbidities_ICD[488:877,3][[1]]
ATF_code <- comorbidities_ICD[1505:1513, 3][[1]]
PNEU_code <- comorbidities_ICD[1514:1592,3][[1]]
HIV_code <- c("B20","042")
SCHI_code <- comorbidities_ICD[1595:1675,3][[1]]
ALZH_code <- comorbidities_ICD[2098:2103, 3][[1]]
ASTH_code <- comorbidities_ICD[1459:1504, 3][[1]]
ANX_code <- comorbidities_ICD[399:448, 3][[1]]
DEP_code <- comorbidities_ICD[449:487, 3][[1]]

for (com in comorbidity) {
  code <- get(paste0(com, "_code"))
  Caucasian_DERM <- AD_Caucasian_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed atopicDermatitis in DERM with ", com, " : ",
               length(unique(Caucasian_DERM$X...PatientID))))
  Caucasian_OC <- AD_Caucasian_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed atopicDermatitis in OC with ", com, " : ",
               length(unique(Caucasian_OC$X...PatientID))))
  AA_DERM <- AD_AA_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed atopicDermatitis in DERM with ", com, " : ",
               length(unique(AA_DERM$X...PatientID))))
  AA_OC <- AD_AA_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed atopicDermatitis in OC with ", com, " : ",
               length(unique(AA_OC$X...PatientID))))
  AD_com <- paste0("AD_", com)
  assign(AD_com, c(unique(Caucasian_DERM$X...PatientID), unique(Caucasian_OC$X...PatientID), 
                   unique(AA_DERM$X...PatientID), unique(AA_OC$X...PatientID)))
}

save(AD_PAID, DERM_AD_PAID, OC_AD_PAID, AD_DERM_Cau_ID, AD_OC_Cau_ID, AD_DERM_AA_ID, AD_OC_AA_ID, AD_T2D,
     AD_HTN, AD_RA, AD_IBD, AD_PARK, AD_OB, AD_T1D,AD_UVE, AD_GOUT, AD_OST, AD_CKD,AD_ATF,
     AD_PNEU, AD_HIV, AD_SCHI, AD_ALZH, AD_ASTH, AD_ANX, AD_DEP, 
     file= "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AD_new_clinic.RData")


