library(dplyr)
library(readxl)
skin_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "skin_diseases")
comorbidities_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "comorbidities")
diagnose_AL <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/alopeciaAreata/diagnose_1_alopeciaAreata.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)

patient_AL <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/alopeciaAreata/patientInfo_alopeciaAreata.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_AL <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/alopeciaAreata/alopeciaAreata_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)

derm_AL <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/alopeciaAreata/alopeciaAreata_derm.csv", sep=',' , 
                    colClasses=c(rep("character",1)), header= TRUE)

AL_PAID <- unique(demo_AL$X...PatientID)
DERM_AL_PAID <- unique(derm_AL$X...PatientID)
OC_AL_PAID <- setdiff(AL_PAID, DERM_AL_PAID)

print(paste0("Number of alopeciaAreata patients: ", length(AL_PAID)))
print(paste0("Number of alopeciaAreata patients diagnosed in Dermatology clinics: ", length(DERM_AL_PAID)))
print(paste0("Number of alopeciaAreata patients diagnosed in non-Dermatology clinics: ", length(OC_AL_PAID)))

#PatientID of Caucasian
Caucasian_ID <- demo_AL[which(grepl("Caucasian",demo_AL$RaceName)),]$X...PatientID
#PatientID of Caucasian diagnosed as alopeciaAreata
AL_DERM_Cau_ID <- intersect(Caucasian_ID, DERM_AL_PAID)
AL_OC_Cau_ID <- intersect(Caucasian_ID, OC_AL_PAID)
print(paste0("Number of Caucasian alopeciaAreata patients diagnosed in Dermatology clinics: ", length(AL_DERM_Cau_ID)))
print(paste0("Number of Caucasian alopeciaAreata patients diagnosed in non-Dermatology clinics: ", length(AL_OC_Cau_ID)))

#PatientID of AA
AA_ID <- demo_AL[which(grepl("African American",demo_AL$RaceName)),]$X...PatientID
#PatientID of AA diagnosed as alopeciaAreata
AL_DERM_AA_ID <- intersect(AA_ID, DERM_AL_PAID)
AL_OC_AA_ID <- intersect(AA_ID, OC_AL_PAID)

print(paste0("Number of AA alopeciaAreata patients diagnosed in Dermatology clinics: ", length(AL_DERM_AA_ID)))
print(paste0("Number of CauAAcasian alopeciaAreata patients diagnosed in non-Dermatology clinics: ", length(AL_OC_AA_ID)))

#Diagnose table of AL is separated into four sub tables.
AL_Caucasian_DERM <- diagnose_AL %>% filter(X...PatientID %in% AL_DERM_Cau_ID)
AL_Caucasian_OC <- diagnose_AL %>% filter(X...PatientID %in% AL_OC_Cau_ID)
AL_AA_DERM <- diagnose_AL %>% filter(X...PatientID %in% AL_DERM_AA_ID)
AL_AA_OC <- diagnose_AL %>%  filter(X...PatientID %in% AL_OC_AA_ID)

comorbidity <- c("OB", "T2D", "HTN", "RA", "IBD", "PARK", "T1D", "CKD", "UVE", "GOUT", "OST", "ATF", "PNEU",
                 "HIV", "SCHI", "ALZH", "ASTH", "ANX", "DEP")

##number of alopeciaAreata patients with Obesity.
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
  Caucasian_DERM <- AL_Caucasian_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed alopeciaAreata in DERM with ", com, " : ",
               length(unique(Caucasian_DERM$X...PatientID))))
  Caucasian_OC <- AL_Caucasian_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed alopeciaAreata in OC with ", com, " : ",
               length(unique(Caucasian_OC$X...PatientID))))
  AA_DERM <- AL_AA_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed alopeciaAreata in DERM with ", com, " : ",
               length(unique(AA_DERM$X...PatientID))))
  AA_OC <- AL_AA_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed alopeciaAreata in OC with ", com, " : ",
               length(unique(AA_OC$X...PatientID))))
  AL_com <- paste0("AL_", com)
  assign(AL_com, c(unique(Caucasian_DERM$X...PatientID), unique(Caucasian_OC$X...PatientID), 
                   unique(AA_DERM$X...PatientID), unique(AA_OC$X...PatientID)))
}

save(AL_PAID, DERM_AL_PAID, OC_AL_PAID, AL_DERM_Cau_ID, AL_OC_Cau_ID, AL_DERM_AA_ID, AL_OC_AA_ID, AL_T2D,
     AL_HTN, AL_RA, AL_IBD, AL_PARK, AL_OB, AL_T1D,AL_UVE, AL_GOUT, AL_OST, AL_CKD,AL_ATF,
     AL_PNEU, AL_HIV, AL_SCHI, AL_ALZH, AL_ASTH, AL_ANX, AL_DEP, 
     file= "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AL_new_clinic.RData")
