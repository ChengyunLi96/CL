library(dplyr)
library(readxl)
skin_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "skin_diseases")
comorbidities_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "comorbidities")
diagnose_VI <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/vitiligo/diagnose_1_vitiligo.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
patient_VI <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/vitiligo/patientInfo_vitiligo.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_VI <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/vitiligo/vitiligo_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)

derm_VI <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/vitiligo/vitiligo_derm.csv", sep=',' , 
                    colClasses=c(rep("character",1)), header= TRUE)

VI_PAID <- unique(demo_VI$X...PatientID)
DERM_VI_PAID <- unique(derm_VI$X...PatientID)
OC_VI_PAID <- setdiff(VI_PAID, DERM_VI_PAID)

print(paste0("Number of vitiligo patients: ", length(VI_PAID)))
print(paste0("Number of vitiligo patients diagnosed in Dermatology clinics: ", length(DERM_VI_PAID)))
print(paste0("Number of vitiligo patients diagnosed in non-Dermatology clinics: ", length(OC_VI_PAID)))

#PatientID of Caucasian
Caucasian_ID <- demo_VI[which(grepl("Caucasian",demo_VI$RaceName)),]$X...PatientID
#PatientID of Caucasian diagnosed as Vitiligo
VI_DERM_Cau_ID <- intersect(Caucasian_ID, DERM_VI_PAID)
VI_OC_Cau_ID <- intersect(Caucasian_ID, OC_VI_PAID)
print(paste0("Number of Caucasian vitiligo patients diagnosed in Dermatology clinics: ", length(VI_DERM_Cau_ID)))
print(paste0("Number of Caucasian vitiligo patients diagnosed in non-Dermatology clinics: ", length(VI_OC_Cau_ID)))

#PatientID of AA
AA_ID <- demo_VI[which(grepl("African American",demo_VI$RaceName)),]$X...PatientID
#PatientID of AA diagnosed as Vitiligo
VI_DERM_AA_ID <- intersect(AA_ID, DERM_VI_PAID)
VI_OC_AA_ID <- intersect(AA_ID, OC_VI_PAID)

print(paste0("Number of AA vitiligo patients diagnosed in Dermatology clinics: ", length(VI_DERM_AA_ID)))
print(paste0("Number of CauAAcasian vitiligo patients diagnosed in non-Dermatology clinics: ", length(VI_OC_AA_ID)))

#Diagnose table of VI is separated into four sub tables.
VI_Caucasian_DERM <- diagnose_VI %>% filter(X...PatientID %in% VI_DERM_Cau_ID)
VI_Caucasian_OC <- diagnose_VI %>% filter(X...PatientID %in% VI_OC_Cau_ID)
VI_AA_DERM <- diagnose_VI %>% filter(X...PatientID %in% VI_DERM_AA_ID)
VI_AA_OC <- diagnose_VI %>%  filter(X...PatientID %in% VI_OC_AA_ID)

comorbidity <- c("OB", "T2D", "HTN", "RA", "IBD", "PARK", "T1D", "CKD", "UVE", "GOUT", "OST", "ATF", "PNEU",
                 "HIV", "SCHI", "ALZH", "ASTH", "ANX", "DEP")

##number of Vitiligo patients with Obesity.
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
  Caucasian_DERM <- VI_Caucasian_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed Vitiligo in DERM with ", com, " : ",
               length(unique(Caucasian_DERM$X...PatientID))))
  Caucasian_OC <- VI_Caucasian_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed Vitiligo in OC with ", com, " : ",
               length(unique(Caucasian_OC$X...PatientID))))
  AA_DERM <- VI_AA_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed Vitiligo in DERM with ", com, " : ",
               length(unique(AA_DERM$X...PatientID))))
  AA_OC <- VI_AA_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed Vitiligo in OC with ", com, " : ",
               length(unique(AA_OC$X...PatientID))))
  VI_com <- paste0("VI_", com)
  assign(VI_com, c(unique(Caucasian_DERM$X...PatientID), unique(Caucasian_OC$X...PatientID), 
                   unique(AA_DERM$X...PatientID), unique(AA_OC$X...PatientID)))
}

save(VI_PAID, DERM_VI_PAID, OC_VI_PAID, VI_DERM_Cau_ID, VI_OC_Cau_ID, VI_DERM_AA_ID, VI_OC_AA_ID, VI_T2D,
     VI_HTN, VI_RA, VI_IBD, VI_PARK, VI_OB, VI_T1D,VI_UVE, VI_GOUT, VI_OST, VI_CKD,VI_ATF,
     VI_PNEU, VI_HIV, VI_SCHI, VI_ALZH, VI_ASTH, VI_ANX, VI_DEP, 
     file= "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/VI_new_clinic.RData")


