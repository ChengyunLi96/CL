library(dplyr)
library(readxl)
skin_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "skin_diseases")
comorbidities_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "comorbidities")
diagnose1_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_1_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose2_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_2_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose3_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_3_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose4_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_4_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose5_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_5_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose6_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_6_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose7_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_7_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose8_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_8_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose9_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_9_acne.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose10_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_10_acne.csv", 
                          sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose11_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_11_acne.csv", 
                          sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose12_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_12_acne.csv", 
                          sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose13_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_13_acne.csv", 
                          sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose14_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_14_acne.csv", 
                          sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose15_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_15_acne.csv", 
                          sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose16_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/diagnose_16_acne.csv", 
                          sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose_AC <- bind_rows(diagnose1_AC, diagnose2_AC, diagnose3_AC, diagnose4_AC, diagnose5_AC, diagnose6_AC, diagnose7_AC, 
                         diagnose8_AC, diagnose9_AC, diagnose10_AC, diagnose11_AC, diagnose12_AC, diagnose13_AC, diagnose14_AC,
                         diagnose15_AC, diagnose16_AC)

patient_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/patientInfo_acne.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/acne_demo.csv", sep=',' , 
                    colClasses=c(rep("character",4)), header= TRUE)

derm_AC <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/acne/acne_derm.csv", sep=',' , 
                    colClasses=c(rep("character",1)), header= TRUE)

AC_PAID <- unique(demo_AC$X...PatientID)
DERM_AC_PAID <- unique(derm_AC$X...PatientID)
OC_AC_PAID <- setdiff(AC_PAID, DERM_AC_PAID)

print(paste0("Number of acne patients: ", length(AC_PAID)))
print(paste0("Number of acne patients diagnosed in Dermatology clinics: ", length(DERM_AC_PAID)))
print(paste0("Number of acne patients diagnosed in non-Dermatology clinics: ", length(OC_AC_PAID)))

#PatientID of Caucasian
Caucasian_ID <- demo_AC[which(grepl("Caucasian",demo_AC$RaceName)),]$X...PatientID
#PatientID of Caucasian diagnosed as Acne
AC_DERM_Cau_ID <- intersect(Caucasian_ID, DERM_AC_PAID)
AC_OC_Cau_ID <- intersect(Caucasian_ID, OC_AC_PAID)
print(paste0("Number of Caucasian acne patients diagnosed in Dermatology clinics: ", length(AC_DERM_Cau_ID)))
print(paste0("Number of Caucasian acne patients diagnosed in non-Dermatology clinics: ", length(AC_OC_Cau_ID)))

#PatientID of AA
AA_ID <- demo_AC[which(grepl("African American",demo_AC$RaceName)),]$X...PatientID
#PatientID of AA diagnosed as Acne
AC_DERM_AA_ID <- intersect(AA_ID, DERM_AC_PAID)
AC_OC_AA_ID <- intersect(AA_ID, OC_AC_PAID)

print(paste0("Number of AA acne patients diagnosed in Dermatology clinics: ", length(AC_DERM_AA_ID)))
print(paste0("Number of CauAAcasian acne patients diagnosed in non-Dermatology clinics: ", length(AC_OC_AA_ID)))

#Diagnose table of AC is separated into four sub tables.
AC_Caucasian_DERM <- diagnose_AC %>% filter(X...PatientID %in% AC_DERM_Cau_ID)
AC_Caucasian_OC <- diagnose_AC %>% filter(X...PatientID %in% AC_OC_Cau_ID)
AC_AA_DERM <- diagnose_AC %>% filter(X...PatientID %in% AC_DERM_AA_ID)
AC_AA_OC <- diagnose_AC %>%  filter(X...PatientID %in% AC_OC_AA_ID)

comorbidity <- c("OB", "T2D", "HTN", "RA", "IBD", "PARK", "T1D", "CKD", "UVE", "GOUT", "OST", "ATF", "PNEU",
                 "HIV", "SCHI", "ALZH", "ASTH", "ANX", "DEP")

##number of Acne patients with Obesity.
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
  Caucasian_DERM <- AC_Caucasian_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed Acne in DERM with ", com, " : ",
               length(unique(Caucasian_DERM$X...PatientID))))
  Caucasian_OC <- AC_Caucasian_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed Acne in OC with ", com, " : ",
               length(unique(Caucasian_OC$X...PatientID))))
  AA_DERM <- AC_AA_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed Acne in DERM with ", com, " : ",
               length(unique(AA_DERM$X...PatientID))))
  AA_OC <- AC_AA_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed Acne in OC with ", com, " : ",
               length(unique(AA_OC$X...PatientID))))
  AC_com <- paste0("AC_", com)
  assign(AC_com, c(unique(Caucasian_DERM$X...PatientID), unique(Caucasian_OC$X...PatientID), 
                   unique(AA_DERM$X...PatientID), unique(AA_OC$X...PatientID)))
}

save(AC_PAID, DERM_AC_PAID, OC_AC_PAID, AC_DERM_Cau_ID, AC_OC_Cau_ID, AC_DERM_AA_ID, AC_OC_AA_ID, AC_T2D,
     AC_HTN, AC_RA, AC_IBD, AC_PARK, AC_OB, AC_T1D,AC_UVE, AC_GOUT, AC_OST, AC_CKD,AC_ATF,
     AC_PNEU, AC_HIV, AC_SCHI, AC_ALZH, AC_ASTH, AC_ANX, AC_DEP, 
     file= "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AC_new_clinic.RData")


