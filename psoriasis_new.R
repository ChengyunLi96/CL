library(dplyr)
library(readxl)
skin_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "skin_diseases")
comorbidities_ICD <- read_excel("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/ICD_codes/Using_ICD_codes.xlsx", sheet = "comorbidities")
diagnose1_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_1_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose2_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_2_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose3_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_3_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose4_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_4_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose5_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_5_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose6_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_6_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose7_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_7_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose8_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_8_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose9_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_9_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose10_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_10_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose11_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/diagnose_11_psoriasis.csv", 
                         sep=',', colClasses=c(rep("character",5)), header = TRUE)
diagnose_PS <- bind_rows(diagnose1_PS, diagnose2_PS, diagnose3_PS, diagnose4_PS, diagnose5_PS, diagnose6_PS, diagnose7_PS, 
                        diagnose8_PS, diagnose9_PS, diagnose10_PS, diagnose11_PS)

patient_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/patientInfo_psoriasis.csv", sep=',' ,
                       colClasses=c(rep("character",3)), header= TRUE)
demo_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/psoriasis_demo.csv", sep=',' , 
                   colClasses=c(rep("character",4)), header= TRUE)

derm_PS <- read.csv("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/psoriasis_derm.csv", sep=',' , 
                    colClasses=c(rep("character",1)), header= TRUE)

PS_PAID <- unique(demo_PS$X...PatientID)
DERM_PS_PAID <- unique(derm_PS$X...PatientID)
OC_PS_PAID <- setdiff(PS_PAID, DERM_PS_PAID)

print(paste0("Number of psoriasis patients: ", length(PS_PAID)))
print(paste0("Number of psoriasis patients diagnosed in Dermatology clinics: ", length(DERM_PS_PAID)))
print(paste0("Number of psoriasis patients diagnosed in non-Dermatology clinics: ", length(OC_PS_PAID)))

#PatientID of Caucasian
Caucasian_ID <- demo_PS[which(grepl("Caucasian",demo_PS$RaceName)),]$X...PatientID
#PatientID of Caucasian diagnosed as Psoriasis
PS_DERM_Cau_ID <- intersect(Caucasian_ID, DERM_PS_PAID)
PS_OC_Cau_ID <- intersect(Caucasian_ID, OC_PS_PAID)
print(paste0("Number of Caucasian psoriasis patients diagnosed in Dermatology clinics: ", length(PS_DERM_Cau_ID)))
print(paste0("Number of Caucasian psoriasis patients diagnosed in non-Dermatology clinics: ", length(PS_OC_Cau_ID)))

#PatientID of AA
AA_ID <- demo_PS[which(grepl("African American",demo_PS$RaceName)),]$X...PatientID
#PatientID of AA diagnosed as Psoriasis
PS_DERM_AA_ID <- intersect(AA_ID, DERM_PS_PAID)
PS_OC_AA_ID <- intersect(AA_ID, OC_PS_PAID)

print(paste0("Number of AA psoriasis patients diagnosed in Dermatology clinics: ", length(PS_DERM_AA_ID)))
print(paste0("Number of CauAAcasian psoriasis patients diagnosed in non-Dermatology clinics: ", length(PS_OC_AA_ID)))

#Diagnose table of PS is separated into four sub tables.
PS_Caucasian_DERM <- diagnose_PS %>% filter(X...PatientID %in% PS_DERM_Cau_ID)
PS_Caucasian_OC <- diagnose_PS %>% filter(X...PatientID %in% PS_OC_Cau_ID)
PS_AA_DERM <- diagnose_PS %>% filter(X...PatientID %in% PS_DERM_AA_ID)
PS_AA_OC <- diagnose_PS %>%  filter(X...PatientID %in% PS_OC_AA_ID)

comorbidity <- c("OB", "T2D", "HTN", "RA", "IBD", "PARK", "T1D", "CKD", "UVE", "GOUT", "OST", "ATF", "PNEU",
                 "HIV", "SCHI", "ALZH", "ASTH", "ANX", "DEP")

##number of Psoriasis patients with Obesity.
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
  Caucasian_DERM <- PS_Caucasian_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed Psoriasis in DERM with ", com, " : ",
               length(unique(Caucasian_DERM$X...PatientID))))
  Caucasian_OC <- PS_Caucasian_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of Caucasian patients dignosed Psoriasis in OC with ", com, " : ",
               length(unique(Caucasian_OC$X...PatientID))))
  AA_DERM <- PS_AA_DERM %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed Psoriasis in DERM with ", com, " : ",
               length(unique(AA_DERM$X...PatientID))))
  AA_OC <- PS_AA_OC %>% 
    filter(TermCodeMapped %in% code)
  print(paste0("Number of AA patients dignosed Psoriasis in OC with ", com, " : ",
               length(unique(AA_OC$X...PatientID))))
  PS_com <- paste0("PS_", com)
  assign(PS_com, c(unique(Caucasian_DERM$X...PatientID), unique(Caucasian_OC$X...PatientID), 
              unique(AA_DERM$X...PatientID), unique(AA_OC$X...PatientID)))
}

save(PS_PAID, DERM_PS_PAID, OC_PS_PAID, PS_DERM_Cau_ID, PS_OC_Cau_ID, PS_DERM_AA_ID, PS_OC_AA_ID, PS_T2D,
     PS_HTN, PS_RA, PS_IBD, PS_PARK, PS_OB, PS_T1D,PS_UVE, PS_GOUT, PS_OST, PS_CKD,PS_ATF,
     PS_PNEU, PS_HIV, PS_SCHI, PS_ALZH, PS_ASTH, PS_ANX, PS_DEP, 
     file= "/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/PS_new_clinic.RData")
  

