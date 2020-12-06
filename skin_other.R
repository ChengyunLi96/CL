library(dplyr)

load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/PS_demo_table.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/PS_new_clinic.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AC_demo_table.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AC_new_clinic.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AD_demo_table.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AD_new_clinic.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AL_demo_table.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/AL_new_clinic.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/VI_demo_table.RData")
load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/VI_new_clinic.RData")

## function: create table of coefficients and odds ratio with their confidient intervals
coeff_odds <- function(glm.result){
  coeff <- summary.glm(glm.result)$coefficients
  coeff_int <- confint(glm.result)
  odds <- exp(coef(glm.result))
  odds_int <- exp(confint(glm.result))
  coeff_odds_table <- cbind(coeff, coeff_int, odds, odds_int)
  return(coeff_odds_table)
}

#Ethnic: Caucasian = 0, African American = 1
#Clinic: Dermatology clinic = 1, other clinic = 0
#Gender: Female = 1, male = 0
#yearBirth: patients' year of birth 
#obesity: Obesity = 1 (BMI>= 30), otherwise = 0 
#Comorbidity diagnosed in DERM (Y=1),otherwise (Y=0)
#Ethnic*Clinic: interaction term


######skin disease:PS, comorbidities: AD
skin_diseases <- c("PS", "AD", "AC", "AL", "VI")
for (disease in skin_diseases){
  disease_PAID <- c(get(paste0(disease, "_DERM_Cau_ID")), get(paste0(disease, "_DERM_AA_ID")), 
                    get(paste0(disease, "_OC_Cau_ID")), get(paste0(disease, "_OC_AA_ID")))
  skin_com <- setdiff(skin_diseases, disease)
  for (com in skin_com){
    Cau_DERM <- get(paste0(com, "_DERM_Cau_ID"))
    AA_DERM <- get(paste0(com, "_DERM_AA_ID"))
    Cau_OC <- get(paste0(com, "_OC_Cau_ID"))
    AA_OC <- get(paste0(com, "_OC_AA_ID"))
    DERM_PAID <- c(Cau_DERM, AA_DERM)
    OC_PAID <- c(Cau_OC, AA_OC)
    disease_DERM_PAID <- intersect(disease_PAID, DERM_PAID) #Y=1
    print(paste0(disease, " patients with ", com, " in DERM: ", length(disease_DERM_PAID)))
    disease_NDERM_PAID <- setdiff(disease_PAID, DERM_PAID) #Y=0
    print(paste0(disease, " patients without ", com, " in DERM: ", length(disease_NDERM_PAID)))
    disease_com_data <- bind_rows(data.frame("PATID"= disease_DERM_PAID, com = c(rep(1,length(disease_DERM_PAID)))), 
                                  data.frame("PATID"= disease_NDERM_PAID, com = c(rep(0,length(disease_NDERM_PAID)))))
    disCom <- inner_join(get(paste0(disease,"_demo_DATA")), disease_com_data, by = "PATID")
    dis_Cau_DERM_com <- intersect(disease_DERM_PAID, get(paste0(disease, "_DERM_Cau_ID")))
    print(paste0("Cau_DERM ",length(dis_Cau_DERM_com)))
    dis_AA_DERM_com <- intersect(disease_DERM_PAID, get(paste0(disease, "_DERM_AA_ID")))
    print(paste0("AA_DERM ",length(dis_AA_DERM_com)))
    dis_Cau_OC_com <- intersect(disease_DERM_PAID, get(paste0(disease, "_OC_Cau_ID")))
    print(paste0("Cau_OC ",length(dis_Cau_OC_com)))
    dis_AA_OC_com <- intersect(disease_DERM_PAID, get(paste0(disease, "_OC_AA_ID")))
    print(paste0("AA_OC ",length(dis_AA_OC_com)))
    if (length(dis_Cau_DERM_com) >= 5 & length(dis_AA_DERM_com) >= 5 & length(dis_Cau_OC_com) >= 5 & length(dis_AA_OC_com) >= 5){
      glm <- paste0("glm.", disease, com)
      assign(glm, glm(data=disCom, com ~ Ethnic + Clinic + Gender_F + yearBirth + OB + Ethnic*Clinic, family = binomial(link=logit)))
      
      print(paste0("coomorbidity is: ", com))
      print(paste0("disease is: ", disease))
      
      glm.result <- get(glm)
      print(summary(glm.result))
      table <- coeff_odds(glm.result)
      write.csv(table, file = paste0("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/skin_com/model_summary_coefficient/", 
                                     disease, com, "_coeff.csv"))
    }
  }
}

library(ggplot2)




for (skin in skin_diseases){
  beta_all <- c()
  beta_low_all <- c()
  beta_up_all <- c()
  odds_all <- c()
  odds_low_all <- c()
  odds_up_all <- c()
  comorbidity_coef <- c()
  comorbidity_odds <- c()
  covariates <- rep(c("Ethnic", "Clinic", "Gender_F", "yearBirth", "OB", "Eth_Clin"), length(skin_diseases)-1)
  
  skin_com <- setdiff(skin_diseases, skin)
  for(com in skin_com){
    disease <- read.csv(file = paste0("/Users/stephen/Desktop/SummerResearch/fromMattew/skin_com/model_summary_coefficient/", skin,
                                      com, "_coeff.csv"), sep =",", col.names = c("X", "Estimate", "Std.error", "Z.value", 
                                                                             "Pr(>|z|)", "Estimate_2.5", "Estimate_97.5", 
                                                                             "odds", "odds_2.5", "odds_97.5"), 
                        header = TRUE)
    coeff <- disease$Estimate[2:7]
    coeff_low <- disease$Estimate_2.5[2:7]
    coeff_up <- disease$Estimate_97.5[2:7]
    odds <- disease$odds[2:7]
    odds_low <- disease$odds_2.5[2:7]
    odds_up <- disease$odds_97.5[2:7]
    
    beta_all <- c(beta_all, coeff)
    beta_low_all <- c(beta_low_all, coeff_low)
    beta_up_all <- c(beta_up_all, coeff_up)
    
    odds_all <- c(odds_all, odds)
    odds_low_all <- c(odds_low_all, odds_low)
    odds_up_all <- c(odds_up_all, odds_up)
    
    comorbidity_coef <- c(comorbidity_coef, rep(paste0(com, "_coef"), 6))
    comorbidity_odds <- c(comorbidity_odds, rep(paste0(com), 6))
    
  }
  print(paste0("loop done", skin))
  
  all_coeff <- data.frame(comorbidity_coef, covariates, beta_all, beta_low_all, beta_up_all)
  all_odds <- data.frame(comorbidity_odds, covariates, odds_all, odds_low_all, odds_up_all)
  
  cova <- c("Ethnic", "Clinic", "Gender_F", "yearBirth", "OB", "Eth_Clin")
  for (i in cova){
    new.coeff <- all_coeff[which(grepl(i, all_coeff$covariates)), ]
    new.odds <- all_odds[which(grepl(i, all_odds$covariates)), ]
    
    
    ggplot(new.odds, aes(x=reorder(comorbidity_odds, odds_all), y=odds_all)) + 
      
      geom_hline(aes(yintercept = 1), size = .25, linetype = 'dashed') +
      geom_point(aes(colour = comorbidity_odds), size = 5) +
      geom_errorbar(aes(ymax = odds_up_all, ymin = odds_low_all), size = 1, width = 0.5, color = 'gray50') +
      ylab("Odds ratio")+
      xlab("comorbidities")+
      ggtitle("Odd ratios (OR) with 95% Confidence Interval")+
      theme_bw()+
      theme(axis.text=element_text(size=15), axis.title=element_text(size=20,face="bold"), 
            axis.text.x = element_text(angle = 45, vjust = 0.65))+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.title = element_text(size =20))
    
    ggsave(paste0('/Users/stephen/Desktop/SummerResearch/fromMattew/skin_com/model_summary_coefficient/', skin, i, '.odds.png'), 
           width = 12, height = 12, dpi = 300)
    
  }
  print(paste0("plot done", skin))
}
