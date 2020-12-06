library(ggplot2)

comorbidity <- c("T2D", "HTN", "RA", "IBD", "T1D", "CKD", "GOUT", "OST", "ATF", "PNEU", "ASTH", "ANX", "DEP")

beta_all <- c()
beta_low_all <- c()
beta_up_all <- c()
odds_all <- c()
odds_low_all <- c()
odds_up_all <- c()
comorbidity_coef <- c()
comorbidity_odds <- c()
covariates <- rep(c("Ethnic", "Clinic", "Gender_F", "yearBirth", "OB", "Eth_Clin"), length(comorbidity))
#covariates <- rep(c("Ethnic", "Clinic", "Gender_F", "yearBirth", "OB", "Eth_Clin", "yearBirth_Clin", "OB_Clin"), length(comorbidity))
for(com in comorbidity){
  disease <- read.csv(file = paste0("/Users/stephen/Desktop/SummerResearch/fromMattew/psoriasis/model_summary_coefficient/", com, 
                                      "_coeff.csv"), sep =",", col.names = c("X", "Estimate", "Std.error", "Z.value", 
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

PS.coef <- data.frame(comorbidity_coef, covariates, beta_all, beta_low_all, beta_up_all)
PS.odds <- data.frame(comorbidity_odds, covariates, odds_all, odds_low_all, odds_up_all)

## bar graph
ggplot(PS.coef, aes(fill=covariates, y=beta_all, x=comorbidity_coef)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  theme_bw()
ggsave('/Users/stephen/Desktop/SummerResearch/fromMattew/psoriasis/model_summary_coefficient/coeffocient.png', 
       width = 16, height = 27, dpi = 100)

## Odds figure
ggplot(PS.odds, aes(x=odds_all, y=covariates)) + 
  geom_rect(aes(xmin = 0.001, xmax = 5,
                ymin = -Inf, ymax = Inf, fill = comorbidity_odds))+
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed') +
  geom_point(aes(colour = covariates), size = 3.5) +
  geom_errorbarh(aes(xmax = odds_up_all, xmin = odds_low_all), size = .5, height = .2, color = 'gray50') +
  scale_fill_manual(values = rep(c("#ffffff00", "#f0f0f090"), 13),
                    guide = "none")+
  facet_grid(comorbidity_odds~., switch = "y")+
  ylab("")+
  xlab("Odds ratio")+
  ggtitle("Odd ratios (OR) with 95% Confidence Interval")+
  theme_bw()
ggsave('/Users/stephen/Desktop/SummerResearch/fromMattew/psoriasis/model_summary_coefficient/odds.png', 
       width = 24, height = 36, dpi = 200)


cova <- c("Ethnic", "Clinic", "Gender_F", "yearBirth", "OB", "Eth_Clin")
for (i in cova){
  PS.new.coeff <- PS.coef[which(grepl(i, PS.coef$covariates)), ]
  PS.new.odds <- PS.odds[which(grepl(i, PS.odds$covariates)), ]
  
  
  ggplot(PS.new.odds, aes(x=reorder(comorbidity_odds, odds_all), y=odds_all)) + 
    
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
  
  ggsave(paste0('/Users/stephen/Desktop/SummerResearch/fromMattew/psoriasis/model_summary_coefficient/', i, '.odds.png'), 
         width = 12, height = 12, dpi = 300)
  
}




