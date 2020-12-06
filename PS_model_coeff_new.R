load("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/RData/PS_new_model.RData")

## function: create table of coefficients and odds ratio with their confidient intervals
coeff_odds <- function(glm.result){
  coeff <- summary.glm(glm.result)$coefficients
  coeff_int <- confint(glm.result)
  odds <- exp(coef(glm.result))
  odds_int <- exp(confint(glm.result))
  coeff_odds_table <- cbind(coeff, coeff_int, odds, odds_int)
  return(coeff_odds_table)
}

#comorbidity <- c("T2D", "HTN", "RA", "IBD", "PARK", "T1D", "CKD", "UVE", "GOUT", "OST", "ATF", "PNEU","HIV", "SCHI", "ALZH", "ASTH", "ANX", "DEP")
##comorbidities with good sample size (PS). 
comorbidity <- c("T2D", "HTN", "RA", "IBD", "T1D", "CKD", "GOUT", "OST", "ATF", "PNEU", "ASTH", "ANX", "DEP") #13

for (com in comorbidity){
  glm.result <- get(paste0("glm.PS.", com))
  table <- coeff_odds(glm.result)
  write.csv(table, file = paste0("/nfs/turbo/umms-alextsoi/lichengyun/EHR_data/fromMatthew/psoriasis/model_summary_coefficient/", 
                                 com, "_coeff.csv"))
  print(paste0(com, " are done"))
}
print(paste0(length(comorbidity), " tables of coefficients and odds ratio are created"))

