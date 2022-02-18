rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/moral bias")


# libraries  -------------------------------------------------------------------
library(lavaan)                    # for running the models
library(semPlot)                   # for plotting the sem graphs
library(tidyverse)                 # for renaming


# data -------------------------------------------------------------------------
data <- read.csv("impressiondata_moralbias.csv")  
# nb. informants are aggregated (items formatted i_[trait]_i1)

data = 
  data %>% rename(SP_attractive = SP_physically.attractive,
                  i_attractive_i1 = i_physically.attractive_i1)


# ------------------------------------------------------------------------------
# --------------------------------- informant ----------------------------------
# model 6: ability  ------------------------------------------------------------ 
i_a = '# ability factor
        i_ability =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                   start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 
        '

i_a.fit = cfa(i_a, data, missing='fiml')
summary(i_a.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_a.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 7: warmth  -------------------------------------------------------------
i_w = '# warmth factor
       i_warmth =~ start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                  start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                  start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                  start(1)*i_cooperative_i1 + start(1)*i_patient_i1 
                  
      '

i_w.fit = cfa(i_w, data, missing='fiml')
summary(i_w.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_w.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 8: morality ------------------------------------------------------------
i_m = '# moral factor
      i_moral =~ start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + start(1)*i_loyal_i1'

i_m.fit = cfa(i_m, data, missing='fiml')
summary(i_m.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_m.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 10: warmth, morality, ability ------------------------------------------
i_wma = '# ability factor
        i_ability =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                   start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 

        # warmth factor
        i_warmth =~ start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                  start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                  start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                  start(1)*i_cooperative_i1 + start(1)*i_patient_i1
                  
        
        # moral factor
        i_moral =~ start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + 
        start(1)*i_loyal_i1

          
        # allow factors to correlate
        i_moral ~~ i_warmth
        i_moral ~~ i_ability
        i_warmth ~~ i_ability
        '

i_wma.fit = cfa(i_wma, data, missing='fiml')
summary(i_wma.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_wma.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)

# positivity-only model --------------------------------------------------------
i_posonly = '
        positivity =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                    start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 +
                    start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                    start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                    start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                    start(1)*i_cooperative_i1 + start(1)*i_patient_i1 + 
                    start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + 
                    start(1)*i_loyal_i1
              '

i_posonly.fit = cfa(i_posonly, data, missing='fiml')
summary(i_posonly.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_posonly.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# ------------------------------------------------------------------------------
# ----------------------------- self & informant -------------------------------
# model 11: agreement pre-bias -------------------------------------------------
si_wma = '## self  --------------------------------------------------------
          # ability factor
          s_ability =~ start(1)*SP_creative + start(1)*SP_intelligent +
                    start(1)*SP_socially.skilled + start(1)*SP_funny
          
          # warmth factor
          s_warmth =~ start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                    start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                    start(1)*SP_cooperative + start(1)*SP_patient
          
          
          # moral factor
          s_moral =~ start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
          
          
          ## informant -----------------------------------------------------
          # ability factor
          i_ability =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                     start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 
  
          # warmth factor
          i_warmth =~ start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                    start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                    start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                    start(1)*i_cooperative_i1 + start(1)*i_patient_i1
                    
          # moral factor
          i_moral =~ start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + 
          start(1)*i_loyal_i1
          
          
          # agreement ----------------------------------------------------------
          s_warmth ~~ i_warmth
          s_moral ~~ i_moral
          s_ability ~~ i_ability
          
          # factors are independent except agreement
          s_warmth ~~ 0*s_moral
          s_warmth ~~ 0*s_ability
          s_warmth ~~ 0*i_moral
          s_warmth ~~ 0*i_ability
          
          s_moral ~~ 0*s_ability
          s_moral ~~ 0*i_ability
          s_moral ~~ 0*i_warmth
          
          s_ability ~~ 0*i_warmth
          s_ability ~~ 0*i_moral
          
          i_warmth ~~ 0*i_moral
          i_warmth ~~ 0*i_ability
          i_moral ~~ 0*i_ability
          '

si_wma.fit = cfa(si_wma, data, missing='fiml')
summary(si_wma.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(si_wma.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 3, sizeLat = 5)


lavInspect(si_wma.fit, what="est")$psi
lavInspect(si_wma.fit, what="std.all")$psi

# get CIs for std correlations
x=standardizedSolution(si_wma.fit)


# model 16: informant bias bifactor --------------------------------------------
i_bi = '# ability factor
          i_ability =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                     start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 
  
          # warmth factor
          i_warmth =~ start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                    start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                    start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                    start(1)*i_cooperative_i1 + start(1)*i_patient_i1
                    
          # compassionate residual fixed to 0 
          i_compassionate_i1 ~~ 0*i_compassionate_i1
          
          # moral factor
          i_moral =~ start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + 
          start(1)*i_loyal_i1

                     
        # bias factor
        i_bias =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                  start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 +
                  start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                  start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                  start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                  start(1)*i_cooperative_i1 + start(1)*i_patient_i1 + 
                  start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + 
                  start(1)*i_loyal_i1 
                      
        # main factors independent of bias
        i_bias~~0*i_ability
        i_bias~~0*i_warmth
        i_bias~~0*i_moral
        
        
        i_moral ~~ 0*i_warmth
        i_moral ~~ 0*i_ability
        i_ability ~~ 0*i_warmth
        
          '


i_bi.fit = cfa(i_bi, data, missing='fiml')
summary(i_bi.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_bi.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("i_bias"), rotation = 3)

lavInspect(i_bi.fit, what="est")$psi
lavInspect(i_bi.fit, what="std")$psi



# model 17: agreement post-bias BIFACTOR  --------------------------------------
si_bi = '## self  --------------------------------------------------------
          # ability factor
         s_ability =~ start(1)*SP_creative + start(1)*SP_intelligent +
                    start(1)*SP_socially.skilled + start(1)*SP_funny
          
          
          # warmth factor
          s_warmth =~ start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                    start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                    start(1)*SP_cooperative + start(1)*SP_patient
          
          
          # moral factor
          s_moral =~ start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
          
          # loyal residual fixed to 0
          SP_loyal ~~ 0*SP_loyal
          
          
          # bias factor
          s_bias =~ start(1)*SP_creative + start(1)*SP_intelligent +
                      start(1)*SP_socially.skilled + start(1)*SP_funny +
                      start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                      start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                      start(1)*SP_cooperative + start(1)*SP_patient +
                      start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
          
          
         ## informant -----------------------------------------------------
         # ability factor
          i_ability =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                     start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 
                     
          # socially skilled residual fixed to 0
          i_socially.skilled_i1 ~~ 0*i_socially.skilled_i1
  
          # warmth factor
          i_warmth =~ start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                    start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                    start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                    start(1)*i_cooperative_i1 + start(1)*i_patient_i1
                    
          # compassionate residual fixed to 0
          i_compassionate_i1 ~~ 0*i_compassionate_i1
          
          # moral factor
          i_moral =~ start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + 
                    start(1)*i_loyal_i1
          
                     
          # bias factor
          i_bias =~ start(1)*i_creative_i1 + start(1)*i_intelligent_i1 +
                    start(1)*i_socially.skilled_i1 + start(1)*i_funny_i1 +
                    start(1)*i_compassionate_i1 + start(1)*i_kind_i1 + 
                    start(1)*i_warm_i1 + start(1)*i_generous_i1 + 
                    start(1)*i_fair_i1 + start(1)*i_humble_i1 +
                    start(1)*i_cooperative_i1 + start(1)*i_patient_i1 +
                    start(1)*i_honest_i1 + start(1)*i_trustworty_i1 + 
                    start(1)*i_loyal_i1 
                     
                    
          # agreement ----------------------------------------------------------
          s_warmth ~~ i_warmth
          s_moral ~~ i_moral
          s_ability ~~ i_ability
          
          
          # bias factors independent of main factors
          i_bias~~0*i_ability
          i_bias~~0*s_ability
          i_bias~~0*i_warmth
          i_bias~~0*s_warmth
          i_bias~~0*i_moral
          i_bias~~0*s_moral
          
          s_bias~~0*s_ability
          s_bias~~0*i_ability
          s_bias~~0*s_warmth
          s_bias~~0*i_warmth
          s_bias~~0*s_moral
          s_bias~~0*i_moral
          
          # factors unrelated except for agreement
          i_ability ~~ 0*i_warmth
          i_ability ~~ 0*i_moral
          i_ability ~~ 0*s_warmth
          i_ability ~~ 0*s_moral
          
          i_warmth ~~ 0*i_moral
          i_warmth ~~ 0*s_moral
          i_warmth ~~ 0*s_ability
          
          i_moral ~~ 0*s_warmth
          i_moral ~~ 0*s_ability
          
          s_moral ~~ 0*s_ability
          s_moral ~~ 0*s_warmth
          s_warmth ~~ 0*s_ability
          '


si_bi.fit = cfa(si_bi, data, missing='fiml')
summary(si_bi.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(si_bi.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("i_bias","s_bias"), rotation = 3)


lavInspect(si_bi.fit, what="est")$psi
lavInspect(si_bi.fit, what="std.all")$psi


# get CIs for std correlations
standardizedSolution(si_bi.fit)












