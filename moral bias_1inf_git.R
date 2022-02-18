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

















