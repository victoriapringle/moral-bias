rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/moral bias")


# libraries  -------------------------------------------------------------------
library(lavaan)                    # for running the models
library(semPlot)                   # for plotting the sem graphs
library(tidyverse)                 # for renaming


# data -------------------------------------------------------------------------
data <- read.csv("impressiondata_moralbias.csv")  
# nb. informants are aggregated (items formatted i_[trait]_avg)

data = 
  data %>% rename(SP_attractive = SP_physically.attractive,
                  i_attractive_avg = i_physically.attractive_avg)

# ------------------------------------------------------------------------------
# ------------------------------------ self ------------------------------------
# model 1: ability  ------------------------------------------------------------
s_a = '# ability factor
       ability =~ start(1)*SP_creative + start(1)*SP_intelligent +
                  start(1)*SP_socially.skilled + start(1)*SP_funny
      '

s_a.fit = cfa(s_a, data, missing='fiml')
summary(s_a.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_a.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)




# model 2: warmth --------------------------------------------------------------
s_w = '# warmth factor
      warmth =~ start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                start(1)*SP_cooperative + start(1)*SP_patient
      '

s_w.fit = cfa(s_w, data, missing='fiml')
summary(s_w.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_w.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)





# model 3: morality ------------------------------------------------------------
s_m = '# moral factor
       moral =~ start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
      '

s_m.fit = cfa(s_m, data, missing='fiml')
summary(s_m.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_m.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)




# model 5: warmth, morality, ability -------------------------------------------
s_wma = '# ability factor
        ability =~ start(1)*SP_creative + start(1)*SP_intelligent +
                  start(1)*SP_socially.skilled + start(1)*SP_funny
        
        
        # warmth factor
        warmth =~ start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                  start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                  start(1)*SP_cooperative + start(1)*SP_patient 
        
        # moral factor
        moral =~ start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
        
                
        # allow factors to correlate
        moral ~~ warmth
        moral ~~ ability
        warmth ~~ ability

        '

s_wma.fit = cfa(s_wma, data, missing='fiml')
summary(s_wma.fit, fit.measures = TRUE, standardized = TRUE)

semPaths(s_wma.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# UNcorrelated warmth, morality, ability ---------------------------------------
s_wma_uc = '# ability factor
        ability =~ start(1)*SP_creative + start(1)*SP_intelligent +
                  start(1)*SP_socially.skilled + start(1)*SP_funny
        
        
        # warmth factor
        warmth =~ start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                  start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                  start(1)*SP_cooperative + start(1)*SP_patient 
        
        # moral factor
        moral =~ start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
        
                
        # allow factors to correlate
        moral ~~ 0*warmth
        moral ~~ 0*ability
        warmth ~~ 0*ability

        '

s_wma_uc.fit = cfa(s_wma_uc, data, missing='fiml')
s_wma_uc.fit %>% summary(fit.measures = TRUE, standardized = TRUE)

semPaths(s_wma_uc.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# compare correlated & uncorrelated factors models
anova(s_wma.fit, s_wma_uc.fit)
abs(BIC(s_wma.fit) - BIC(s_wma_uc.fit))


# positivity-only model --------------------------------------------------------
s_posonly = '# positivity 
        positivity =~ start(1)*SP_creative + start(1)*SP_intelligent +
        start(1)*SP_socially.skilled + start(1)*SP_funny + 
        start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
        start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
        start(1)*SP_cooperative + start(1)*SP_patient +
        start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
        '
s_posonly.fit = cfa(s_posonly, data, missing='fiml')
s_posonly.fit %>% summary(fit.measures = TRUE, standardized = TRUE)
semPaths(s_posonly.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# compare pos-only to correlated-factors model ---------------------------------
# i don't think these are nested because we've added/removed factors, so we
# will compare AIC + BIC

# Compare by AIC
exp(-1/2 * abs(AIC(s_posonly.fit) - AIC(s_wma.fit)))
AIC(s_posonly.fit)
AIC(s_wma.fit)

# Compare BIC
abs(BIC(s_posonly.fit) - BIC (s_wma.fit))
BIC(s_posonly.fit)
BIC(s_wma.fit)



# ------------------------------------------------------------------------------
# --------------------------------- informant ----------------------------------
# model 6: ability  ------------------------------------------------------------ 
i_a = '# ability factor
        i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                   start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 
        '

i_a.fit = cfa(i_a, data, missing='fiml')
summary(i_a.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_a.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 7: warmth  -------------------------------------------------------------
i_w = '# warmth factor
       i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                  start(1)*i_warm_avg + start(1)*i_generous_avg + 
                  start(1)*i_fair_avg + start(1)*i_humble_avg +
                  start(1)*i_cooperative_avg + start(1)*i_patient_avg 
                  
      '

i_w.fit = cfa(i_w, data, missing='fiml')
summary(i_w.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_w.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 8: morality ------------------------------------------------------------
i_m = '# moral factor
      i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + start(1)*i_loyal_avg'

i_m.fit = cfa(i_m, data, missing='fiml')
summary(i_m.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_m.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 10: warmth, morality, ability ------------------------------------------
i_wma = '# ability factor
        i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                   start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 

        # warmth factor
        i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                  start(1)*i_warm_avg + start(1)*i_generous_avg + 
                  start(1)*i_fair_avg + start(1)*i_humble_avg +
                  start(1)*i_cooperative_avg + start(1)*i_patient_avg
                  
        
        # moral factor
        i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
        start(1)*i_loyal_avg

          
        # allow factors to correlate
        i_moral ~~ i_warmth
        i_moral ~~ i_ability
        i_warmth ~~ i_ability
        '

i_wma.fit = cfa(i_wma, data, missing='fiml')
summary(i_wma.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_wma.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# UNcorrelated warmth, morality, ability ---------------------------------------
i_wma_uc = '# ability factor
        i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                   start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 

        # warmth factor
        i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                  start(1)*i_warm_avg + start(1)*i_generous_avg + 
                  start(1)*i_fair_avg + start(1)*i_humble_avg +
                  start(1)*i_cooperative_avg + start(1)*i_patient_avg
                  
        
        # moral factor
        i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
        start(1)*i_loyal_avg

          
        # allow factors to correlate
        i_moral ~~ 0*i_warmth
        i_moral ~~ 0*i_ability
        i_warmth ~~ 0*i_ability
        '

i_wma_uc.fit = cfa(i_wma_uc, data, missing='fiml')
summary(i_wma_uc.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_wma_uc.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# compare correlated- and uncorrelated-factors models --------------------------
anova(i_wma.fit, i_wma_uc.fit)
abs(BIC(i_wma.fit) - BIC(i_wma_uc.fit))


# positivity-only model --------------------------------------------------------
i_posonly = '
        positivity =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                    start(1)*i_socially.skilled_avg + start(1)*i_funny_avg +
                    start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                    start(1)*i_warm_avg + start(1)*i_generous_avg + 
                    start(1)*i_fair_avg + start(1)*i_humble_avg +
                    start(1)*i_cooperative_avg + start(1)*i_patient_avg + 
                    start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
                    start(1)*i_loyal_avg
              '

i_posonly.fit = cfa(i_posonly, data, missing='fiml')
summary(i_posonly.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_posonly.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# compare pos-only to correlated-factors model ---------------------------------
# i don't think these are nested because we've added/removed factors, so we
# will compare AIC + BIC

# Compare by AIC
exp(-1/2 * abs(AIC(i_posonly.fit) - AIC(i_wma.fit)))
AIC(i_posonly.fit)
AIC(i_wma.fit)

# Compare BIC
abs(BIC(i_posonly.fit) - BIC (i_wma.fit))
BIC(i_posonly.fit)
BIC(i_wma.fit)



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
          i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                     start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 
  
          # warmth factor
          i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                    start(1)*i_warm_avg + start(1)*i_generous_avg + 
                    start(1)*i_fair_avg + start(1)*i_humble_avg +
                    start(1)*i_cooperative_avg + start(1)*i_patient_avg
                    
          # moral factor
          i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
          start(1)*i_loyal_avg
          
          
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


# model 15: self-bias bifactor  ------------------------------------------------
s_bi = "# ability factor
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
          
          SP_compassionate ~~ 0*SP_compassionate
          
          
          # bias factor
          s_bias =~ start(1)*SP_creative + start(1)*SP_intelligent +
                      start(1)*SP_socially.skilled + start(1)*SP_funny +
                      start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                      start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                      start(1)*SP_cooperative +
                      start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal + start(1)*SP_patient
          
          s_bias ~~ 0*s_moral
          s_bias ~~ 0*s_warmth
          s_bias ~~ 0*s_ability
          
          s_moral ~~ 0*s_warmth
          s_moral ~~ 0*s_ability
          s_ability ~~ 0*s_warmth
          "

        

s_bi.fit = cfa(s_bi, data, missing='fiml')
summary(s_bi.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_bi.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("s_bias"), rotation = 3)

lavInspect(s_bi.fit, what="est")$psi
lavInspect(s_bi.fit, what="std")$psi


# compare bifactor to uncorrelated-factors model -------------------------------
anova(s_bi.fit, s_wma_uc.fit)

# Compare BIC
abs(BIC(s_wma_uc.fit) - BIC(s_bi.fit))
BIC(s_bi.fit)
BIC(s_wma_uc.fit)



# model 16: informant bias bifactor --------------------------------------------
i_bi = '# ability factor
          i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                     start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 
  
          # warmth factor
          i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                    start(1)*i_warm_avg + start(1)*i_generous_avg + 
                    start(1)*i_fair_avg + start(1)*i_humble_avg +
                    start(1)*i_cooperative_avg + start(1)*i_patient_avg
                    
          # compassionate residual fixed to 0 
          i_compassionate_avg ~~ 0*i_compassionate_avg
          
          # moral factor
          i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
          start(1)*i_loyal_avg

                     
        # bias factor
        i_bias =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                  start(1)*i_socially.skilled_avg + start(1)*i_funny_avg +
                  start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                  start(1)*i_warm_avg + start(1)*i_generous_avg + 
                  start(1)*i_fair_avg + start(1)*i_humble_avg +
                  start(1)*i_cooperative_avg + start(1)*i_patient_avg + 
                  start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
                  start(1)*i_loyal_avg 
                      
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


# compare bifactor to uncorrelated-factors model -------------------------------
anova(i_bi.fit, i_wma_uc.fit)

# Compare BIC
abs(BIC(i_wma_uc.fit) - BIC(i_bi.fit))
BIC(i_bi.fit)
BIC(i_wma_uc.fit)

anova(i_bi.fit, i_wma_uc.fit)


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
          i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                     start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 
                     
          # socially skilled residual fixed to 0
          i_socially.skilled_avg ~~ 0*i_socially.skilled_avg
  
          # warmth factor
          i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                    start(1)*i_warm_avg + start(1)*i_generous_avg + 
                    start(1)*i_fair_avg + start(1)*i_humble_avg +
                    start(1)*i_cooperative_avg + start(1)*i_patient_avg
                    
          # compassionate residual fixed to 0
          i_compassionate_avg ~~ 0*i_compassionate_avg
          
          # moral factor
          i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
                    start(1)*i_loyal_avg
          
                     
          # bias factor
          i_bias =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                    start(1)*i_socially.skilled_avg + start(1)*i_funny_avg +
                    start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                    start(1)*i_warm_avg + start(1)*i_generous_avg + 
                    start(1)*i_fair_avg + start(1)*i_humble_avg +
                    start(1)*i_cooperative_avg + start(1)*i_patient_avg +
                    start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
                    start(1)*i_loyal_avg 
                     
                    
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


# ------------------------------------------------------------------------------
# -------------------------------- extras --------------------------------------
# is bias related to self-esteem? ----------------------------------------------
se_bias = "# ability factor
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
          
          SP_compassionate ~~ 0*SP_compassionate
          
          
          # bias factor
          s_bias =~ start(1)*SP_creative + start(1)*SP_intelligent +
                      start(1)*SP_socially.skilled + start(1)*SP_funny +
                      start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                      start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                      start(1)*SP_cooperative +
                      start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal + start(1)*SP_patient
          
           
                      
          # but main factors independent of bias
          s_bias~~0*s_ability
          s_bias~~0*s_warmth
          s_bias~~0*s_moral

          # self-esteem
          se =~ NA*SP_selfesteem
          se ~~ 1*se
          
          se ~~ s_bias
          se ~~ 0*s_moral
          se ~~ 0*s_warmth
          se ~~ 0*s_ability
          
          s_moral ~~ 0*s_warmth
          s_moral ~~ 0*s_ability
          s_ability ~~ 0*s_warmth
          "


se_bias.fit = cfa(se_bias, data, missing='fiml')
summary(se_bias.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(se_bias.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("se","s_bias"), rotation = 3)



# get CIs for std correlations
standardizedSolution(se_bias.fit)




# is bias related to liking? ---------------------------------------------------
like_bias = "# ability factor
              i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                         start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 
      
              # warmth factor
              i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                        start(1)*i_warm_avg + start(1)*i_generous_avg + 
                        start(1)*i_fair_avg + start(1)*i_humble_avg +
                        start(1)*i_cooperative_avg + start(1)*i_patient_avg
                        
              # compassionate residual fixed to 0 
              i_compassionate_avg ~~ 0*i_compassionate_avg
              
              # moral factor
              i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
              start(1)*i_loyal_avg
      
                           
              # bias factor
              i_bias =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                        start(1)*i_socially.skilled_avg + start(1)*i_funny_avg +
                        start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                        start(1)*i_warm_avg + start(1)*i_generous_avg + 
                        start(1)*i_fair_avg + start(1)*i_humble_avg +
                        start(1)*i_cooperative_avg + start(1)*i_patient_avg + 
                        start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
                        start(1)*i_loyal_avg 
                            
              # main factors independent of bias
              i_bias~~0*i_ability
              i_bias~~0*i_warmth
              i_bias~~0*i_moral
              
              
              i_moral ~~ 0*i_warmth
              i_moral ~~ 0*i_ability
              i_ability ~~ 0*i_warmth
              
              # liking
              liking =~ NA*i_like_avg
              liking ~~ 1*liking
              
              liking ~~ i_bias
              liking ~~ 0*i_moral
              liking ~~ 0*i_warmth
              liking ~~ 0*i_ability
          "

like_bias.fit = cfa(like_bias, data, missing='fiml')
summary(like_bias.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(like_bias.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("liking","i_bias"), rotation = 3)

# get CIs for std correlations
standardizedSolution(like_bias.fit)




# compare WMA models pre-/post-positivity --------------------------------------
# self  ------------------------------------------------------------------------
s_wma2 = '# ability factor
        ability =~ start(1)*SP_creative + start(1)*SP_intelligent +
                  start(1)*SP_socially.skilled + start(1)*SP_funny
        
        
        # warmth factor
        warmth =~ start(1)*SP_compassionate + start(1)*SP_kind + start(1)*SP_warm +
                  start(1)*SP_generous + start(1)*SP_fair + start(1)*SP_humble +
                  start(1)*SP_cooperative + start(1)*SP_patient 
        
        # moral factor
        moral =~ start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal
        
                
        # allow factors to correlate
        moral ~~ 0*warmth
        moral ~~ 0*ability
        warmth ~~ 0*ability

        '

s_wma2.fit = cfa(s_wma2, data, missing='fiml')
summary(s_wma2.fit, fit.measures = TRUE, standardized = TRUE)

semPaths(s_wma2.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# anova to compare bc they're nested
anova(s_wma.fit, s_bi.fit)

fitmeasures(s_wma2.fit)
fitmeasures(s_bi.fit)


# informant  -------------------------------------------------------------------
i_wma2 = '# ability factor
        i_ability =~ start(1)*i_creative_avg + start(1)*i_intelligent_avg +
                   start(1)*i_socially.skilled_avg + start(1)*i_funny_avg 

        # warmth factor
        i_warmth =~ start(1)*i_compassionate_avg + start(1)*i_kind_avg + 
                  start(1)*i_warm_avg + start(1)*i_generous_avg + 
                  start(1)*i_fair_avg + start(1)*i_humble_avg +
                  start(1)*i_cooperative_avg + start(1)*i_patient_avg
                  
        
        # moral factor
        i_moral =~ start(1)*i_honest_avg + start(1)*i_trustworty_avg + 
        start(1)*i_loyal_avg

          
        # allow factors to correlate
        i_moral ~~ 0*i_warmth
        i_moral ~~ 0*i_ability
        i_warmth ~~ 0*i_ability
        '

i_wma2.fit = cfa(i_wma2, data, missing='fiml')
summary(i_wma2.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_wma2.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# anova to compare 
anova(i_wma.fit, i_bi.fit)

fitmeasures(i_wma2.fit)
fitmeasures(i_bi.fit)



# are positivity loadings correlated with desirability ratings? ----------------
desirability = read.csv("clean social desirability ratings.csv")

avg_desirability = 
  as.data.frame(colMeans(desirability[,2:23], na.rm = T))

avg_desirability = 
  avg_desirability %>% 
  rownames_to_column(.) %>%
  rename(avg_desirability = `colMeans(desirability[, 2:23], na.rm = T)`,
         trait = rowname) 


# extract positivity loadings for self-model
self_pos_loadings = inspect(s_bi.fit,what="std")$lambda[,4]
inf_pos_loadings = inspect(i_bi.fit,what="std")$lambda[,4]


# are the loadings correlated with the item's evaluativeness?
cor.test(self_pos_loadings, avg_desirability$avg_desirability[1:15])
cor.test(inf_pos_loadings, avg_desirability$avg_desirability[1:15])

std.prmts = standardizedSolution(s_bi.fit)

item = std.prmts[1:15,3]
store_lambda = data.frame(loading = rep(NA, 15))

for (i in 1:15){
  lambda_pos = std.prmts[std.prmts$lhs=="s_bias" & std.prmts$rhs==item[i],"est.std"]
  
  store_lambda[i,] = lambda_pos
} # close loop






