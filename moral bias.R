rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/moral bias")

# libraries  -------------------------------------------------------------------
library(lavaan)                    # for running the models
library(tidyverse)                 # for renaming
library(semPlot)                   # for plotting the sem graphs

# data -------------------------------------------------------------------------
data <- read.csv("impressiondata_moralbias.csv")  


# ------------------------------------------------------------------------------
# ------------------------------------ self ------------------------------------
# model 1: ability  ------------------------------------------------------------
s_a = '# ability factor
      ability =~ NA*SP_creative + SP_intelligent + SP_socially.skilled
      ability ~~ 1*ability
      '

s_a.fit = cfa(s_a, data, missing='fiml')
summary(s_a.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_a.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# model 2: warmth --------------------------------------------------------------
s_w = '# warmth factor
      warmth =~ NA*SP_happy + SP_warm + SP_funny
      warmth ~~ 1*warmth
      '

s_w.fit = cfa(s_w, data, missing='fiml')
summary(s_w.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_w.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 3: morality ------------------------------------------------------------
s_m = '# moral factor
      moral =~ NA*SP_fair + SP_honest + SP_trustworthy + SP_loyal
      moral ~~ 1*moral
      '

s_m.fit = cfa(s_m, data, missing='fiml')
summary(s_m.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_m.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# model 4: warm-moral ----------------------------------------------------------
s_wm = '# warmth factor
        warmth =~ NA*SP_happy + SP_warm + SP_funny + 
                  SP_humble + SP_kind + SP_grateful + SP_cooperative         # warm-moral items
        warmth ~~ 1*warmth
        
        # moral factor
        moral =~ NA*SP_fair + SP_honest + SP_trustworthy + SP_loyal + 
                  SP_humble + SP_kind + SP_grateful + SP_cooperative         # warm-moral items
        moral ~~ 1*moral
        
        # allow factors to correlate
        moral ~~ warmth
       '

s_wm.fit = cfa(s_wm, data, missing='fiml')
summary(s_wm.fit, fit.measures = TRUE, standardized = TRUE)                  # CFI = .894, RMSEA = .083, SRMR = .056, WxM = .510
semPaths(s_wm.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# model 5: warmth, morality, ability -------------------------------------------
s_wma = '# warmth factor
        warmth =~ NA*SP_happy + SP_warm + SP_funny + 
                  SP_humble + SP_kind + SP_grateful + SP_cooperative         # warm-moral items
        warmth ~~ 1*warmth
        
        # moral factor
        moral =~ NA*SP_fair + SP_honest + SP_trustworthy + SP_loyal + 
                  SP_humble + SP_kind + SP_grateful + SP_cooperative         # warm-moral items
        moral ~~ 1*moral
        
        # ability factor
        ability =~ NA*SP_creative + SP_intelligent + SP_socially.skilled
        ability ~~ 1*ability
        
        # allow factors to correlate
        moral ~~ warmth
        moral ~~ ability
        warmth ~~ ability
        '

s_wma.fit = cfa(s_wma, data, missing='fiml')
summary(s_wma.fit, fit.measures = TRUE, standardized = TRUE)
    # CFI = .825, RMSEA = .090, SRMR = .070
    # WxM = .520, WxA = .520, MxA = .651

semPaths(s_wma.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)

# ------------------------------------------------------------------------------
# --------------------------------- informant ----------------------------------
# model 6: ability  ------------------------------------------------------------ 
i_a = '# ability factor
      i_ability =~ NA*i_creative_avg + i_intelligent_avg + i_socially.skilled_avg
      i_ability ~~ 1*i_ability
      '

i_a.fit = cfa(i_a, data, missing='fiml')
summary(s_a.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_a.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# model 7: warmth  -------------------------------------------------------------
i_w = '# warmth factor
      i_warmth =~ NA*i_happy_avg + i_warm_avg + i_funny_avg
      i_warmth ~~ 1*i_warmth
      '

i_w.fit = cfa(i_w, data, missing='fiml')
summary(i_w.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_w.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# model 8: morality ------------------------------------------------------------
i_m = '# moral factor
      i_moral =~ NA*i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg
      i_moral ~~ 1*i_moral
      '

i_m.fit = cfa(i_m, data, missing='fiml')
summary(i_m.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_m.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# model 9: warm-moral ----------------------------------------------------------
i_wm = '# warmth factor
        i_warmth =~ NA*i_happy_avg + i_warm_avg + i_funny_avg + 
                    i_humble_avg + i_kind_avg  + i_cooperative_avg   # warm-moral items
        i_warmth ~~ 1*i_warmth
        
        # moral factor
        i_moral =~ NA*i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg +
                  i_humble_avg + i_kind_avg  + i_cooperative_avg     # warm-moral items                 
        i_moral ~~ 1*i_moral
        
        # allow factors to correlate
        i_moral ~~ i_warmth
       '

i_wm.fit = cfa(i_wm, data, missing='fiml')
summary(i_wm.fit, fit.measures = TRUE, standardized = TRUE)                  # CFI = .887, RMSEA = .100, SRMR = .056, WxM = .707
semPaths(i_wm.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# model 10: warmth, morality, ability ------------------------------------------
i_wma = '# warmth factor
        i_warmth =~ NA*i_happy_avg + i_warm_avg + i_funny_avg + 
                    i_humble_avg + i_kind_avg  + i_cooperative_avg                   # warm-moral items
        i_warmth ~~ 1*i_warmth
        
        # moral factor
        i_moral =~ NA*i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg +
                  i_humble_avg + i_kind_avg + i_cooperative_avg                      # warm-moral items                 
        i_moral ~~ 1*i_moral
        
        # ability factor
        i_ability =~ NA*i_creative_avg + i_intelligent_avg + i_socially.skilled_avg
        i_ability ~~ 1*i_ability
          
        # allow factors to correlate
        i_moral ~~ i_warmth
        i_moral ~~ i_ability
        i_warmth ~~ i_ability
        '

i_wma.fit = cfa(i_wma, data, missing='fiml')
summary(i_wma.fit, fit.measures = TRUE, standardized = TRUE)
    # CFI = .855, RMSEA = .093, SRMR = .062
    # WxM = .712, WxA = .746, MxA = .686
semPaths(i_wma.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)

# ------------------------------------------------------------------------------
# ----------------------------- self & informant -------------------------------
# model 11: agreement pre-bias -------------------------------------------------
si_wma = '## self  --------------------------------------------------------
          # self warmth factor
          s_warmth =~ NA*SP_happy + SP_warm + SP_funny + 
                    SP_humble + SP_kind + SP_grateful + SP_cooperative         
          s_warmth ~~ 1*s_warmth
          
          # self moral factor
          s_moral =~ NA*SP_fair + SP_honest + SP_trustworthy + SP_loyal + 
                    SP_humble + SP_kind + SP_grateful + SP_cooperative    
          s_moral ~~ 1*s_moral
          
          # self ability factor
          s_ability =~ NA*SP_creative + SP_intelligent + SP_socially.skilled
          s_ability ~~ 1*s_ability
          
          ## informant -----------------------------------------------------
          # informant warmth factor
          i_warmth =~ NA*i_happy_avg + i_warm_avg + i_funny_avg + 
                    i_humble_avg + i_kind_avg + i_cooperative_avg 
          i_warmth ~~ 1*i_warmth
          
          # informant moral factor
          i_moral =~ NA*i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg +
                  i_humble_avg + i_kind_avg + i_cooperative_avg               
          i_moral ~~ 1*i_moral
          
          # informant ability factor
          i_ability =~ NA*i_creative_avg + i_intelligent_avg + i_socially.skilled_avg
          i_ability ~~ 1*i_ability
          
          # agreement ----------------------------------------------------------
          s_warmth ~~ i_warmth
          s_moral ~~ i_moral
          s_ability ~~ i_ability
          '
si_wma.fit = cfa(si_wma, data, missing='fiml')
summary(si_wma.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(si_wma.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 3, sizeLat = 5)


lavInspect(si_wma.fit, what="est")$psi
# low moral agreement pre-bias



# model 15: self-bias bifactor  ------------------------------------------------
s_bi = '# self warmth factor
          s_warmth =~ start(1)*SP_happy + start(1)*SP_warm + start(1)*SP_funny + 
                      start(1)*SP_humble + start(1)*SP_kind + start(1)*SP_grateful + start(1)*SP_cooperative

        # self moral factor
          s_moral =~ start(1)*SP_fair + start(1)*SP_honest + start(1)*SP_trustworthy + start(1)*SP_loyal + 
                     start(1)*SP_humble + start(1)*SP_kind + start(1)*SP_grateful + start(1)*SP_cooperative


        # self ability factor
          s_ability =~ start(1)*SP_creative + start(1)*SP_intelligent +
                       start(1)*SP_selfdiciplined + start(-1)*SP_disorganized
                       
                       
        # bias factor   
          s_bias =~ SP_happy + SP_warm + SP_funny +
                    SP_fair + SP_honest + SP_trustworthy + SP_loyal + 
                    SP_humble + SP_kind + SP_grateful + SP_cooperative +
                    SP_creative + SP_intelligent + SP_selfdiciplined + SP_disorganized
                    
   
        s_moral ~~ 0*s_bias
        s_warmth ~~ 0*s_bias
        s_ability ~~ 0*s_bias

    '

s_bi.fit = cfa(s_bi, data, missing='fiml')
summary(s_bi.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(s_bi.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("s_bias"), rotation = 3)

# CFI = .907, RMSEA = .074, SRMR = .051

# model 16: informant bias bifactor --------------------------------------------
i_bi = '# warmth factor
        i_warmth =~ i_happy_avg + i_warm_avg + i_funny_avg + 
                    i_humble_avg + i_kind_avg  + i_cooperative_avg   

        # moral factor
        i_moral =~ i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg +
                  i_humble_avg + i_kind_avg  + i_cooperative_avg                      

        # ability factor
        i_ability =~ i_creative_avg + i_intelligent_avg + i_socially.skilled_avg

        # bias factor
        i_bias =~ i_happy_avg + i_warm_avg + i_funny_avg + 
                  i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg +
                  i_humble_avg + i_kind_avg  + i_cooperative_avg +
                  i_creative_avg + i_intelligent_avg + i_socially.skilled_avg
                  
        i_moral ~~ 0*i_bias
        i_warmth ~~ 0*i_bias
        i_ability ~~ i_bias
      '


i_bi.fit = cfa(i_bi, data, missing='fiml')
summary(i_bi.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(i_bi.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("i_bias"), rotation = 3)

# CFI = .855, RMSEA = .093, SRMR = .062


# model 17: agreement post-bias BIFACTOR  --------------------------------------
si_bi = '## self -----------------------------------------------------
       # self bias factor 
       s_bias =~ NA*SP_happy + SP_warm + SP_funny + 
                 SP_humble + SP_kind + SP_grateful + SP_cooperative +
                 SP_fair + SP_honest + SP_trustworthy + SP_loyal + 
                 SP_humble + SP_kind + SP_grateful + SP_cooperative +
                 SP_creative + SP_intelligent + SP_socially.skilled
       
       # self warmth factor
       s_warmth =~ NA*SP_happy + SP_warm + SP_funny + 
                   SP_humble + SP_kind + SP_grateful + SP_cooperative         
       s_warmth ~~ 1*s_warmth
          
       # self moral factor
       s_moral =~ NA*SP_fair + SP_honest + SP_trustworthy + SP_loyal + 
                  SP_humble + SP_kind + SP_grateful + SP_cooperative    
       s_moral ~~ 1*s_moral
          
       # self ability factor
       s_ability =~ NA*SP_creative + SP_intelligent + SP_socially.skilled
       s_ability ~~ 1*s_ability
       
       ## informant -----------------------------------------------------
       # warmth factor
        i_warmth =~ i_happy_avg + i_warm_avg + i_funny_avg + 
                    i_humble_avg + i_kind_avg  + i_cooperative_avg   

        # moral factor
        i_moral =~ i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg +
                  i_humble_avg + i_kind_avg  + i_cooperative_avg                      

        # ability factor
        i_ability =~ i_creative_avg + i_intelligent_avg + i_socially.skilled_avg

        # bias factor
        i_bias =~ i_happy_avg + i_warm_avg + i_funny_avg + 
                  i_fair_avg + i_honest_avg + i_trustworty_avg + i_loyal_avg +
                  i_humble_avg + i_kind_avg  + i_cooperative_avg +
                  i_creative_avg + i_intelligent_avg + i_socially.skilled_avg
                  
        i_moral ~~ 0*i_bias
        i_warmth ~~ 0*i_bias
        i_ability ~~ i_bias
      '
si_bi.fit = cfa(si_bi, data, missing='fiml')
summary(si_bi.fit, fit.measures = TRUE, standardized = TRUE)
semPaths(si_bi.fit, "std", intercepts = FALSE, edge.label.cex = .7, 
         style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5, layout="tree2",
         bifactor = c("i_bias","s_bias"), rotation = 3)

# CFI = .898, RMSEA = .051, SRMR = .059
# warmth agreement = .116
# moral agreement = .116
# ability agreement = .132
# conclusion: agreement INCREASES post-bias