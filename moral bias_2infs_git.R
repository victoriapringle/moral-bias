rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/GitHub/moral-bias")


# libraries  -------------------------------------------------------------------
library(lavaan)                    # for running the models
library(semPlot)                   # for plotting the sem graphs
library(tidyverse)                 # for renaming


# data -------------------------------------------------------------------------
data <- read_csv("C:/Users/victo/OneDrive/Documents/moral bias/impressiondata_moralbias.csv") %>%
  rename(SP_attractive = SP_physically.attractive,
         i_attractive_i1 = i_physically.attractive_i1)


# ------------------------------------------------------------------------------
# ----------------------- informant measurement models -------------------------
# ------------------------------------------------------------------------------

# ability ----------------------------------------------------------------------
i_a = '# ability factor
        i1_ability =~ 
        a*i_creative_i1 + 
        b*i_intelligent_i1 +
        c*i_socially.skilled_i1 + 
        d*i_funny_i1 
                   
        i2_ability =~ 
        a*i_creative_i2 +
        b*i_intelligent_i2 +
        c*i_socially.skilled_i2 + 
        d*i_funny_i2 
        
        '

i_a.fit = cfa(i_a, data, missing='fiml')

i_a.fit %>% summary(., fit.measures = TRUE, standardized = TRUE) 

i_a.fit %>% semPaths(., "std", intercepts = FALSE, edge.label.cex = .7, 
                     style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)




# warmth -----------------------------------------------------------------------
i_w = '# warmth factor
       i1_warmth =~ 
       e*i_compassionate_i1 + 
       f*i_kind_i1 + 
       g*i_warm_i1 +
       h*i_generous_i1 + 
       i*i_cooperative_i1 

      i2_warmth =~
      e*i_compassionate_i2 + 
      f*i_kind_i2 + 
      g*i_warm_i2 +
      h*i_generous_i2 + 
      i*i_cooperative_i2 
'

i_w.fit = cfa(i_w, data, missing='fiml')

i_w.fit %>% summary(., fit.measures = TRUE, standardized = TRUE) 

i_w.fit %>% semPaths(., "std", intercepts = FALSE, edge.label.cex = .7, 
                     style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)


# morality ---------------------------------------------------------------------
i_m = '# moral factor
      i1_moral =~ 
      j*i_honest_i1 + 
      k*i_trustworty_i1 + 
      l*i_loyal_i1 +
      m*i_fair_i1
      
      i2_moral =~ 
      j*i_honest_i2 + 
      k*i_trustworty_i2 + 
      l*i_loyal_i2 +
      m*i_fair_i2
      '

i_m.fit = cfa(i_m, data, missing='fiml')

i_m.fit %>% summary(., fit.measures = TRUE, standardized = TRUE) 

i_m.fit %>% semPaths(., "std", intercepts = FALSE, edge.label.cex = .7, 
                     style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# WMA  -------------------------------------------------------------------------
i_wma = "# ability factor
          i1_ability =~ 
          a*i_creative_i1 + 
          b*i_intelligent_i1 +
          c*i_socially.skilled_i1 + 
          d*i_funny_i1 
                     
          i2_ability =~ 
          a*i_creative_i2 +
          b*i_intelligent_i2 +
          c*i_socially.skilled_i2 + 
          d*i_funny_i2 
          
         # warmth factor
         i1_warmth =~ 
         e*i_compassionate_i1 + 
         f*i_kind_i1 + 
         g*i_warm_i1 +
         h*i_generous_i1 + 
         i*i_cooperative_i1 

         i2_warmth =~
         e*i_compassionate_i2 + 
         f*i_kind_i2 + 
         g*i_warm_i2 +
         h*i_generous_i2 + 
         i*i_cooperative_i2 
        
         # moral factor
         i1_moral =~ 
         m*i_honest_i1 + 
         n*i_trustworty_i1 + 
         o*i_loyal_i1
         
         i2_moral =~ 
         m*i_honest_i2 +
         n*i_trustworty_i2 + 
         o*i_loyal_i2
         "

i_wma.fit = cfa(i_wma, data, missing='fiml')

i_wma.fit %>% summary(., fit.measures = TRUE, standardized = TRUE) 

i_wma.fit %>% semPaths(., "std", intercepts = FALSE, edge.label.cex = .7, 
                       style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)



# ------------------------------------------------------------------------------
# ----------------------------- positivity -------------------------------------
# ------------------------------------------------------------------------------
i_bi = "# ability factor
          i1_ability =~ 
          a*i_creative_i1 + 
          b*i_intelligent_i1 +
          c*i_socially.skilled_i1 + 
          d*i_funny_i1 
                     
          i2_ability =~ 
          a*i_creative_i2 +
          b*i_intelligent_i2 +
          c*i_socially.skilled_i2 + 
          d*i_funny_i2 
          
          
         # warmth factor
         i1_warmth =~ 
         e*i_compassionate_i1 + 
         f*i_kind_i1 + 
         g*i_warm_i1 +
         h*i_generous_i1 + 
         i*i_fair_i1 + 
         j*i_humble_i1 +
         k*i_cooperative_i1 +
         l*i_patient_i1
                    
        i2_warmth =~
        e*i_compassionate_i2 + 
        f*i_kind_i2 + 
        g*i_warm_i2 + 
        h*i_generous_i2 + 
        i*i_fair_i2 + 
        j*i_humble_i2 +
        k*i_cooperative_i2 + 
        l*i_patient_i2
        
       # moral factor
        i1_moral =~ 
        m*i_honest_i1 + 
        n*i_trustworty_i1 + 
        o*i_loyal_i1
        
        i2_moral =~ 
        m*i_honest_i2 +
        n*i_trustworty_i2 + 
        o*i_loyal_i2
        
        # positivity factors
        i1_bias =~ start(1)*i_creative_i1 + 
        start(1)*i_intelligent_i1 +
        start(1)*i_socially.skilled_i1 +
        start(1)*i_funny_i1 +
        start(1)*i_compassionate_i1 + 
        start(1)*i_kind_i1 + 
        start(1)*i_warm_i1 + 
        start(1)*i_generous_i1 + 
        start(1)*i_fair_i1 + 
        start(1)*i_humble_i1 +
        start(1)*i_cooperative_i1 + 
        start(1)*i_patient_i1 + 
        start(1)*i_honest_i1 +
        start(1)*i_trustworty_i1 + 
        start(1)*i_loyal_i1 +
        start(1)*i_like_i1
                    
        i2_bias =~ start(1)*i_creative_i2 + 
        start(1)*i_intelligent_i2 +
        start(1)*i_socially.skilled_i2 + 
        start(1)*i_funny_i2 +
        start(1)*i_compassionate_i2 + 
        start(1)*i_kind_i2 + 
        start(1)*i_warm_i2 + 
        start(1)*i_generous_i2 + 
        start(1)*i_fair_i2 + 
        start(1)*i_humble_i2 +
        start(1)*i_cooperative_i2 +
        start(1)*i_patient_i2 + 
        start(1)*i_honest_i2 + 
        start(1)*i_trustworty_i2 + 
        start(1)*i_loyal_i2 +
        start(1)*i_like_i2
                      
        # main factors independent of bias
        i1_bias~~0*i1_ability
        i1_bias~~0*i1_warmth
        i1_bias~~0*i1_moral
        i1_bias~~0*i2_ability
        i1_bias~~0*i2_warmth
        i1_bias~~0*i2_moral
          
        i2_bias~~0*i2_ability
        i2_bias~~0*i2_warmth
        i2_bias~~0*i2_moral
        i2_bias~~0*i1_ability
        i2_bias~~0*i1_warmth
        i2_bias~~0*i1_moral
      "

i_bi.fit = cfa(i_bi, data, missing='fiml')

i_bi.fit %>% summary(., fit.measures = TRUE, standardized = TRUE) 

i_bi.fit %>% semPaths(., "std", intercepts = FALSE,
                      edge.label.cex = .7, 
                      style = 'lisrel', fade=F, 
                      sizeMan = 5, sizeLat = 5,
                      layout="tree2",
                      bifactor = c("i1_bias", "i2_bias"), 
                      rotation = 3)




# ------------------------------------------------------------------------------
# ----------------------------- agreement --------------------------------------
# ------------------------------------------------------------------------------
# pre-positivity agreement -----------------------------------------------------
si_wma = "
         ## self ---------------------------------------------------------------
         # ability factor
          s_ability =~ 
          start(1)*SP_creative + 
          start(1)*SP_intelligent +
          start(1)*SP_socially.skilled + 
          start(1)*SP_funny
          
          # warmth factor
          s_warmth =~ 
          start(1)*SP_compassionate + 
          start(1)*SP_kind + 
          start(1)*SP_warm + 
          start(1)*SP_generous + 
          start(1)*SP_fair + 
          start(1)*SP_humble + 
          start(1)*SP_cooperative + 
          start(1)*SP_patient
          
          # moral factor
          s_moral =~ 
          start(1)*SP_honest + 
          start(1)*SP_trustworthy +
          start(1)*SP_loyal
          
          ## informant -----------------------------------------------------
          # ability factor
          i1_ability =~ 
          a*i_creative_i1 + 
          b*i_intelligent_i1 +
          c*i_socially.skilled_i1 + 
          d*i_funny_i1 
                     
          i2_ability =~ 
          a*i_creative_i2 +
          b*i_intelligent_i2 +
          c*i_socially.skilled_i2 + 
          d*i_funny_i2 
          
          # warmth factor
          i1_warmth =~ 
          e*i_compassionate_i1 + 
          f*i_kind_i1 + 
          g*i_warm_i1 +
          h*i_generous_i1 + 
          i*i_fair_i1 + 
          j*i_humble_i1 +
          k*i_cooperative_i1 +
          l*i_patient_i1
                    
          i2_warmth =~
          e*i_compassionate_i2 + 
          f*i_kind_i2 + 
          g*i_warm_i2 + 
          h*i_generous_i2 + 
          i*i_fair_i2 + 
          j*i_humble_i2 +
          k*i_cooperative_i2 + 
          l*i_patient_i2
          
          # moral factor
          i1_moral =~ 
          m*i_honest_i1 + 
          n*i_trustworty_i1 + 
          o*i_loyal_i1
          
          i2_moral =~ 
          m*i_honest_i2 +
          n*i_trustworty_i2 + 
          o*i_loyal_i2
    
          # agreement ----------------------------------------------------------
          s_warmth ~~ a1*i1_warmth
          s_moral ~~ b1*i1_moral
          s_ability ~~ c1*i1_ability
          s_warmth ~~ a1*i2_warmth
          s_moral ~~ b1*i2_moral
          s_ability ~~ c1*i2_ability
      "

si_wma.fit = cfa(si_wma, data, missing='fiml')

si_wma.fit %>% summary(., fit.measures = TRUE, standardized = TRUE) 

si_wma.fit %>% semPaths(., "std", intercepts = FALSE, edge.label.cex = .7, 
                        style = 'lisrel', fade=F, sizeMan = 5, sizeLat = 5)

lavInspect(si_wma.fit, what="std.all")$psi



# post-positivity agreement -----------------------------------------------------
si_bi = "
          ## self ---------------------------------------------------------------
          # ability factor
          s_ability =~ 
          start(1)*SP_creative + 
          start(1)*SP_intelligent +
          start(1)*SP_socially.skilled + 
          start(1)*SP_funny
          
          # warmth factor
          s_warmth =~ 
          start(1)*SP_compassionate + 
          start(1)*SP_kind + 
          start(1)*SP_warm + 
          start(1)*SP_generous + 
          start(1)*SP_fair + 
          start(1)*SP_humble + 
          start(1)*SP_cooperative + 
          start(1)*SP_patient
          
          # moral factor
          s_moral =~ 
          start(1)*SP_honest + 
          start(1)*SP_trustworthy +
          start(1)*SP_loyal
          
          # loyal residual fixed to 0
          SP_loyal ~~ 0*SP_loyal
          
          # positivity factor
          s_bias =~ start(1)*SP_creative +
          start(1)*SP_intelligent +
          start(1)*SP_socially.skilled +
          start(1)*SP_funny +
          start(1)*SP_compassionate + 
          start(1)*SP_kind + 
          start(1)*SP_warm +
          start(1)*SP_generous +
          start(1)*SP_fair +
          start(1)*SP_humble +
          start(1)*SP_cooperative + 
          start(1)*SP_patient +
          start(1)*SP_honest + 
          start(1)*SP_trustworthy + 
          start(1)*SP_loyal
          
          
          ## informant -----------------------------------------------------
          # ability factor
          i1_ability =~ 
          a*i_creative_i1 + 
          b*i_intelligent_i1 +
          c*i_socially.skilled_i1 + 
          d*i_funny_i1 
                     
          i2_ability =~ 
          a*i_creative_i2 +
          b*i_intelligent_i2 +
          c*i_socially.skilled_i2 + 
          d*i_funny_i2 
          
          i_socially.skilled_i1 ~~ 0*i_socially.skilled_i1
          i_socially.skilled_i2 ~~ 0*i_socially.skilled_i2
          
          # warmth factor
          i1_warmth =~ 
          e*i_compassionate_i1 + 
          f*i_kind_i1 + 
          g*i_warm_i1 +
          h*i_generous_i1 + 
          i*i_fair_i1 + 
          j*i_humble_i1 +
          k*i_cooperative_i1 +
          l*i_patient_i1
                    
          i2_warmth =~
          e*i_compassionate_i2 + 
          f*i_kind_i2 + 
          g*i_warm_i2 + 
          h*i_generous_i2 + 
          i*i_fair_i2 + 
          j*i_humble_i2 +
          k*i_cooperative_i2 + 
          l*i_patient_i2
          
          # moral factor
          i1_moral =~ 
          m*i_honest_i1 + 
          n*i_trustworty_i1 + 
          o*i_loyal_i1
          
          i2_moral =~ 
          m*i_honest_i2 +
          n*i_trustworty_i2 + 
          o*i_loyal_i2
          
          # positivity factors
          i1_bias =~ start(1)*i_creative_i1 + 
          start(1)*i_intelligent_i1 +
          start(1)*i_socially.skilled_i1 +
          start(1)*i_funny_i1 +
          start(1)*i_compassionate_i1 + 
          start(1)*i_kind_i1 + 
          start(1)*i_warm_i1 + 
          start(1)*i_generous_i1 + 
          start(1)*i_fair_i1 + 
          start(1)*i_humble_i1 +
          start(1)*i_cooperative_i1 + 
          start(1)*i_patient_i1 + 
          start(1)*i_honest_i1 +
          start(1)*i_trustworty_i1 + 
          start(1)*i_loyal_i1 +
          start(1)*i_like_i1
                      
          i2_bias =~ start(1)*i_creative_i2 + 
          start(1)*i_intelligent_i2 +
          start(1)*i_socially.skilled_i2 + 
          start(1)*i_funny_i2 +
          start(1)*i_compassionate_i2 + 
          start(1)*i_kind_i2 + 
          start(1)*i_warm_i2 + 
          start(1)*i_generous_i2 + 
          start(1)*i_fair_i2 + 
          start(1)*i_humble_i2 +
          start(1)*i_cooperative_i2 +
          start(1)*i_patient_i2 + 
          start(1)*i_honest_i2 + 
          start(1)*i_trustworty_i2 + 
          start(1)*i_loyal_i2 +
          start(1)*i_like_i2
          
          
          # agreement ----------------------------------------------------------
          s_warmth ~~ a1*i1_warmth
          s_moral ~~ b1*i1_moral
          s_ability ~~ c1*i1_ability
          
          s_warmth ~~ a1*i2_warmth
          s_moral ~~ b1*i2_moral
          s_ability ~~ c1*i2_ability
          
          s_bias ~~ ps*i1_bias
          s_bias ~~ ps*i2_bias
          
          # positivity factors independent of main factors
          i2_bias~~0*i2_ability
          i2_bias~~0*i2_warmth
          i2_bias~~0*i2_moral
          i2_bias~~0*i1_ability
          i2_bias~~0*i1_warmth
          i2_bias~~0*i1_moral
          
          i1_bias~~0*i1_ability
          i1_bias~~0*i2_ability
          i1_bias~~0*s_ability
          
          i1_bias~~0*i1_warmth
          i1_bias~~0*i2_warmth
          i1_bias~~0*s_warmth
          
          i1_bias~~0*i1_moral
          i1_bias~~0*i2_moral
          i1_bias~~0*s_moral
          
          s_bias~~0*s_ability
          s_bias~~0*i1_ability
          s_bias~~0*i2_ability
          
          
          s_bias~~0*s_warmth
          s_bias~~0*i1_warmth
          s_bias~~0*i2_warmth
          
          s_bias~~0*s_moral
          s_bias~~0*i1_moral
          s_bias~~0*i2_moral
          
          i2_bias~~0*s_ability
          i2_bias~~0*s_warmth
          i2_bias~~0*s_moral
        "

si_bi.fit = cfa(si_bi, data, missing='fiml')

si_bi.fit %>% summary(., fit.measures = TRUE, standardized = TRUE) 

si_bi.fit %>% semPaths(., "std", 
                       intercepts = FALSE, 
                       edge.label.cex = .7, 
                       style = 'lisrel', fade=F, 
                       sizeMan = 5, sizeLat = 5,
                       layout="tree2",
                       bifactor = c("i1_bias","i2_bias","s_bias"),
                       rotation = 3)

lavInspect(si_bi.fit, what="std.all")$psi
























