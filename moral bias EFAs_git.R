rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/moral bias")


# libraries --------------------------------------------------------------------
library(dplyr)   # for data cleaning & reshaping etc.
library(psych)   # for parallel analysis
library(ggplot2) # for plots
library(sjPlot)


# data -------------------------------------------------------------------------
data <- read.csv("impressiondata_moralbias.csv")  
data = rename(data, i_trustworthy_avg = i_trustworty_avg)
# nb. informants are aggregated (items formatted i_[trait]_avg)


# splitting up self and other items for ease 
sp_items = data %>% select(starts_with("SP_"))
i_items = data %>% select(ends_with("avg"))


# subset to the items we're currently using to conduct EFAs on current pool
current_sp = sp_items[, -c(6,8:12,17,23:26)]
current_i = i_items[, -c(1:2,8,13:16)]

current_items = cbind(current_sp, current_i)


# ------------------------------------------------------------------------------
# EFA with current items, within each perspective
# ------------------------------------------------------------------------------
## self ------------------------------------------------------------------------
# basic correlations
cs = cor(current_sp, use = "pairwise.complete.obs")
corrplot::corrplot(cs, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=cs, fa="both")  # suggests 3 factors
# can also use scree(current_sp) for just the scree plot

# factor analysis
fa_s = fa(r = cs, nfactors = 3, 
          rotate = "oblimin", 
          fm = "pa")

fa.diagram(fa_s)  # breaks up roughly how you'd expect


## informant -------------------------------------------------------------------
colnames(current_i) = gsub("i_", "", colnames(current_i))
colnames(current_i) = gsub("_avg", "", colnames(current_i))
  
# basic correlations
ci = cor(current_i, use = "pairwise.complete.obs")
corrplot::corrplot(ci, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 
  
  
# parallel analysis
fa.parallel(x=ci, fa="both")  # suggests 3 factors

  
# factor analysis
fa_i = fa(r = ci, nfactors = 4, 
            rotate = "oblimin", 
            fm = "pa")

fa.diagram(fa_i)  # bit different than self


# ------------------------------------------------------------------------------
# EFA with current items, across perspectives
# ------------------------------------------------------------------------------
colnames(current_items) = gsub("SP_", "s_", colnames(current_items))
colnames(current_items) = gsub("_avg", "", colnames(current_items))


# basic correlations
cc = cor(current_items, use = "pairwise.complete.obs")
corrplot::corrplot(cc, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=cc, fa="both")  # suggests 3 factors


# factor analysis
fa_c = fa(r = cc, nfactors = 6, 
          rotate = "oblimin", 
          fm = "pa")

fa.diagram(fa_c)



# ------------------------------------------------------------------------------
# EFA with all possible items
# ------------------------------------------------------------------------------
## self ------------------------------------------------------------------------
colnames(sp_items) = gsub("SP_", "", colnames(sp_items))

sp_items = sp_items[,-26] # rm self-esteem


# basic correlations
cs_all = cor(sp_items, use = "pairwise.complete.obs")
corrplot::corrplot(cs_all, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=cs_all, fa="both")  # suggests 3 factors


# factor analysis
fa_sall = fa(r = cs_all, nfactors = 4, 
          rotate = "oblimin", 
          fm = "pa")

fa.diagram(fa_sall)  # breaks up roughly how you'd expect



## informant -------------------------------------------------------------------
colnames(i_items) = gsub("_avg", "", colnames(i_items))
i_items = i_items[,-1] # rm liking


# basic correlations
ci_all = cor(i_items, use = "pairwise.complete.obs")
corrplot::corrplot(ci_all, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=ci_all, fa="both")  # suggests 3 factors


# factor analysis
fa_iall = fa(r = ci_all, nfactors = 3, 
             rotate = "oblimin", 
             fm = "pa")

fa.diagram(fa_iall)  




# ------------------------------------------------------------------------------
# EFA but rm disorganized and self-disc
# ------------------------------------------------------------------------------
## self ------------------------------------------------------------------------
sp_items = select(sp_items, -c(disorganized, selfdiciplined))  # rm the 2 items

# basic correlations
cs2 = cor(sp_items, use = "pairwise.complete.obs")
corrplot::corrplot(cs2, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=cs2, fa="both")  


# factor analysis
fa_s2 = fa(r = cs2, nfactors = 4, 
             rotate = "oblimin", 
             fm = "pa")

fa.diagram(fa_s2)  


## informant -------------------------------------------------------------------
i_items = select(i_items, -c(i_disorganized, i_selfdiciplined))  # rm the 2 items

# basic correlations
ci2 = cor(i_items, use = "pairwise.complete.obs")
corrplot::corrplot(ci2, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=ci2, fa="both")  # suggests 1 factor


# factor analysis
fa_ci2 = fa(r = ci2, nfactors = 4, 
           rotate = "oblimin", 
           fm = "pa")

fa.diagram(fa_ci2)  




# ------------------------------------------------------------------------------
# EFA - moral items only
# ------------------------------------------------------------------------------
spmoral = 
sp_items %>% select(c(SP_compassionate, SP_kind, SP_warm, SP_generous, SP_fair,
                      SP_humble, SP_cooperative, SP_grateful, SP_patient, SP_loyal,
                      SP_honest, SP_trustworthy))

colnames(spmoral) = gsub("SP_", "", colnames(spmoral))


# basic correlations
cm = cor(spmoral, use = "pairwise.complete.obs")
corrplot::corrplot(cm, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=cm, fa="both")  # suggests 1 factor


# factor analysis
fa_m = fa(r = cm, nfactors = 2, 
            rotate = "oblimin", 
            fm = "pa")

fa.diagram(fa_m)  



# ------------------------------------------------------------------------------
# EFA - ability items only
# ------------------------------------------------------------------------------
spability = 
  sp_items %>% select(c(SP_socially.skilled, SP_funny, SP_intelligent, SP_creative))

colnames(spability) = gsub("SP_", "", colnames(spability))

# basic correlations
ca = cor(spability, use = "pairwise.complete.obs")
corrplot::corrplot(ca, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=ca, fa="both")  # suggests 1 factor


# factor analysis
fa_a = fa(r = ca, nfactors = 1, 
            rotate = "oblimin", 
            fm = "pa")

fa.diagram(fa_a)  




# ------------------------------------------------------------------------------
# EFA - above w/m/a items
# ------------------------------------------------------------------------------
spwma = cbind(spmoral, spability)
spwma = spwma %>% select(-creative)

# basic correlations
cwma = cor(spwma, use = "pairwise.complete.obs")
corrplot::corrplot(cwma, order = "original", tl.col='black', tl.cex=.75,
                   tl.srt = 45, method = "color", type="lower",  
                   col = colorRampPalette(c('coral2', 'white', 'darkslategray3'))(10)) 


# parallel analysis
fa.parallel(x=cwma, fa="both")  # suggests 1 factor


# factor analysis
fa_wma = fa(r = cwma, nfactors = 3, 
          rotate = "oblimin", 
          fm = "pa")

fa.diagram(fa_wma)  











