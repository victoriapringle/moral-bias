rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/moral bias")


# libraries --------------------------------------------------------------------
library(dplyr)   # for data cleaning & reshaping etc.
library(psych)   # for parallel analysis
library(ggplot2) # for plots


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

current_items = cbind(sp_items, i_items)


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


# factor analysis

fa.diagram(fa_s)  # breaks up roughly how you'd expect

fa_s = fa(r = cs, nfactors = 3, 
          rotate = "oblimin", 
          fm = "pa")


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










# ------------------------------------------------------------------------------
# EFA with all possible items
# ------------------------------------------------------------------------------




