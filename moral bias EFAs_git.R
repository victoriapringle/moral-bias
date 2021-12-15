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
current_i = i_items[, -c(1,7,12:16)]

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
fa_s = fa(r = cs, nfactors = 3, 
          rotate = "oblimin", 
          fm = "pa")

fa.diagram(fa_s)  # breaks up roughly how you'd expect


# first create a df of the loadings on the 3 extracted factors
loadings_s = xtable::xtable(unclass(fa_s$loadings))
loadings_s = tibble::rownames_to_column(loadings_s, "trait")   # make rownames (cue) a col
loadings_s$trait = factor(loadings_s$trait, loadings_s$trait)


# then reshape it to long format for the barplot
loadings_s.m <- reshape2::melt(loadings_s, id="trait", 
                             measure=c("PA1", "PA2", "PA3"), 
                             variable.name="Factor", value.name="Loading")


# then plot the barplot
# this code is lifted from https://rpubs.com/danmirman/plotting_factor_analysis
loadings_s.m$trait = gsub("SP_", "", loadings_s.m$trait)

  ggplot(loadings_s.m, aes(x=reorder(trait,Loading), abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + # place the factors in separate facets
  geom_bar(stat="identity") +    # make the bars
  coord_flip() +                 # flip the axes so the cues are horizontal  
  # define the fill color gradient: blue=pos, red=neg
  scale_fill_gradient2(name = "Loading", 
                       high = "darkslategray3", mid = "white", low = "coral2", 
                       midpoint=0, guide="none") +
  ylab("Loading strength") +     # change y-axis label
  theme_bw(base_size=10) +       # use a b&w theme with set font size
  labs(title = "Factor loadings of all traits for 3 extracted factors",
       caption = "NB. PA1 is correlated with PA3, r = .4")








## informant -------------------------------------------------------------------







# ------------------------------------------------------------------------------
# EFA with current items, across perspectives
# ------------------------------------------------------------------------------










# ------------------------------------------------------------------------------
# EFA with all possible items
# ------------------------------------------------------------------------------




