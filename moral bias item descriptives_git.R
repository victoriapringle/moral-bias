rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/moral bias")

# libraries  -------------------------------------------------------------------
library(dplyr)     # for renaming + reshaping
library(ggplot2)   # for plots
library(tidyr)
library(tayloRswift) # heh, for pretty plots


# data -------------------------------------------------------------------------
data <- read.csv("impressiondata_moralbias.csv")

data = rename(data, i_trustworthy_avg = i_trustworty_avg)


sp_items = data %>% select(starts_with("SP_"))
i_items = data %>% select(ends_with("avg"))

names(i_items) = gsub("_avg", "", names(i_items))

s_wma_items = 
sp_items %>% select(c("SP_warm", "SP_funny", "SP_humble", "SP_kind", "SP_cooperative",
                      "SP_generous", "SP_compassionate", "SP_patient", "SP_fair",
                      "SP_honest", "SP_trustworthy", "SP_loyal", "SP_creative",
                      "SP_intelligent", "SP_socially.skilled"))


i_wma_items =
  i_items %>% select(c("i_warm", "i_funny", "i_humble", "i_kind", "i_cooperative",
                       "i_generous", "i_compassionate", "i_patient", "i_fair",
                       "i_honest", "i_trustworthy", "i_loyal", "i_creative",
                       "i_intelligent", "i_socially.skilled"))

all_wma = cbind(s_wma_items, i_wma_items)
cortable = round(cor(all_wma, use = "na.or.complete"),3)

library(corrr)
cortable = correlate(all_wma)
cortable = cortable %>% mutate(across(where(is.numeric), round, 3)) 


long_cor = 
cortable %>%  
  gather(-term, key = "colname", value = "cor") %>%
  mutate(across(where(is.numeric), round, 3)) 

# informants as cols, self as rows, so diagonal = item-level agreement
small_cortable = 
cortable %>% 
  focus(names(i_wma_items))


write.table(small_cortable, "cortable.txt", row.names = F)

#-------------------------------------------------------------------------------
# ------------------------- SELF ITEMS -----------------------------------------
# descriptives table -----------------------------------------------------------
reshape_sp = reshape2::melt(sp_items)

sp_mean <- aggregate(data = reshape_sp, value ~ variable, mean)
reshape_sp <- merge(reshape_sp, sp_mean, by="variable")
names(reshape_sp) = c("variable", "value", "mean")


sp_descriptives = merge(sp_mean,
                             aggregate(data = reshape_sp, value ~ variable, sd),
                             by="variable")
sp_descriptives <- sp_descriptives %>% 
  mutate(across(where(is.numeric), round, 3)) %>%
  rename(mean = value.x,
         sd = value.y)


# cor matrix -------------------------------------------------------------------
s_cor = cor(sp_items, use="na.or.complete", method = "pearson")                 # create cor matrix

colnames(s_cor) = gsub("SP_", "",colnames(s_cor))                               # remove SP prefix
rownames(s_cor) = gsub("SP_", "",rownames(s_cor))

corrplot::corrplot(s_cor, method="color",type = "lower",  
                   col =colorRampPalette(c("#EFCC98","#F89078","#ED5983","#C0369D"))(10),
                   tl.col="black", tl.srt=45, addCoef.col = "#EDD9A3",cl.ratio = .1)
mtext("Correlations among self-reported traits", at=7.5, line=.5, cex=1.5)



# plot histograms for all ratings ----------------------------------------------
ggplot(reshape_sp) + 
  geom_histogram(aes(x=value, fill = factor(variable), alpha=.1),
                 binwidth = 1, 
                 colour="black") +
  facet_wrap(~variable) + 
  geom_vline(data = reshape_sp, aes(xintercept = mean, col="red")) +
  xlim(0,10) +
  theme(legend.position="none") +
  labs(x = "self-perception", title = "histograms of self-perceptions")


ggplot(reshape_sp, 
       aes(x=value, fill=variable)) +
  geom_histogram(binwidth = 1,colour="black") +
  facet_wrap(~variable) +
  theme(legend.position="none") +
  scale_fill_taylor(palette = "taylorRed") +
  xlim(0,10)


mean(s_cor)
range(s_cor)
sd(s_cor)

#-------------------------------------------------------------------------------
# ---------------------- INFORMANT ITEMS ---------------------------------------
# descriptives table -----------------------------------------------------------
reshape_i = reshape2::melt(i_items)

i_mean <- aggregate(data = reshape_i, value ~ variable, mean)
reshape_i <- merge(reshape_i, i_mean, by="variable")
names(reshape_i) = c("variable", "value", "mean")


i_descriptives = merge(i_mean,
                        aggregate(data = reshape_i, value ~ variable, sd),
                        by="variable")
i_descriptives <- i_descriptives %>% 
  mutate(across(where(is.numeric), round, 3)) %>%
  rename(mean = value.x,
         sd = value.y)


# cor matrix -------------------------------------------------------------------
i_cor = cor(i_items, use="na.or.complete", method = "pearson")                  # create cor matrix

colnames(i_cor) = gsub(c("i_"), "",colnames(i_cor))                             
rownames(i_cor) = gsub("i_", "",rownames(i_cor))
colnames(i_cor) = gsub(c("_avg"), "",colnames(i_cor))                             
rownames(i_cor) = gsub("_avg", "",rownames(i_cor))

corrplot::corrplot(i_cor, method="color",type = "lower",  
                   col =colorRampPalette(c("#EFCC98","#F89078","#ED5983","#C0369D"))(10),
                   tl.col="black", tl.srt=45, addCoef.col = "#EDD9A3",cl.ratio = .1, mar=c(0,0,2,0))
mtext("Correlations among informant-reported traits", at=6, line=.5, cex=1.5)





# plot histograms for all ratings ----------------------------------------------
ggplot(reshape_i) + 
  geom_histogram(aes(x=value, fill = factor(variable), alpha=.1),
                 binwidth = 1, 
                 colour="black") +
  facet_wrap(~variable) + 
  geom_vline(data = reshape_i, aes(xintercept = mean, col="red")) +
  xlim(0,10) +
  theme(legend.position="none") +
  labs(x = "self-perception", title = "histograms of informant-perceptions")


ggplot(reshape_i, 
       aes(x=value, fill=variable)) +
  geom_histogram(binwidth = 1,colour="black") +
  facet_wrap(~variable) +
  theme(legend.position="none") +
  scale_fill_taylor(palette = "taylorRed") +
  xlim(0,10)



mean(i_cor)
range(i_cor)
sd(i_cor)




# cor table --------------------------------------------------------------------
library(modelsummary)  # for the m, sd, cor table

# put the relevant self/informant items in a single df
m = cbind(sp_items, i_items)


# rename them to have similar colnames (s_ = self, i_ = informant)
colnames(m) = gsub("SP_", "s_", colnames(m))
colnames(m) = gsub("_avg", "", colnames(m))


# compute summary table
datasummary(All(m) ~ Mean + SD + Median, data = m)                   # All = use all vars; capitalized Mean and SD = implicitly has na.rm=T


# compute cor table separately & save as df
corr <- datasummary_correlation(m, output = "data.frame")
corr = corr[,-1]                                            # remove col 1 which has the varnames 


# then bind the mean/sd and cor table
datasummary(All(m) ~ Mean + SD + Median, data = m, add_columns = corr, output = 'moralbias_table1.html')


corrplot::corrplot(cor(m,use="na.or.complete", method = "pearson"), type = "lower", method = "color",
                   tl.col="black", tl.srt=45, cl.ratio = .1, mar=c(0,0,2,0))









