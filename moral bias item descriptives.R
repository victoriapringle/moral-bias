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


sp_items = sp_items[, -c(10:12,17,21,23)]
i_items = i_items[, -c(3,6,12:13)]


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
mtext("Correlations among informant-reported traits", at=12, line=.5, cex=1.5)





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






