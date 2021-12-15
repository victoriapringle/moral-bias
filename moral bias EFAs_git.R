rm(list = ls())    # clear the environment

# working directory ------------------------------------------------------------
setwd("C:/Users/victo/OneDrive/Documents/moral bias")


# libraries --------------------------------------------------------------------
library(dplyr)   # for data cleaning & reshaping etc.


# data -------------------------------------------------------------------------
data <- read.csv("impressiondata_moralbias.csv")  
data = rename(data, i_trustworthy_avg = i_trustworty_avg)
# nb. informants are aggregated (items formatted i_[trait]_avg)


# subset to the items we're currently using


sp_items = data %>% select(starts_with("SP_"))
i_items = data %>% select(ends_with("avg"))


sp_items = sp_items[, -c(6,8:12,17,23:25)]
i_items = i_items[, -c(1,7,12:15)]







