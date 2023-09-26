#install tidyverse and ggplot within tidy verse
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readxl")

# read and install cardata into workable dataframe. 
cardat = read.table("carbitrage.xlsx")
head(cardat)



dramdat = read.table("dram_shop.xlsx")
unhoused = read.table("unhoused.xlsx")
