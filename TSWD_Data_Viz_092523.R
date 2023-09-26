#install tidyverse and ggplot within tidy verse
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("writexl")
install.packages("fuzzyjoin")
install.packages("cli")

library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)
library(fuzzyjoin)

# read and install cardata into workable dataframe. 
cardat = "carbitrage.xlsx"
cardatwb = read_excel(cardat)

# convert to lowercase, remove spaces
normcarcounts = function(category){
  category = tolower(category)
  category = gsub(" ", "", category)
  return(category)
}

# normalize make and model columns
cardatwb = cardatwb %>%
  mutate(
    make = normcarcounts(make),
    model = normcarcounts(model)
  )

# find similar makes and models
makesim = stringdist_left_join(cardatwb, cardatwb, by = c("make" = "make"), method = "lv", max_dist = 2)
modelsim = stringdist::stringdistmatrix(cardatwb$model, cardatwb$model) <= 1

# clean data, normalize and unify categories, and adjust for misspellings.

vocabstrings = function(similarity_matrix){
  simgroups = list()
  for (string in input_vector) {
    matchedgroup = NULL
    for (group in simgroups) {
      if (any(stringdist::stringdistmatrix(string, group) <= threshold)) {
        matchedgroup = group
        break
      }
    }
    if (is.null(matchedgroup)) {
      simgroups[[length(simgroups) + 1]] = string
    } else {
      matchedgroup[[length(matchedgroup) + 1]] = string
    }
  }
  return(simgroups)
}

# prep data, count frequency for each make and model.




carcounts = cardatwb %>%
  group_by(make, model) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#create visualization 1: A visualization that illustrates what makes and models of cars are most popular, using ggplot
ggplot(carcounts, aes(x = reorder(paste(make, model), -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    tile = "Most Popular Car Makes and Models", 
    x = "Make and Model",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )





dramdat = read.table("dram_shop.xlsx")
unhoused = read.table("unhoused.xlsx")
