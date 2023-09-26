library(ggplot2)
library(dplyr)
library(forcats)  # Load the 'forcats' package

# read and install cardata into a workable dataframe. 
cardat = read.csv("carbitrage.csv")

# this adjusts all text to lowercase
cardat = cardat %>%
  mutate(make = tolower(make),
         model = tolower(model))

#this groups and counts the cars by make and model
carcounts = cardat %>%
  group_by(make, model) %>%
  summarize(count = n())

# Select the top 15 most popular car makes and models
top20carcounts = carcounts %>%
  mutate(make_model = paste(make, model, sep = " ")) %>%
  filter(count > 500) %>%
  arrange(desc(count)) %>%
  head(20)

# Clean the "make_model" column to remove leading and trailing whitespace
top20carcounts$make_model <- trimws(top20carcounts$make_model)

# Filter out "Ford" and blank values from the plot
top20carcounts <- top20carcounts %>%
  filter(!(make_model %in% c("ford", "")))

# Create the ggplot visualization for the modified data
ggplot(top20carcounts,
       aes(x = fct_reorder(make_model, count), y = count)) +
  geom_bar(stat = "identity", fill = "mediumorchid3") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +  # Add count labels
  coord_flip() +
  labs(
    title = "Ford F150 Leads Top 20 Most Popular Vehicles for Sale",
    subtitle = "Craigslist Carbitrage Dataset",
    x = "Make and Model",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0)
  )

####


