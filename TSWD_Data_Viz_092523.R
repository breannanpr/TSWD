library(ggplot2)
library(dplyr)
library(forcats)

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
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)

# Read and install the car data into a workable dataframe.
cardat = read.csv("carbitrage.csv")

# Convert the 'time_posted' column to a datetime object
cardat$time_posted = as.POSIXct(cardat$time_posted, format = "%Y-%m-%d %H:%M:%S")

# Extract day and week information
cardat = cardat %>%
  mutate(day = as.Date(time_posted),
         week = lubridate::floor_date(time_posted, unit = "week"),
         weekday = weekdays(day))  # Extract weekday information

# Group by day and calculate the count of new cars posted
dailycarcounts = cardat %>%
  group_by(day, weekday) %>%
  summarize(count = n())

# Find the top day(s) of the week for posting
topcardays = dailycarcounts %>%
  group_by(weekday) %>%
  summarize(total_count = sum(count)) %>%
  filter(total_count == max(total_count)) %>%
  pull(weekday)

# Find the lowest count value(s)
lowestcarcount = min(dailycarcounts$count)

# Create a ggplot for daily counts
dailycarplot = ggplot(dailycarcounts, aes(x = day, y = count)) +
  geom_line() +
  geom_text(
    aes(label = ifelse(weekday %in% topcardays, as.character(count), "")),
    vjust = -0.5,
    size = 3,
    hjust = 1
  ) +
  geom_point(data = dailycarcounts %>%
               filter(weekday %in% topcardays),
             aes(x = day, y = count),
             color = "maroon",
             size = 3) +
  geom_point(data = dailycarcounts %>%
               filter(count == lowestcarcount),
             aes(x = day, y = count),
             color = "skyblue",
             size = 3)+
  labs(
    title = "Slump Revealed in Daily Car Listings",
    subtitle = "Service Interruption Disrupts Steady Ebb and Flows",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_x_date(
    date_breaks = "20 days",
    date_labels = "%m-%d"
  )

# Display the daily plot
dailycarplot
