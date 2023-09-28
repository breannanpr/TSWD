##### VISUALIZATION ONE - EASY #####
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

##### VISUALIZATION TWO - EASY #####
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

##### VISUALIZATION THREE - EASY #####
library(ggplot2)

# Read and install the car data into a workable dataframe.
cardat = read.csv("carbitrage.csv")

# Convert 'time_posted' to date format
cardat$time_posted = as.Date(cardat$time_posted)

# Aggregate posting counts by day and city
postingrates = cardat %>%
  group_by(location, day = as.Date(time_posted)) %>%
  summarize(posting_count = n())

# Include top 20 locations (city)
top20locations = postingrates %>%
  group_by(location) %>%
  summarize(total_posting_count = sum(posting_count)) %>%
  arrange(desc(total_posting_count)) %>%
  head(20)  # Adjust the number as needed

# Filter the posting rates data to include only the top 20 locations
filteredpostrates = postingrates %>%
  filter(location %in% top20locations$location)

# heat map plot
ggplot(filteredpostrates, aes(x = day, y = reorder(location, posting_count), fill = posting_count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  labs(
    title = "San Francisco / Bay Area in Top 20 High Frequency Posting Trends",
    subtitle = "Heatmap of posting rates by city and day (Top 20 Locations)",
    x = "Day",
    y = "City",
    fill = "Posting Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0),
    axis.text.y = element_text(angle = 0, hjust = 1)
  )

##### VISUALIZATION FOUR - MEDIUM #####
library(ggplot2)
library(dplyr)

#read in data
cardat = read.csv("carbitrage.csv")

# create a plot to address by location
ggplot(cardat, aes(x = time_posted, fill = location)) +
  geom_histogram(binwidth = 3600, position = "dodge") +  # Adjust binwidth as needed for time intervals (1 hour in seconds)
  labs(
    title = "Car Posting Trends by Location",
    x = "Time Posted",
    y = "Posting Count",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"  # Adjust the legend position
  )

##### VISUALIZATION FIVE - HARD #####
library(ggplot2)
library(dplyr)
library(scales)

#read in data
cardat = read.csv("carbitrage.csv")

#specify parameters
subimake = "subaru"
subimodel = "forester"
minyear = 2005
maxyear = 2023

# filter data based on parameters listed above
subidat = cardat %>%
  filter(make == subimake,
         model == subimodel,
         year >= minyear,
         year <= maxyear,
         odometer < 300000)

#create scatter plot for age vs mileage with price as size
ggplot(subidat, aes(x = year, y = odometer, color = price)) +
  geom_point(alpha = 5) +
  labs(
    title = "Unlocking Forester Resale Values",
    subtitle = "Newer, Low-Mileage Subarus Demand Top Dollar!",
    x = "Year",
    y = "Mileage",
    color = "Price"
  ) +
  scale_size_continuous(range = c(5, 10), labels = comma) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_shape_manual(values = c(20, 19)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0),
    axis.text.y = element_text(angle = 0, hjust = 1)
  ) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "K"))
