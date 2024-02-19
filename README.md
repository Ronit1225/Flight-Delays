# Flight Delays ..
# Steps to Perform:

## 1. Read the dataset

install.packages("readr")
library(readr)

file_path <- "C:\\Users\\Ronit\\Desktop\\Data Analysis\\R programing\\Project\\1657873325_flightdelays.csv"
# Use read_csv() to read the CSV file into a data frame
data <- read_csv(file_path)

# 2. Read the dataset description
head(data)
variable_names <- names(data)
print(variable_names)

# 3. Understand the data
str(data)

# 4. Find out the null values
# Columns to check for missing values
columns_to_check <- c("schedtime", "carrier", "deptime", "dest", "distance", "date", 
                      "flightnumber", "origin", "weather", "dayweek", "daymonth", "tailnu", "delay")

# Checking for missing values in the specified columns
missing_values_columns <- sapply(data[columns_to_check], function(x) sum(is.na(x)))
print(missing_values_columns)
# There are no 'NAN' values in the data set.

# 5. Installing the required packages
install.packages(c("dplyr", "tidyr", "summarytools"))
install.packages(c("ggplot2", "plotly", "ggcorrplot"))
install.packages(c("caret", "randomForest", "glmnet", "xgboost"))

# 6. Understand the summary of descriptive statistics
library(summarytools)

descr(iris)

# 7. Plot the histograms to understand the relationships between scheduled time, carrier, destination, origin, weather, and day of the week
# Assuming 'data' is the name of your data frame
# If you have just loaded the data using read_csv()

library(ggplot2)

# Plot histograms for selected variables
ggplot(data, aes(x = schedtime)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Histogram of Scheduled Time")

ggplot(data, aes(x = carrier)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Histogram of Carrier")

ggplot(data, aes(x = dest)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Histogram of Destination")

ggplot(data, aes(x = origin)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Histogram of Origin")

ggplot(data, aes(x = weather)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Histogram of Weather")

ggplot(data, aes(x = dayweek)) +
  geom_bar(fill = "pink", color = "black") +
  labs(title = "Histogram of Day of the Week")

# 8. Plot the scatter plot for flights on time and delayed

ggplot(data, aes(x = schedtime, y = delay, color = factor(delay), size = distance)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "red"), labels = c("On Time", "Delayed")) +
  labs(title = "Scatter Plot for Flights On Time and Delayed",
       x = "Scheduled Time",
       y = "Delayed",
       color = "Flight Status",
       size = "Distance") +
  theme_minimal()

# 9. Plot the box plot to understand how many days in a month flights are delayed by what time

ggplot(data, aes(x = factor(daymonth), y = delay)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_jitter(position = position_jitter(width = 0.3), color = "darkblue", alpha = 0.5) +
  labs(title = "Enhanced Boxplot of Flight Delays by Day of the Month",
       x = "Day of the Month",
       y = "Delay Time (minutes)") +
  scale_x_discrete(labels = function(x) paste("Day ", x)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability

# 10. Define the hours of departure
data$departure_hours <- as.numeric(substr(data$deptime, 1, 2))
str(data)

# 11.Create a categorical representation of data using a table
delay_table <- table(data$dayweek, data$delay)
print(delay_table)

data$dayweek <- factor(data$dayweek, levels = 1:7)
delay_table <- table(data$dayweek, data$delay)
dimnames(delay_table) <- list("Day of the Week" = levels(data$dayweek), "Delay Status" = levels(data$delay))

print(delay_table)

# 12.Redefine the delay variables
library(dplyr)
data <- mutate(data, delay_category = ifelse(delay > 0, 1, 0))
str(data)

# 13.Understand the summary of major variables
library(summarytools)
descr(data)


# 14. Plot histograms of major variables
# Histogram for 'daymonth'
ggplot(data, aes(x = daymonth)) +
  geom_histogram(fill = "lightblue", color = "darkblue", bins = 20, alpha = 0.7) +
  labs(title = "Histogram of Day of the Month",
       x = "Day of the Month",
       y = "Frequency") +
  theme_minimal()

# Histogram for 'delay_category'
ggplot(data, aes(x = factor(delay_category), fill = factor(delay_category))) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  labs(title = "Histogram of Delay Category",
       x = "Delay Category",
       y = "Frequency") +
  theme_minimal()

# Histogram for 'departure_hours'
ggplot(data, aes(x = departure_hours)) +
  geom_histogram(fill = "lightblue", color = "darkblue", bins = 20, alpha = 0.7) +
  labs(title = "Histogram of Departure Hours",
       x = "Departure Hours",
       y = "Frequency") +
  theme_minimal()

# Histogram for 'deptime'
ggplot(data, aes(x = deptime)) +
  geom_histogram(fill = "lightblue", color = "darkblue", bins = 20, alpha = 0.7) +
  labs(title = "Histogram of Departure Time",
       x = "Departure Time",
       y = "Frequency") +
  theme_minimal()

# Histogram for 'distance'
ggplot(data, aes(x = distance)) +
  geom_histogram(fill = "lightblue", color = "darkblue", bins = 20, alpha = 0.7) +
  labs(title = "Histogram of Distance",
       x = "Distance",
       y = "Frequency") +
  theme_minimal()

# Histogram for 'flightnumber'
ggplot(data, aes(x = flightnumber)) +
  geom_histogram(fill = "lightblue", color = "darkblue", bins = 20, alpha = 0.7) +
  labs(title = "Histogram of Flight Number",
       x = "Flight Number",
       y = "Frequency") +
  theme_minimal()

# 15.Plot a pie chart to see how many flights were delayed
# Create a pie chart using ggplot2
pie_chart <- ggplot(flight_delay_df, aes(x = "", y = Count, fill = factor(`Delay Category`))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Proportion of Delayed vs On-Time Flights by Flight Number",
       fill = "Delay Category") +
  theme_minimal() +
  theme(legend.position = "right")

# Create a table for delayed flights
delayed_flights_table <- delayed_flights %>%
  select(`Flight Number`, Count)

print(pie_chart)

# Print the table of delayed flights
print(delayed_flights_table)


