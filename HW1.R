## load package check datasets
## install.packages("nycflights13")
library(nycflights13)
data(package = "nycflights13")
flights <- as.data.frame(flights)

## 1. Visualize the distribution of arrival delays per origin airport!
library(ggplot2)
ggplot(flights, aes(origin, arr_delay)) +
  geom_boxplot()

## 2. Visualize the distribution of arrival delays per destination airport! Note that the x axis labels need to be rotated, and spend some time cleaning up the axis and plot titles! Might need to tweak fig.height and fig.width params of the code chunk in Rmd!
ggplot(flights, aes(dest, arr_delay)) +
  geom_boxplot() +
  labs(title = "Distribution of Arrival Delays per Destination Airport",
       x = "Destination Airport",
       y = "Arrival Delay (minutes)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, size = 5))

## 3. Compute and visualize the average arrival delay per destination! Make sure to handle NAs and order the barplot by the average delay.

# Compute average arrival delay per destination
avg_delay <- aggregate(arr_delay ~ dest, data = flights, FUN = mean, na.rm = TRUE)
avg_delay <- avg_delay[order(avg_delay$arr_delay), ]
# Order by average delay
avg_delay$dest <- factor(avg_delay$dest, levels = avg_delay$dest)

ggplot(avg_delay, aes(dest, arr_delay)) +
  geom_col() +
  labs(title = "Average Arrival Delays per Destination Airport",
       x = "Destination Airport",
       y = "Arrival Delay (minutes)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, size = 5))

## 4. Redo the above plot by showing the actual name of the destination airport instead of it’s FAA id!
airports <- as.data.frame(airports)
flights_merged <- merge(flights, airports, by.x = "dest", by.y = "faa" )

# Compute average arrival delay per destination
avg_delay2 <- aggregate(arr_delay ~ name, data = flights_merged, FUN = mean, na.rm = TRUE)

# Order by average delay
avg_delay2 <- avg_delay2[order(avg_delay2$arr_delay), ]
avg_delay2$name <- factor(avg_delay2$name, levels = avg_delay2$name)

ggplot(avg_delay2, aes(name, arr_delay)) +
  geom_col() +
  labs(title = "Average Arrival Delays per Destination Airport",
       x = "Destination Airport",
       y = "Arrival Delay (minutes)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 5))

## 5. Color the bars by the timezone of the airport! Make sure to render the legend on the top, using a single line
avg_delay2 <- merge(avg_delay2, airports, by.x = 'name', by.y = 'name')

ggplot(avg_delay2, aes(name, arr_delay, fill = tzone)) +
  geom_col() +
  labs(title = "Average Arrival Delays per Destination Airport",
       x = "Destination Airport",
       y = "Arrival Delay (minutes)",
       fill = "Timezone") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 5),
        legend.position = 'top',
        legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 1))

## 6. Geocode the destination airports, then visualize those on a worldmap with the point sizes relative to the number of flights to there from NY!

library(ggmap)
world <- map_data('world')

num_of_flights <- as.data.frame(table(flights_merged$name))
colnames(num_of_flights) <- c("name", "num_flights")
num_of_flights <- merge(num_of_flights, airports, by = "name")

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  geom_point(data = num_of_flights,
             aes(x = lon,y = lat, size = num_flights),
             color = 'orange') +
  labs(size = "Number of flights", x = NULL, y = NULL) +
  theme_void() +
  coord_fixed(1.3)

## 7. Compute the average departure and arrival delays, also the average air time and distance per destination, then pass the scaled data frame through MDS to visualize the similarities/dissimilarities of the destination airports!
# Computing the statistics
library(data.table)
flights_merged <- as.data.table(flights_merged)
stats <- flights_merged[, .(
  avg_dep_delay = mean(dep_delay, na.rm = TRUE),
  avg_arr_delay = mean(arr_delay, na.rm = TRUE),
  avg_air_time = mean(air_time, na.rm = TRUE),
  avg_distance = mean(distance, na.rm = TRUE)
), by = dest]

# Dropping the non-numeric column
scaled_data <- scale(stats[, -1])

# Scaling the df and passing it through MDS
mds <- as.data.frame(cmdscale(dist(scale(scaled_data))))

# Add the 'dest' column back to the dataframe
mds$dest <- stats$dest

# Visualizing the mds
library(ggrepel)
ggplot(mds, aes(x = -V1, y = V2, label = dest)) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  geom_text_repel()

ggplot(mds, aes(-V1, V2, label = dest)) + geom_point() + geom_text_repel(size = 3, max.overlaps = 10) + 
  theme_void()
?geom_text_repel
