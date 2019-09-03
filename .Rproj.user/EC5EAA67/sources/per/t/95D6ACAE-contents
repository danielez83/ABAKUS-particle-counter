# Display ABAKUS cumulative distribution
# Author: Daniele Z.
# Date: 03/09/2019

# abakus_data required

# SETTINGS --------------------------------------------------------
start_row_index <- 3 # first line of interest
last_row_index <- length(abakus_data$Index) # last line of interest
start_column_index <- 3 # first channel of interest
last_column_index <- length((abakus_data)) #last channel of interest

# Define size classes
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,14.1,15.5,80.0)
# -----------------------------------------------------------------

# Calculate cumulative distribution
cumulative_distribution <- colSums(abakus_data[start_row_index:last_index,start_column_index:last_column_index])

# Display cumulative distribution as barplot
barplot(cumulative_distribution,
        names.arg = size_classes,
        space = 0,
        border = NA,
        ylab = "Counts",
        xlab = expression(paste("Diameter (", mu, "m)")))

# Display cumulative distribution as line (log scale)
plot(x = size_classes, y = cumulative_distribution, 
     type = 'l',
     log = "xy",
     ylab = "Counts",
     xlab = expression(paste("Diameter (", mu, "m)")),
     lwd=2,
     col="steelblue3",
     )