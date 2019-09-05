# Display ABAKUS cumulative distribution
# Author: Daniele Z.
# Date: 03/09/2019

# Load ABAKUS data -----------------------------------------------
# file_name <- "test_data.txt" # Select ABACUS file name manually
# Select file using Windows Prompt
file_name <- choose.files(default = "", caption = "Select file",
                          multi = FALSE, filters = Filters,
                          index = nrow(Filters))
# Import text file and convert to a data frame
abakus_data <- read.delim2(file = file_name,
                           sep = ";",
                           skip = 6,
                           header = TRUE)
rm(file_name) # remove variables
# -----------------------------------------------------------------

# SETTINGS --------------------------------------------------------
start_row_index <- 3 # first line of interest
last_row_index <- length(abakus_data$Index) # last line of interest
start_column_index <- 3 # first channel of interest
last_column_index <- length((abakus_data)) #last channel of interest

# Define size classes
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,14.1,15.5,80.0)
# -----------------------------------------------------------------


# Calculate cumulative distribution
cumulative_distribution <- colSums(abakus_data[start_row_index:last_row_index,start_column_index:last_column_index])

# Two plot on same figure
par(mfrow=c(1,2))

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

# Defalut plot settings
par(mfrow=c(1,1))