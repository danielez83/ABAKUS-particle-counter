file_name <- "test_data.txt" # ABACUS file name
skip_lines = 6; #number of lines to skip

# Import text file and convert to a data frame
abakus_data <- read.delim2(file = file_name,
            sep = ";",
            skip = 6,
            header = TRUE)
rm(file_name) # remove variables
rm(skip_lines) # remove variables

