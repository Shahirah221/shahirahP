
#1) A function that detects missing values in dataset

detect_missing_values <- function(data) {

  missing_count <- numeric(ncol(data))
  names(missing_count) <- colnames(data)

  for(i in 1:ncol(data)){
    missing_count[i] <- sum(is.na(data[[i]]))
  }

  missing_count <- missing_count[missing_count > 0]

  return(missing_count)
}

datasample <- airquality
detect_missing_values(datasample)

#2) Median for each value & 3) Replace the missing values with its median value

fill_missing_with_median <- function(data) {
  for (i in 1:ncol(data)) {
    # Check if there are missing values in the column
    if (any(is.na(data[[i]]))) {
      # Calculate the median, ignoring NA values
      median_value <- median(data[[i]], na.rm = TRUE)
      # Replace NA values with the median
      data[[i]][is.na(data[[i]])] <- median_value
    }
  }
  return(data)
}

fill_missing_with_median(datasample)


#4) Print the dataset without missing values

datasample <- fill_missing_with_median(airquality)

print(datasample)
