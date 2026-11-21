
# 1: Import Irish Property Price Register

property_data <- read.csv(
  file.choose(),
  fileEncoding = "Windows-1252",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("")
)

# Check dataset dimensions
dim(property_data)

# Check column names
names(property_data)

# Display first six records
head(property_data)


# 2 : Data quality 
# 2.1 Missing values 

# Count missing values in each column
missing_values <- colSums(is.na(property_data))

# Calculate percentage of missing values
missing_percentage <- round(
  colMeans(is.na(property_data)) * 100,
  2
)

# Create missing values summary table
missing_summary <- data.frame(
  Column = names(property_data),
  Missing_Values = missing_values,
  Missing_Percentage = missing_percentage
)

# Display results
print(missing_summary)


# 2.2  Duplicate records

# Identify exact duplicate rows
duplicate_rows <- duplicated(property_data)

# Count duplicate records
duplicate_count <- sum(duplicate_rows)

# Calculate duplicate percentage
duplicate_percentage <- round(
  (duplicate_count / nrow(property_data)) * 100,
  2
)

# Display results
cat("Total records:", nrow(property_data), "\n")

cat("Duplicate records:", duplicate_count, "\n")

cat("Duplicate percentage:",
    duplicate_percentage, "%\n")

# Display first six duplicate records
head(property_data[duplicate_rows, ])

