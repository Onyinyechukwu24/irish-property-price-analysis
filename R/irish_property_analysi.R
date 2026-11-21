
# TASK 1: Import Irish Property Price Register

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
