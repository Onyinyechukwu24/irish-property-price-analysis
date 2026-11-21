
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



# 2.3 : Data Type 

# Examine data types of all variables
sapply(property_data, class)

# Generate a statistical summary
summary(property_data)

# Check unique property descriptions
unique(property_data[["Description of Property"]])

# Check unique counties
unique(property_data[["County"]])


# 2.4 : Property Price 
# Convert property prices into numeric values
sale_price_numeric <- as.numeric(
  gsub("[€,]", "", property_data[["Price (€)"]])
)

# Summary statistics
summary(sale_price_numeric)

# Check for invalid or missing converted values
sum(is.na(sale_price_numeric))

# Display price distribution
hist(
  sale_price_numeric,
  main = "Distribution of Irish Property Sale Prices",
  xlab = "Property Price (€)",
  col = "lightpink",
  breaks = 50
)

# Close existing graphics devices
graphics.off()

hist(
  sale_price_numeric,
  main = "Distribution of Irish Property Sale Prices",
  xlab = "Property Price (€)",
  col = "lightblue",
  breaks = 50
)


boxplot(
  sale_price_numeric,
  main = "Boxplot of Irish Property Sale Prices",
  ylab = "Property Price (€)",
  col = "lightblue",
  outline = TRUE
)

# Reset all graphics devices
graphics.off()

# Reset plot margins
par(mar = c(4, 4, 2, 1))

windows(width = 9, height = 6)


hist(
  sale_price_numeric,
  main = "Distribution of Irish Property Sale Prices",
  xlab = "Property Price (€)",
  ylab = "Frequency",
  col = "lightblue",
  breaks = 50
)


boxplot(
  sale_price_numeric,
  main = "Boxplot of Irish Property Sale Prices",
  ylab = "Property Price (€)",
  col = "lightblue",
  outline = TRUE
)


hist(
  sale_price_numeric,
  main = "Distribution of Irish Property Sale Prices",
  xlab = "Property Price (€)",
  ylab = "Frequency",
  col = "lightblue",
  breaks = 50
)


# 2.5: OUTLIER


# Calculate first quartile
Q1 <- quantile(
  sale_price_numeric,
  0.25,
  na.rm = TRUE
)

# Calculate third quartile
Q3 <- quantile(
  sale_price_numeric,
  0.75,
  na.rm = TRUE
)

# Calculate interquartile range
IQR_value <- IQR(
  sale_price_numeric,
  na.rm = TRUE
)

# Calculate lower and upper boundaries
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value

# Identify potential outliers
outliers <- sale_price_numeric[
  !is.na(sale_price_numeric) &
    (sale_price_numeric < lower_limit |
       sale_price_numeric > upper_limit)
]

# Display results
cat("First Quartile (Q1):", Q1, "\n")
cat("Third Quartile (Q3):", Q3, "\n")
cat("Interquartile Range:", IQR_value, "\n")
cat("Lower Limit:", lower_limit, "\n")
cat("Upper Limit:", upper_limit, "\n")
cat("Number of Potential Outliers:", length(outliers), "\n")


# value Outilers 

# Count valid prices
cat("Valid prices:",
    sum(!is.na(sale_price_numeric)), "\n")

# Count prices above the upper boundary
cat("Prices above upper boundary:",
    sum(sale_price_numeric > upper_limit, na.rm = TRUE), "\n")

# Check for zero or negative prices
cat("Zero or negative prices:",
    sum(sale_price_numeric <= 0, na.rm = TRUE), "\n")

# Check missing or invalid converted prices
cat("Missing or invalid prices:",
    sum(is.na(sale_price_numeric)), "\n")

# Identify the 10 highest property transactions
top_10_prices <- order(
  sale_price_numeric,
  decreasing = TRUE,
  na.last = NA
)[1:10]

# Display original records
property_data[
  top_10_prices,
  c(
    "Date of Sale (dd/mm/yyyy)",
    "County",
    "Price (€)",
    "Description of Property"
  )
]

# Investigate the 10 highest-value transactions

property_data[
  top_10_prices,
  c(
    "Date of Sale (dd/mm/yyyy)",
    "Address",
    "County",
    "Price (€)",
    "Not Full Market Price",
    "VAT Exclusive",
    "Description of Property"
  )
]


# Check price validity

cat("Valid prices:",
    sum(!is.na(sale_price_numeric)), "\n")

cat("Prices above upper boundary:",
    sum(sale_price_numeric > upper_limit, na.rm = TRUE), "\n")

cat("Zero or negative prices:",
    sum(sale_price_numeric <= 0, na.rm = TRUE), "\n")

cat("Missing prices:",
    sum(is.na(sale_price_numeric)), "\n")



# Display full details of the 10 highest-value transactions

property_data[
  top_10_prices,
  c(
    "Date of Sale (dd/mm/yyyy)",
    "Address",
    "County",
    "Price (€)",
    "Not Full Market Price",
    "VAT Exclusive",
    "Description of Property"
  )
]

# Create potential outlier indicator
property_data$Potential_Outlier <-
  sale_price_numeric > upper_limit

# Check that the lengths match
length(property_data$Potential_Outlier)
nrow(property_data)


# Compare outliers with market price status
table(
  property_data$Potential_Outlier,
  property_data[["Not Full Market Price"]]
)

# Compare outliers with VAT status
table(
  property_data$Potential_Outlier,
  property_data[["VAT Exclusive"]]
)

# Compare outliers with property descriptions
table(
  property_data$Potential_Outlier,
  property_data[["Description of Property"]]
)
