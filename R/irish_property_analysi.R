
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

# 3:1 Data preparation and cleaning 

property_data_clean <- property_data[
  , names(property_data) != "Potential_Outlier",
  drop = FALSE
]

# Check original working copy dimensions
dim(property_data_clean)

# 3.2 Removing duplicates 

# Count duplicates before removal
duplicates_before <- sum(
  duplicated(property_data_clean)
)

# Remove exact duplicate records
property_data_clean <- property_data_clean[
  !duplicated(property_data_clean),
]

# Count duplicates after removal
duplicates_after <- sum(
  duplicated(property_data_clean)
)

# Display results
cat("Duplicates before cleaning:",
    duplicates_before, "\n")

cat("Duplicates after cleaning:",
    duplicates_after, "\n")

# Check cleaned dataset dimensions
dim(property_data_clean)



# 3.3 Finding the missing value 

# Count missing values after duplicate removal
missing_clean <- colSums(is.na(property_data_clean))

# Calculate missing percentages
missing_clean_percentage <- round(
  colMeans(is.na(property_data_clean)) * 100,
  2
)

# Create summary table
missing_clean_summary <- data.frame(
  Column = names(property_data_clean),
  Missing_Values = missing_clean,
  Missing_Percentage = missing_clean_percentage
)

# Display results
print(missing_clean_summary)



#3.4 finding missimg values 


# Confirm missing values remain identifiable
colSums(is.na(property_data_clean))

# Check dataset dimensions
dim(property_data_clean)

# Confirm the number of columns
ncol(property_data_clean)


# 3.5 dATA TYPE 

property_data_clean[["Date of Sale (dd/mm/yyyy)"]] <-
  as.Date(
    property_data_clean[["Date of Sale (dd/mm/yyyy)"]],
    format = "%d/%m/%Y"
  )

# Convert Price (€) into numeric format
property_data_clean[["Price (€)"]] <-
  as.numeric(
    gsub(
      "[€,]",
      "",
      property_data_clean[["Price (€)"]]
    )
  )

# Check converted data types
sapply(
  property_data_clean[
    c("Date of Sale (dd/mm/yyyy)", "Price (€)")
  ],
  class
)

# Display first six converted records
head(
  property_data_clean[
    c("Date of Sale (dd/mm/yyyy)", "Price (€)")
  ]
)

# Display summary statistics
summary(
  property_data_clean[
    c("Date of Sale (dd/mm/yyyy)", "Price (€)")
  ]
)

# 3.6 Data validation 


# Check date conversion
cat(
  "Missing dates:",
  sum(is.na(property_data_clean[["Date of Sale (dd/mm/yyyy)"]])),
  "\n"
)

# Check price conversion
cat(
  "Missing prices:",
  sum(is.na(property_data_clean[["Price (€)"]])),
  "\n"
)

# Check final dataset dimensions
cat("Final rows:", nrow(property_data_clean), "\n")
cat("Final columns:", ncol(property_data_clean), "\n")

# Check data types
str(property_data_clean)

# Check remaining exact duplicates
cat(
  "Remaining duplicates:",
  sum(duplicated(property_data_clean)),
  "\n"
)

write.csv(
  property_data_clean,
  "outputs/irish_property_price_cleaned.csv",
  row.names = FALSE,
  na = ""
)

# Confirm file was saved
file.exists("outputs/irish_property_price_cleaned.csv")

# Select the GitHub repository folder
setwd(choose.dir())



write.csv(
  property_data_clean,
  "outputs/irish_property_price_cleaned.csv",
  row.names = FALSE,
  na = ""
)

# Confirm successful saving
file.exists("outputs/irish_property_price_cleaned.csv")



# 4 : Data Exploratory

# RQ1: How have property prices changed over time?

# 4.1 Overall descriptive statistics

# Calculate mean property price
mean_price <- mean(
  property_data_clean[["Price (€)"]],
  na.rm = TRUE
)

# Calculate median property price
median_price <- median(
  property_data_clean[["Price (€)"]],
  na.rm = TRUE
)

# Calculate minimum property price
minimum_price <- min(
  property_data_clean[["Price (€)"]],
  na.rm = TRUE
)

# Calculate maximum property price
maximum_price <- max(
  property_data_clean[["Price (€)"]],
  na.rm = TRUE
)

# Calculate standard deviation
price_sd <- sd(
  property_data_clean[["Price (€)"]],
  na.rm = TRUE
)

# Display descriptive statistics
cat("Mean price: €", mean_price, "\n")
cat("Median price: €", median_price, "\n")
cat("Minimum price: €", minimum_price, "\n")
cat("Maximum price: €", maximum_price, "\n")
cat("Standard deviation: €", price_sd, "\n")



# Reload cleaned dataset
property_data_clean <- read.csv(
  "outputs/irish_property_price_cleaned.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)


# Convert sale date back to Date format
property_data_clean[["Date of Sale (dd/mm/yyyy)"]] <-
  as.Date(
    property_data_clean[["Date of Sale (dd/mm/yyyy)"]],
    format = "%Y-%m-%d"
  )

# Ensure property price is numeric
property_data_clean[["Price (€)"]] <-
  as.numeric(property_data_clean[["Price (€)"]])



# Check dataset dimensions
dim(property_data_clean)

# Check data types
sapply(
  property_data_clean[
    c("Date of Sale (dd/mm/yyyy)", "Price (€)")
  ],
  class
)

# Check missing dates and prices
sum(is.na(property_data_clean[["Date of Sale (dd/mm/yyyy)"]]))
sum(is.na(property_data_clean[["Price (€)"]]))

# 4 : Statistics 

# TASK 4.1: DESCRIPTIVE STATISTICS

mean_price <- mean(property_data_clean[["Price (€)"]])
median_price <- median(property_data_clean[["Price (€)"]])
minimum_price <- min(property_data_clean[["Price (€)"]])
maximum_price <- max(property_data_clean[["Price (€)"]])
price_sd <- sd(property_data_clean[["Price (€)"]])

cat("Mean price: €", mean_price, "\n")
cat("Median price: €", median_price, "\n")
cat("Minimum price: €", minimum_price, "\n")
cat("Maximum price: €", maximum_price, "\n")
cat("Standard deviation: €", price_sd, "\n")


# task 4.2 property prices 

property_data_clean$Sale_Year <- as.integer(
  format(
    property_data_clean[["Date of Sale (dd/mm/yyyy)"]],
    "%Y"
  )
)

annual_median <- aggregate(
  property_data_clean[["Price (€)"]],
  by = list(Year = property_data_clean$Sale_Year),
  FUN = median
)

names(annual_median)[2] <- "Median_Price"

print(annual_median)

plot(
  annual_median$Year,
  annual_median$Median_Price,
  type = "o",
  main = "Median Irish Residential Property Prices Over Time",
  xlab = "Year",
  ylab = "Median Property Price (€)",
  col = "blue",
  pch = 16
)

# 4.3 the property prices by the county 

# Calculate median property price by county
county_median <- aggregate(
  property_data_clean[["Price (€)"]],
  by = list(
    County = property_data_clean$County
  ),
  FUN = median,
  na.rm = TRUE
)

# Rename price column
names(county_median)[2] <- "Median_Price"

# Sort counties from highest to lowest
county_median <- county_median[
  order(county_median$Median_Price, decreasing = TRUE),
]

# Display county results
print(county_median)



# Display five counties with highest median prices
top_5_counties <- head(county_median, 5)

print(top_5_counties)




# the five most expensive counties

barplot(
  top_5_counties$Median_Price,
  names.arg = top_5_counties$County,
  main = "Five Most Expensive Counties in Ireland",
  xlab = "County",
  ylab = "Median Property Price (€)",
  col = "lightblue",
  las = 2,
  cex.names = 0.8
)
 
# edidted plot 

#Improved horizontal bar chart

par(mar = c(5, 8, 4, 2))

barplot(
  top_5_counties$Median_Price,
  names.arg = top_5_counties$County,
  horiz = TRUE,
  main = "Five Most Expensive Counties in Ireland",
  xlab = "Median Property Price (€)",
  col = "lightblue",
  las = 1,
  cex.names = 0.9
)

par(mar = c(5, 4, 4, 2) + 0.1)


# 4 : property type 

# Examine property description categories
property_type_counts <- table(
  property_data_clean[["Description of Property"]],
  useNA = "ifany"
)

# Display property types and their frequencies
print(property_type_counts)


# 4 : property type classifcation 

# Extract property descriptions
property_description <-
  property_data_clean[["Description of Property"]]

# Create standardised property categories
property_data_clean$Property_Type <- ifelse(
  grepl("Nua", property_description, ignore.case = TRUE),
  "New",
  ifelse(
    grepl(
      "Second-Hand|Ath",
      property_description,
      ignore.case = TRUE
    ),
    "Second-Hand",
    "Other/Unclassified"
  )
)

# Display standardised categories
table(property_data_clean$Property_Type)

# Check that all records have been classified
sum(is.na(property_data_clean$Property_Type))

#task 4.4 compare prices 

# Calculate median prices by property type
property_type_median <- aggregate(
  property_data_clean[["Price (€)"]],
  by = list(
    Property_Type = property_data_clean$Property_Type
  ),
  FUN = median,
  na.rm = TRUE
)

# Rename price column
names(property_type_median)[2] <- "Median_Price"

# Calculate transaction counts
property_type_counts <- table(
  property_data_clean$Property_Type
)

# Display median prices
print(property_type_median)

# Display transaction counts
print(property_type_counts)

# 4.5 property classifying 


property_description <-
  property_data_clean[["Description of Property"]]

# Create standardised categories
property_data_clean$Property_Type <-
  rep("Other/Unclassified", length(property_description))

# Identify new properties (English and Irish)
new_properties <- grepl(
  "New Dwelling|Nua",
  property_description,
  ignore.case = TRUE
)

# Identify second-hand properties (English and Irish)
second_hand_properties <- grepl(
  "Second-Hand|Ath",
  property_description,
  ignore.case = TRUE
)

# Assign categories
property_data_clean$Property_Type[new_properties] <- "New"

property_data_clean$Property_Type[second_hand_properties] <-
  "Second-Hand"

# Display corrected categories
table(property_data_clean$Property_Type)



# Calculated median prices
property_type_median <- aggregate(
  property_data_clean[["Price (€)"]],
  by = list(
    Property_Type = property_data_clean$Property_Type
  ),
  FUN = median,
  na.rm = TRUE
)

names(property_type_median)[2] <- "Median_Price"

# Display results
print(property_type_median)


#4.5 new vesrus second hand 


#  median price values
property_medians <- property_type_median$Median_Price

#  comparison bar chart
barplot(
  property_medians,
  names.arg = property_type_median$Property_Type,
  main = "Median Prices: New vs Second-Hand Properties",
  xlab = "Property Type",
  ylab = "Median Property Price (€)",
  col = "lightPink",
  ylim = c(0, max(property_medians) * 1.2)
)

# 4.6 transaction

# Compare median prices by market-price status

market_price_comparison <- aggregate(
  property_data_clean[["Price (€)"]],
  by = list(
    Market_Status = property_data_clean[["Not Full Market Price"]]
  ),
  FUN = median,
  na.rm = TRUE
)

names(market_price_comparison)[2] <- "Median_Price"

print(market_price_comparison)



# Compare median prices by VAT status

vat_comparison <- aggregate(
  property_data_clean[["Price (€)"]],
  by = list(
    VAT_Status = property_data_clean[["VAT Exclusive"]]
  ),
  FUN = median,
  na.rm = TRUE
)

names(vat_comparison)[2] <- "Median_Price"

print(vat_comparison)



png(
  "outputs/figures/market_price_status.png",
  width = 1400,
  height = 900,
  res = 150
)

par(mar = c(5, 5, 4, 2))

barplot(
  market_price_comparison$Median_Price,
  names.arg = market_price_comparison$Market_Status,
  main = "Median Prices by Market Price Status",
  xlab = "Not Full Market Price",
  ylab = "Median Property Price (€)",
  col = "lightblue"
)

dev.off()





# Save VAT comparison graph
png(
  "outputs/figures/vat_status_comparison.png",
  width = 1400,
  height = 900,
  res = 150
)

par(mar = c(5, 5, 4, 2))

barplot(
  vat_comparison$Median_Price,
  names.arg = vat_comparison$VAT_Status,
  main = "Median Prices by VAT Status",
  xlab = "VAT Exclusive",
  ylab = "Median Property Price (€)",
  col = "lightblue"
)

dev.off()



file.exists("outputs/figures/market_price_status.png")

file.exists("outputs/figures/vat_status_comparison.png")



# Market price status counts
table(property_data_clean[["Not Full Market Price"]])

# VAT status counts
table(property_data_clean[["VAT Exclusive"]])


getwd()

list.files("outputs/figures")
