

#### *****  Data Validation Steps ***** 

# 0 Import data and load necessary libraries
# Data Validation steps
# 1 Checing for misssing values 
# 1a By columns 
# 1b By row
# 1c Calculating percentage of missing values
# 2 Checking for duplicates
# 3 Checking for overdued probation
# 4 Checking for expired documents Visa and ID 
# 5 Additional checks:
# 5.1 Probations due in 1 month from today
# 5.2 Listing Visas and IDs expiring within 1 month form today


### Load necessary libraries
library(dplyr)
library(openxlsx)
library(readxl)
library(tidyr)

### Importing an altered sample of HR data
data <- read_excel(file.choose())
# Data review
dim(data)
str(data)

# As the ProbationDate appears as a character, it needs to be change to a date format:
# If the dates are Excel serial numbers based on January 1, 1900:
data$ProbationDate <- as.Date(as.numeric(data$ProbationDate), origin = "1899-12-30")

# Verify the conversion
str(data) 
View(data)

### 1 - Checking for missing values and reporting on those:
##  1a - Checking for missing values by column 

# Create a copy of the data to avoid modifying the original
data_copy <- data
# Replace NA in VisaExpiryDate with a placeholder date where VisaType is "Not_Applicable"
data_copy$VisaExpiryDate <- ifelse(data_copy$VisaType == "Not_Applicable" & is.na(data_copy$VisaExpiryDate),
                                   as.Date("9999-12-31"),  # Use a distant future date as a placeholder
                                   data_copy$VisaExpiryDate)
# Now calculate missing values, excluding "Not_Applicable" VisaExpiryDate cases
MissingV_by_columns <- colSums(is.na(data_copy))
# Print the results
print("Missing Values in Employee data set by columns (excluding Not_Applicable VisaExpiryDate):")
print(MissingV_by_columns)

# --> There are a number of missing values
# --> One DOB, one IDNumber, one  EmergencyContPhoneNr, two IBAN numbers and one ProbationDate 

## 1b -  Filter rows with missing values
rows_with_na <- data_copy[apply(data_copy, 1, function(row) any(is.na(row))), ]
rows_with_na
View(rows_with_na)

## 1c - Calculating the percentage of missing values
total_rows <- length(data$ID)
total_rows
missing_values_percentage <- (MissingV_by_columns / total_rows) * 100
missing_values_percentage
# Creating a data frame to display the results 
percentage_of_missing_values_data <- data.frame(
  Column = names(missing_values_percentage),
  MissingValues = MissingV_by_columns,
  Percentage = missing_values_percentage
)
print(percentage_of_missing_values_data)
countOf_missingValues = sum(MissingV_by_columns)
countOf_missingValues
# Calculating percentage of the missing values out of the 100 rows:
missing_values_percentage_outOfTotalRows <- (countOf_missingValues /total_rows ) * 100
print(missing_values_percentage_outOfTotalRows)
#--> Missing values = 6%


### 2 - Checking for duplicates
empid_unique_count<- length(unique(data$ID))
empid_unique_count
# Checking for and counting duplicates
duplicate_count <- data %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  summarize(count = n())
duplicate_count

# --> No duplicates detected

### 3 - Reporting overdue Probation  
# Filter rows where ProbationDate is due (past or today) and ProbationStatus is "On"
probation_overdue <- data %>%
  filter(ProbationDate <= Sys.Date() & ProbationStatus == "On")
# View the report
probation_overdue

#--> No overdued probation to be reported. 

### 4 - Checking for expired Visa and ID 
# Filter rows where VisaExpiryDate or IdExpiryDate is today or  before today.
expired_documents <- data %>%
  filter(VisaExpiryDate <= Sys.Date() | IdExpiryDate <= Sys.Date())

# View the report of expiring Visa or ID dates
expired_documents

# --> No expired documents to be reported

### 5.1 - Probation expiring within 1 month form today

one_month_from_now <- Sys.Date() %m+% months(1)

probation_due <- data %>%
  filter(ProbationDate <= one_month_from_now & ProbationStatus == "On")
# View the report
probation_due
View(probation_due)

### 5.2 -  Identifying Visas and ID Dates expiring within one month
# Calculate one month from today
one_month_from_now <- Sys.Date() %m+% months(1)
# Filter rows where VisaExpiryDate or IdExpiryDate is within one month from today
expiring_documents <- data %>%
  filter(VisaExpiryDate <= one_month_from_now | IdExpiryDate <= one_month_from_now)
# View the report of expiring Visa or ID dates
expiring_documents
View(expiring_documents)






