

### Creating a sample HR data set & Primary analysis

# 00 Create a sample workforce dataframe
# Load necessary libraries
library(dplyr)
library(openxlsx)
library(tidyr)
# Setting seed for reproducibility
set.seed(123)
# Number of rows
n <- 100
# Generating random data
df <- data.frame(
  ID = 1:n,
  StartDate = as.Date("2015-01-01") + sample(0:(365*9), n, replace = TRUE),  # Random dates between 2015 and 2024
  DOB = as.Date("1980-01-01") + sample(0:(365*25), n, replace = TRUE),  # Random dates between 1980 and 2005
  Age = sample(18:44, n, replace = TRUE),  # Age based on DOB, can be adjusted if needed
  Address1 = paste("Street", sample(1:100, n, replace = TRUE)),
  Address2 = paste("Building", sample(1:50, n, replace = TRUE)),
  Address3 = paste("Floor", sample(1:10, n, replace = TRUE)),
  PostCode = paste0(sample(LETTERS, n, replace = TRUE), sample(100:999, n, replace = TRUE)),
  City = sample(c("Dublin", "Cork", "Galway", "Limerick", "Clane", "Drogheda", "Arklow"), n, replace = TRUE),
  County = sample(c("Dublin", "Cork", "Galway", "Limerick", "Kildare", "Meath", "Wicklow"), n, replace = TRUE),
  Country = rep("Ireland", n),
  Gender = sample(c("Male", "Female"), n, replace = TRUE),
  MaritalStatus = sample(c("Single", "Married", "Divorced", "Widow"), n, replace = TRUE),
  RightToWork = sample(c("ROI", "EU", "Visa"), n, replace = TRUE),
  VisaType = NA,  # Initialize as NA
  VisaExpiryDate = NA,  # Initialize as NA
  ContractType = sample(c("Full Time", "Part Time"), n, replace = TRUE),
  Hours = sample(c(20, 30, 37, 39, 40), n, replace = TRUE),
  ContractDuration = sample(c("Permanent", "Fixed Term", "Superannuation"), n, replace = TRUE),
  ContractType2 = sample(c("Employee", "Contractor"), n, replace = TRUE),
  IDNumber = sample(1000000:9999999, n, replace = TRUE),
  IdExpiryDate = as.Date("2025-01-01") + sample(0:(365*5), n, replace = TRUE),  # Random dates not before today
  JobTitle = sample(c("Analyst", "Manager", "Engineer", "Assistant", "Specialist"), n, replace = TRUE),
  Department = sample(c("Operations", "CS", "Sales", "Procurement", "Finance", "HR", "IT", "Marketing"), n, replace = TRUE),
  SupervisorName = paste("Supervisor", sample(1:10, n, replace = TRUE)),
  LineManager = paste("Manager", sample(1:10, n, replace = TRUE)),
  EmailAddress = paste0("user", 1:n, "@company.com"),
  BankName = sample(c("Bank A", "Bank B", "Bank C"), n, replace = TRUE),
  BIC = paste0("BIC", sample(100:999, n, replace = TRUE)),
  IBAN = paste0("IBAN", sample(100000000:999999999, n, replace = TRUE)),
  BaseSalary = ifelse(sample(c(TRUE, FALSE), n, replace = TRUE), sample(30000:80000, n, replace = TRUE), 0),
  HourlyRate = ifelse(sample(c(TRUE, FALSE), n, replace = TRUE), sample(15:40, n, replace = TRUE), 0),
  Bonus = sample(0:10000, n, replace = TRUE),
  PentionERCont = sample(500:5000, n, replace = TRUE),
  PentionEECon = sample(200:2000, n, replace = TRUE),
  PensionEEVolContr = sample(0:1000, n, replace = TRUE),
  HRPerson = paste("HR", sample(1:5, n, replace = TRUE)),
  Salary = ifelse(sample(c(TRUE, FALSE), n, replace = TRUE), "Yes", "No"),
  EmergencyContName = paste("Emergency", sample(1:100, n, replace = TRUE)),
  EmergencyConRelationship = sample(c("Parent", "Sibling", "Spouse", "Friend"), n, replace = TRUE),
  EmergencyContPhoneNr = paste("08", sample(1000000:9999999, n, replace = TRUE))
)

# Adjusting cities, counties, VisaType, and VisaExpiryDate
df <- df %>%
  mutate(
    County = case_when(
      City == "Dublin" ~ "Dublin",
      City == "Cork" ~ "Cork",
      City == "Galway" ~ "Galway",
      City == "Limerick" ~ "Limerick",
      City == "Clane" ~ "Kildare",
      City == "Drogheda" ~ "Meath",
      City == "Arklow" ~ "Wicklow",
      TRUE ~ County  # Default to the existing value if no match is found
    ),
    VisaType = case_when(
      RightToWork %in% c("ROI", "EU") ~ "Not_Applicable",
      RightToWork == "Visa" ~ sample(c("GEP", "CSEP", "ICT", "DPSEP", "Stamp1G"), 1, replace = TRUE)
    ),
    VisaExpiryDate = case_when(
      RightToWork %in% c("ROI", "EU") ~ as.Date(NA),
      RightToWork == "Visa" ~ as.Date("2025-01-01") + sample(0:(365*5), 1, replace = TRUE)
    )
  )

# Correcting Probation Date and Status
library(lubridate)
df <- df %>%
  mutate(
    ProbationDate = StartDate %m+% months(6),  # Set Probation Date to 6 months from StartDate
    ProbationStatus = ifelse(ProbationDate <= Sys.Date(), "Completed", "On")  # Status based on Probation Date
  )

# Review of the generated dataset, checking dimention and structure
dim(df)
summary(df)
View(df)

# Saving the dataframe to an Excel file
write.xlsx(df,  "C:/Users/Maja/Documents/Learning/MS_Projects/Portfolio 2024/GitHub Portfolio/00 Data Validation/sample_employee_data.xlsx")

#### *****  Data Validation Steps - Primary analysis ***** 

### 1 - Checking for missing values and reporting on those:

## 1a - Checking for missing values by column
MissingV_by_columns <- colSums(is.na(df))
print("Missing Values in Employee data set by columns:")
print(MissingV_by_columns)

#--> Here it is reported that 67 VisaExpiryDates are missing. However, knowing the data set, it is known that those missing dates may 
# belown to those employees who hold ROI or EU ID, and the expiry date of that ID will be stored in IdExpiryDate column. 

# Correction for 67 VisaExpiryDates which are not really missing, just not applicable. 
# Create a copy of the data to avoid modifying the original
df_copy <- df
# Replace NA in VisaExpiryDate with a placeholder date where VisaType is "Not_Applicable"
df_copy$VisaExpiryDate <- ifelse(df_copy$VisaType == "Not_Applicable" & is.na(df_copy$VisaExpiryDate),
                                 as.Date("9999-12-31"),  # Use a distant future date as a placeholder
                                 df_copy$VisaExpiryDate)

# Now calculate missing values, excluding "Not_Applicable" VisaExpiryDate cases
MissingV_by_columns <- colSums(is.na(df_copy))

# Print the results
print("Missing Values in Employee data set by columns (excluding Not_Applicable VisaExpiryDate):")
print(MissingV_by_columns)

## 1b -  # Filter rows with missing values
rows_with_na <- df_copy[apply(df_copy, 1, function(row) any(is.na(row))), ]
rows_with_na
View(rows_with_na)

## 1c - Percentage of missing values
# Calculating the percentage of missing values
total_rows <- length(df$ID)
total_rows
missing_values_percentage <- (MissingV_by_columns / total_rows) * 100
missing_values_percentage
# Creating a data frame to display the results 
percentage_of_missing_values_df <- data.frame(
  Column = names(missing_values_percentage),
  MissingValues = MissingV_by_columns,
  Percentage = missing_values_percentage
)
print(percentage_of_missing_values_df)
countOf_missingValues = sum(MissingV_by_columns)
countOf_missingValues
# Calculating percentage of the missing values out of the 100 rows:
missing_values_percentage_outOfTotalRows <- (countOf_missingValues /total_rows ) * 100
print(missing_values_percentage_outOfTotalRows)
#--> Missing values = 0%

### 2 - Checking for duplicates
empid_unique_count<- length(unique(df$ID))
empid_unique_count
# Checking for and counting duplicates
duplicate_count <- df %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  summarize(count = n())
duplicate_count

# --> No duplicates detected

### 3 - Reporting overdue Probation  
# Filter rows where ProbationDate is due (past or today) and ProbationStatus is "On"
probation_overdue <- df %>%
  filter(ProbationDate <= Sys.Date() & ProbationStatus == "On")
# View the report
probation_overdue

# 4 - Checking for expired Visa and ID 
# Filter rows where VisaExpiryDate or IdExpiryDate is today or  before today.
expired_documents <- df %>%
  filter(VisaExpiryDate <= Sys.Date() | IdExpiryDate <= Sys.Date())

# View the report of expiring Visa or ID dates
expired_documents

# 5.1 - Probation expiring within 1 month form today

one_month_from_now <- Sys.Date() %m+% months(1)

probation_due <- df %>%
  filter(ProbationDate <= one_month_from_now & ProbationStatus == "On")
# View the report
probation_due

# 5.2 -  Identifying Visas and ID Dates expiring within one month
# Calculate one month from today
one_month_from_now <- Sys.Date() %m+% months(1)
# Filter rows where VisaExpiryDate or IdExpiryDate is within one month from today
expiring_documents <- df %>%
  filter(VisaExpiryDate <= one_month_from_now | IdExpiryDate <= one_month_from_now)
# View the report of expiring Visa or ID dates
expiring_documents

### As that created data set had no missing values, no expiring visas and IDs, the exported excel file will be sligtly altered.
### There will be some missing values that will need to be detected.