# HR-Data-Validation
Data validation ensures the accuracy, consistency, and reliability of data. In this exercise, I will conduct a simple data validation on a sample HR dataset created specifically for this purpose. 

# Background
Data validation ensures the accuracy, consistency, and reliability of data. It helps prevent errors that could impact payroll, compliance, reporting, and overall decision-making. In this exercise, we'll conduct a simple data validation on a sample HR dataset created specifically for this purpose. The sample dataset includes essential HR information:
•	Personal Information
•	Contact Information
•	Employment Information
•	Legal and Compliance Data
•	Payroll and Bank Details
•	Emergency Contact Information

Overall, the dataset contains 43 columns and 100 rows.
## **Here’s what we’ll explore in this project:**
Creating a Sample HR Dataset

The dataset was created and exported to an Excel file. There were initially no missing values, no expired visas, or IDs, so the Excel file was manually altered for this exercise to include missing and expired values that need to be detected. (See attached R file.)
 00 Importing an altered sample of HR data
### **Data Validation Steps**
1 **Checking for Missing Values**
a. By columns
b. By rows
c. Calculating the percentage of missing values
2 **Checking for Duplicates**
3 **Checking for Overdue Probation**
4 **Checking for Expired Documents (Visa and ID)**
5 **Additional Checks:**
5.1. Probations due within 1 month
5.2. Visas and IDs expiring within 1 month
This approach provides a comprehensive view of the data validation process and highlights the importance of accurate and reliable HR data.

## **Ready to dive in? Just keep scrolling!**

### Load necessary libraries
```r
library(dplyr)
library(openxlsx)
library(readxl)
library(tidyr)
```
### Importing an altered sample of HR data
```r
data <- read_excel(file.choose())
```
#### Data review
```
dim(data)
str(data)
```
##### As the ProbationDate appears as a character, it needs to be change to a date format:
##### If the dates are Excel serial numbers based on January 1, 1900:
```r
data$ProbationDate <- as.Date(as.numeric(data$ProbationDate), origin = "1899-12-30")
```
#### Verify the conversion
```r
str(data) 
View(data)
```
### 1 - Checking for missing values and reporting on those:
####  1a - Checking for missing values by column 

### Create a copy of the data to avoid modifying the original
```r
data_copy <- data
```
#### Replace NA in VisaExpiryDate with a placeholder date where VisaType is "Not_Applicable"
```r
data_copy$VisaExpiryDate <- ifelse(data_copy$VisaType == "Not_Applicable" & is.na(data_copy$VisaExpiryDate),
                                   as.Date("9999-12-31"),  # Use a distant future date as a placeholder
                                   data_copy$VisaExpiryDate)
# Now calculate missing values, excluding "Not_Applicable" VisaExpiryDate cases
MissingV_by_columns <- colSums(is.na(data_copy))
# Print the results
print("Missing Values in Employee data set by columns (excluding Not_Applicable VisaExpiryDate):")
print(MissingV_by_columns)
```
##### --> There are a number of missing values
##### --> One DOB, one IDNumber, one  EmergencyContPhoneNr, two IBAN numbers and one ProbationDate 

#### 1b -  Filter rows with missing values
```r
rows_with_na <- data_copy[apply(data_copy, 1, function(row) any(is.na(row))), ]
rows_with_na
View(rows_with_na)
```
#### 1c - Calculating the percentage of missing values
```r
total_rows <- length(data$ID)
total_rows
missing_values_percentage <- (MissingV_by_columns / total_rows) * 100
missing_values_percentage
```
##### Creating a data frame to display the results 
```r
percentage_of_missing_values_data <- data.frame(
  Column = names(missing_values_percentage),
  MissingValues = MissingV_by_columns,
  Percentage = missing_values_percentage
)
print(percentage_of_missing_values_data)
countOf_missingValues = sum(MissingV_by_columns)
countOf_missingValues
```
#### Calculating percentage of the missing values out of the 100 rows:
```r
missing_values_percentage_outOfTotalRows <- (countOf_missingValues /total_rows ) * 100
print(missing_values_percentage_outOfTotalRows)
```
##### --> Missing values = 6%


### 2 - Checking for duplicates
```r
empid_unique_count<- length(unique(data$ID))
empid_unique_count
```
#### Checking for and counting duplicates
```r
duplicate_count <- data %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  summarize(count = n())
duplicate_count
```
##### --> No duplicates detected

### 3 - Reporting overdue Probation  
#### Filter rows where ProbationDate is due (past or today) and ProbationStatus is "On"
```r
probation_overdue <- data %>%
  filter(ProbationDate <= Sys.Date() & ProbationStatus == "On")
# View the report
probation_overdue
```
#####--> No overdued probation to be reported. 

### 4 - Checking for expired Visa and ID 
##### Filter rows where VisaExpiryDate or IdExpiryDate is today or  before today.
```r
expired_documents <- data %>%
  filter(VisaExpiryDate <= Sys.Date() | IdExpiryDate <= Sys.Date())
```
##### View the report of expiring Visa or ID dates
```r
expired_documents
```
##### --> No expired documents to be reported

### 5.1 - Probation expiring within 1 month form today
```r
one_month_from_now <- Sys.Date() %m+% months(1)

probation_due <- data %>%
  filter(ProbationDate <= one_month_from_now & ProbationStatus == "On")
# View the report
probation_due
View(probation_due)
```
### 5.2 -  Identifying Visas and ID Dates expiring within one month
##### Calculate one month from today
```r
one_month_from_now <- Sys.Date() %m+% months(1)
```
##### Filter rows where VisaExpiryDate or IdExpiryDate is within one month from today
```r
expiring_documents <- data %>%
  filter(VisaExpiryDate <= one_month_from_now | IdExpiryDate <= one_month_from_now)
```
##### View the report of expiring Visa or ID dates
```expiring_documents
View(expiring_documents)
```
