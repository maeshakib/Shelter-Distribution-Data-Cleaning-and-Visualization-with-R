

#1. Load Necessary Libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
#load writexl library for export data to excel format
library(writexl)

setwd('C:/Users/SHAKIBM/OneDrive - UNHCR/Shelter distribution R dashboard')

# Read the Excel file and list sheet names
file_path <- "ShelterDistribution.xlsx"
data <- read_excel(file_path,sheet = "Improved & Composite")
print(data)




#add underscore with in column name values
data <- data %>%
  rename_with(~ gsub(" ", "_", .))
#check structure
str(data)

# Add New Column with Specific Data Type
data <- data %>%
  mutate(assistantTypeNew=as.character(NA))
#check structure
str(data)

#Based on One Column Value Add New Value to New Column

data <- data %>%
  mutate(
    assistantTypeNew = case_when(
      Assistance_Type=="MTS"~"Multi Storied Shelter",
      Assistance_Type=="CBS"~"Composite Bamboo Shelter",
      Assistance_Type=="Single Storied Shelter"~"Single Storied Shelter"
    ))
# Add unique ID column
data <- data %>%
  mutate(unique_id = row_number())






#check null vlaue in column House_Number
FamilyWithoutHouseNumber <- data %>%
  filter(is.na(House_Number))
print(FamilyWithoutHouseNumber)



# Print the number of null values
num_null_values <- nrow(FamilyWithoutHouseNumber)
print(paste("Family without House Number :", num_null_values))






# Replace the values in the 'Remarks' column, replace NA to "No remakrs"
data <- data %>%
  mutate(Remarks = case_when(
    Remarks=="Filling Sand-50cft,Brick-132"~ "Filling Sand-50cft,Brick-132",
    Remarks=="HH condition EVI"~ "HH condition EVI",
    Remarks %in% c("Stilt Shelter","Stilt Shelter(One Big Tarpuline In 4 Shelter Coverage)","Stilt Shelter(Tie Down Only Rope in 4 shelter)")~ "Stilt Shelter",
    Remarks %in% c("2 shelter had provided due to 6 Nos of family member and this was an especial request from CiC office to provide 2 shelter due to relocation of existing shelter.",
                   "2 shelter had provided due to 6 Nos of family member and this was an especial request from CiC office to provide 2 shelter due to relocation of existing shelter.",
                   "2 shelter had provided due to 8 Nos of family member",
                   "2 shelter had provided due to 9 Nos of family member","Double Shelter",
                   "Family same but 2 shelters had provided previously due to extended family",
                   "In coordination with UNHCR Shelter Focal and CIC, two separate shelters were provided under this Family for their larger household size (10 members).",
                   "Family same but they have 02 different rooms previously due to extended family",
                   "Family same but they have 02 rooms previously due to extended family","Family same but they have 02 different rooms previously due to extended family",
                   "Family same due to extended family double Shelter provided") ~ "Family same due to extended family double Shelter provided"
    ,    TRUE ~ "No remarks"
  ))

#show data with cleaning Remarks
data
# Get distinct values of the 'Remarks' column
distinct_remarks <- data %>%
  distinct(Remarks)
distinct_remarks


#group by remarks
totalRemarksWiseData <- data %>%
  group_by(Remarks) %>%
  summarise(n())
print(totalRemarksWiseData)




 


#update "HH condition EVI" when EVI=="Yes" & Remarks == "No remarks"
data <- data %>%
  mutate(
    Remarks = case_when(
      Vulnerable == "Yes" & Remarks == "No remarks" ~ "HH condition EVI",
      TRUE ~ Remarks
    )     )

# Convert "NA" to "No"
data <- data %>%
  mutate(Vulnerable = ifelse(is.na(Vulnerable), "No", Vulnerable))

# Get distinct values of the 'Remarks' column
Unique_value_of_Vulnerable <- data %>%
  distinct(Vulnerable)
print(Unique_value_of_Vulnerable)


# Select and display specific columns, including the new one
data <- data%>%
  select(SL, District_Name ,Distribution_date, Name_of_HH,`Father's_Name` , House_Number,Vulnerable ,  Assistance_Type, assistantTypeNew,Remarks)

 

#after update check total data of remarks
totalRemarksWiseData <- data %>%
  group_by(Remarks) %>%
  summarise(n())
print(totalRemarksWiseData)


#Every Assistance type total records and total unique families supported
groupByassistantTypeNew <- data %>%
  group_by( District_Name,assistantTypeNew) %>%
  summarise(
    TotalRecords= n(),
    UniqueFamiliesSupported =n_distinct(House_Number)
  )
print(groupByassistantTypeNew)




########################


# Create a mapping of Camp_Name to integer values
District_name_mapping <- c(
  "Bandarban" = 1,
  "Brahmanbaria" = 2,
  "Chandpur" = 3,
  "Chattogram" = 4,
  "Coxsbazar" = 5,
  "Cumilla" = 6,
  "Feni" = 7,
  "Habiganj" = 8,
  "Khagrachhari" = 9,
  "Kutupalong RC" = 10,
  "Lakshmipur" = 11,
  "Moulvibazar" = 12,
  "Noakhali" = 13,
  "Rangamati" = 14,
  "Sunamganj" = 15,
  "Sylhet" = 16
)

# Add the integer column to the data
data <- data %>%
  mutate(District_Name_int = District_name_mapping[District_Name])


# Group by Camp_Name, assistantTypeNew, and Camp_Name_int, then summarize
groupByassistantTypeNew <- data %>%
  group_by(District_Name, District_Name_int, assistantTypeNew) %>%
  summarise(
    TotalRecords = n(),
    UniqueFamiliesSupported = n_distinct(House_Number)
  ) %>%
  arrange(District_Name_int, assistantTypeNew) # Sort by the integer column and assistantTypeNew

# Convert Camp_Name to a factor with levels in the sorted order
groupByassistantTypeNew <- groupByassistantTypeNew %>%
  mutate(District_Name = factor(District_Name, levels = names(District_name_mapping)[order(District_name_mapping)]))


# Create the stacked bar chart
ggplot(groupByassistantTypeNew, aes(x = District_Name, y = UniqueFamiliesSupported, fill = assistantTypeNew)) +
  geom_bar(stat = "identity") +
  labs(title = "District Name wise Distribution of assistantTypeNew",
       x = "District",
       y = "Total Families Supported",
       fill = "Assistant Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("#0072BC", "#8EBEFF","#DCE9FF"))

 


# Calculate the distinct count of EVI for each House_Number
distinct_count <- data %>%
  group_by(Vulnerable) %>%
  summarise(distinct_Vulnerable_count = n_distinct(House_Number))

# Create a pie chart
ggplot(distinct_count, aes(x = "", y = distinct_Vulnerable_count, fill = as.factor(Vulnerable))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Families Vulnerable count", y = "Count", x = "") +
  theme_void() +
  theme(legend.title = element_blank())+
  geom_text(aes(label = distinct_Vulnerable_count), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c( "#0072BC",
                                "#8EBEFF"))


# Calculate the total distribution of unique families and total records
Total_distribution_Unique_families <- data %>%
  group_by(assistantTypeNew) %>%
  summarise( `Distinct Family Count` = n_distinct(House_Number),
             `Total Records`  = n())

# Reshape the data to a long format for plotting
long_data <- Total_distribution_Unique_families %>%
  pivot_longer(cols = c(`Distinct Family Count`, `Total Records`),
               names_to = "Type",
               values_to = "Count")

 
 # Create a bar chart
ggplot(long_data, aes(x = assistantTypeNew, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Unique Families and Total Records by Assistant Type",
       x = "Assistant Type",
       y = "Count") +
  theme_minimal()+
  # Use custom colors
  scale_fill_manual(values = c("#0072BC", "#8EBEFF"))




# Save the modified dataframe to a new Excel file
output_file_path <- "C:/Users/SHAKIBM/OneDrive - UNHCR/Shelter distribution R dashboard/ShelterDistributionOutput.xlsx"
write_xlsx(data, output_file_path)

# Print success message
print(paste("File saved to", output_file_path))


