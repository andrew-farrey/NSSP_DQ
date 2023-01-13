# NSSP RStudio Workbench Detecting Duplicate Patients Example
# NSSP DQSC Meeting 1/13

if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(Rnssp, tidyverse, lubridate, httr, janitor, kableExtra, grid, gridExtra, 
               jsonlite, RODBC, DBI, odbc, data.table, patchwork, slider, cowplot, flextable, sjmisc)

con <- dbConnect(odbc::odbc(), dsn = "BioSense_Platform")

# Put your facility's C_Biosense_Facility_ID in the '#####" in the pull below, or remove 'and C_Biosense_Facility_ID='#####''
# to pull messages from all active facilities.

# facility pull ----------------------------------------------------------
messages <- dbGetQuery(con, paste0("Select C_Biosense_Facility_ID, Trigger_Event, Message_ID, Message_Control_ID, Medical_Record_Number, C_BioSense_ID, C_Processed_BioSense_ID, 
                                            C_Visit_Date, Arrived_Date_Time, C_Visit_Date_Time, Admit_Reason_Code, Admit_Reason_Description, Admit_Reason_Segment, Admit_Source, 
                                            C_Chief_Complaint, Chief_Complaint_Segment, Chief_Complaint_Text, Chief_Complaint_Code, Chief_Complaint_Combo, Chief_Complaint_Type, 
                                 Diagnosis_Code, Diagnosis_Combo, Assigned_Patient_Location, Administrative_Sex, C_Patient_Age, C_Patient_County, Facility_Type_Description, 
                                 Facility_Type_Code, Facility_Type_Segment, Patient_Class_Code, C_Patient_Class, C_Patient_Class_Source, Race_Code, Ethnicity_Code,
                                 Patient_Zip, Patient_City,First_Patient_ID,Visit_ID,First_Patient_ID_Type_Code,First_Patient_ID_Assigning_Authority, First_Patient_ID_Assigning_Facility, 
                                 Treating_Facility_ID, Medical_Record_Number_Assigning_Authority, Medical_Record_Number_Assigning_Facility, Patient_Account_Number 
                                 from KY_PR_Processed WHERE cast(C_Visit_Date as date)>='20220101' and cast(C_Visit_Date as date)<='20220910' 
                                 and C_Biosense_Facility_ID='#####'"))

encounters <- messages %>% 
  group_by(C_BioSense_ID) %>% 
  slice(which.max(Arrived_Date_Time)) %>% 
  ungroup()

facility <- encounters %>% 
  select(C_BioSense_ID, Message_ID, Message_Control_ID, Medical_Record_Number, Sex = Administrative_Sex, Age = C_Patient_Age, C_Visit_Date_Time, C_Chief_Complaint, 
         Diagnosis_Code, Patient_Zip, Patient_City, Race_Code, Ethnicity_Code, Treating_Facility_ID, Assigned_Patient_Location, 
         First_Patient_ID,Visit_ID,First_Patient_ID_Type_Code,First_Patient_ID_Assigning_Authority, 
         First_Patient_ID_Assigning_Facility, First_Patient_ID_Type_Code, Medical_Record_Number_Assigning_Authority, 
         Medical_Record_Number_Assigning_Facility, Patient_Account_Number) %>%
  distinct() %>% 
  mutate(Age = as.numeric(Age)) %>%  
  arrange(C_Visit_Date_Time)


# Encounters sent through different facility feed; identifiable due to correctly formatted Race_Code field (likely not applicable
# but will leave for reference)
other_facility <- facility %>% 
  filter(Race_Code %in% c('2106-3', '1002-5', '2054-5', '2131-1', '2028-9'))

# Duplicates by Patient_Account_Number (could insert multiple other possibly unique identifiers)
facility.duplicate.ids <- facility %>% 
  group_by(C_Visit_Date_Time, Patient_Account_Number) %>% 
  count() %>% 
  filter(n > 1) %>% 
  ungroup() %>% 
  select(Patient_Account_Number) %>% 
  pull()

facility.duplicates <- facility %>% 
  filter(Patient_Account_Number %in% facility.duplicate.ids) %>% 
  arrange(desc(C_Visit_Date_Time))

# Other options for identifying unique encounters: First_Patient_ID, Visit_ID, Medical_Record_Number
# Your choice will depend on which fields are *actually* uniquely assigned to each patient encounter for that particular facility.
# You'll need to do a record-level review and examine patient identifiers, demographic fields, chief complaint/diagnosis code
# for similarities. Arranging by C_Visit_Date_Time is helpful for this.
