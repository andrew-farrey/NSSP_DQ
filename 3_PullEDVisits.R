# DQ Subcommittee R Walkthrough #3, Pull ED Visits---------------------------------------------

# This assumes you have loaded the relevant R packages and are already connected to BioSense.
# If you aren't, you'll to do so again, so uncomment the below code.
# con <- dbConnect(odbc::odbc(), dsn = "BioSense_Platform")


# I'm picking a reasonable number of dates to work with, a week of data in this case. 
# 
x1 <- as.Date(Sys.Date() - 8)
y1 <- as.Date(Sys.Date() - 1)
x <- gsub("-", "", x1) 
y <- gsub("-", "", y1)

# I chose to put the dates in using paste0() but you can also just type hard-coded dates instead (convenient for exploratory queries).

# Next, we're going to pull all messages from ED visits.
# 
# It can be helpful to reduce the number of message fields pulled queries into fewer variables for several reasons: to reduce 
# the memory required by the pull itself, reduce how long it takes, or reduce the resulting space the data occupies. 

# This variable list pulls all NSSP Priority 1 and Priority 2 variables.

ED <- dbGetQuery(con, paste0("SELECT Feed_Name, C_Biosense_Facility_ID, C_Biosense_ID, Visit_ID, Processed_ID, 
                             Processing_ID, C_Unique_Patient_ID, Message_ID, Treating_Facility_ID, Sending_Facility_ID, 
                             Facility_Type_Code, C_FacType_Patient_Class, Patient_Class_Code, Admit_Date_Time, 
                             Admit_Reason_Description, C_Chief_Complaint, Chief_Complaint_Text, C_Patient_Age, C_Patient_Age_Years, 
                             Diagnosis_Code, Diagnosis_Description, Patient_Zip, Trigger_Event, Arrived_Date, Arrived_Date_Time, 
                             C_Visit_Date, C_Visit_Date_Time, Message_Date, Message_Date_Time, Recorded_Date_Time, 
                             Create_Raw_Date_Time, Sending_Facility_ID_Source, Discharge_Disposition, Discharge_Date_Time, 
                             First_Patient_ID, Medical_Record_Number, Admit_Reason_Code, Chief_Complaint_Code, Diagnosis_Type, 
                             Administrative_Sex, Age_Reported, Age_Units_Reported, C_Patient_Age_Units, C_Patient_County, 
                             Ethnicity_Code, Ethnicity_Description, Patient_City, Patient_Country, Patient_State, Race_Code, 
                             Race_Description, Version_ID FROM ",params$site,"_PR_Processed WITH (nolock) WHERE C_Patient_Class='E' 
                             and cast(C_Visit_Date as date) >='",x,"'"," and cast(C_Visit_Date as date) <='",y,"'"))

# Join to your facility names

ED <- ED %>%
  left_join(names, by = "C_Biosense_Facility_ID") %>%
  mutate(C_Visit_Date = as.Date(C_Visit_Date))


# The sjmisc package makes it really simple to get summaries of variable values
# Granted these are still total messages not limited to individual patients yet

ED %>% frq(Facility_Type_Description)
ED %>% frq(Facility_Type_Code)

ED %>% frq(Race_Code)
ED %>% frq(Race_Description)
ED %>% frq(Ethnicity_Code)
ED %>% frq(Ethnicity_Description)

ED %>% frq(Administrative_Sex)

# Message Counts by Facility
ED %>% frq(Facility_Name)
