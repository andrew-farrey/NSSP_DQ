
# Assessing Excessive Message Update Delays--------------------------------

# This code will allow you to check for update messages sent for encounters that
# occurred more than three months prior to the most recently received message update
# for a given encounter

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(odbc)
library(DBI)
library(dbplyr)


# User Inputs --------------------------------------------------------------

# Replace "KY" with your site's short abbreviation below:
site <- "KY"

# Set threshold for what is considered an "extreme" delay
# (represents # of days since encounter occurred)
extreme <- 90

# Set Dates
# Change startDate to increase the range of encounter dates queried

# Make the window smaller to assess for late messages from a smaller range of
# encounter dates, and larger to assess for recent A08 updates to
# encounters that occurred longer ago
startDate <- Sys.Date() - years(1)

# The MFT table setup can't be easily parameterized in this context as-is,
# so you will need to check that the "mft_filter") value is accurate
# in each pull below. (Alternatively, you could set it up as a SQL join
# prior to calling collect())

# Connect to BioSense DB --------------------------------------------------
con <- dbConnect(odbc::odbc(), dsn = "BioSense_Platform")

# Pull MFT for Hospital Names ---------------------------------------------
mft <- tbl(con, in_schema("dbo", paste0(site, '_MFT'))) %>%
  select(Facility_Name, C_Biosense_Facility_ID, Patient_Class_Code, Facility_Type) %>%
  collect() %>%
  select(C_Biosense_Facility_ID, Facility_Name, Patient_Class_Code, Facility_Type) %>%
  mutate(
    # Kentucky-specific string cleaning to remove a duplicate hospital and standardize hospital names
    # feel free to comment out or replace with string cleaning specific to your site
    Facility_Name = str_trim(Facility_Name, side = "both"),
    Facility_Name = gsub(', Inc.', '', Facility_Name),
    Facility_Name = gsub(' Inc.', '', Facility_Name),
    Facility_Name = gsub(', LLC', '', Facility_Name),
    Facility_Name = gsub(' - EPIC', '', Facility_Name),
    Facility_Name = gsub(' EPIC', '', Facility_Name)) %>%
  distinct() %>%
  arrange(Facility_Name)


# MFT/Patient Class Filter
mft_filter <- "E"

# Limit MFT to ED Facilities
mft_ed <- mft %>%
  filter(Patient_Class_Code == mft_filter) %>%
  select(-Patient_Class_Code, -Facility_Type)

# Pull A08 Updates with Significant Delay - ED Encounters -----------------
updates_to_ED_encounters <- tbl(con, dbplyr::in_schema("dbo", paste0(site, '_PR_Processed'))) %>%
  select(
    C_Biosense_Facility_ID, C_Facility_ID, C_BioSense_ID, Visit_ID,
    C_Unique_Patient_ID, Message_ID, Message_Control_ID, Treating_Facility_ID, Sending_Facility_ID,
    Facility_Type_Code, C_FacType_Patient_Class, C_Patient_Class, Patient_Class_Code, Admit_Date_Time,
    Admit_Reason_Description, C_Chief_Complaint, Chief_Complaint_Text, C_Death, C_Patient_Age,
    C_Patient_Age_Years, Diagnosis_Code, Diagnosis_Description, Patient_Zip, Trigger_Event, Arrived_Date_Time,
    C_Visit_Date, C_Visit_Date_Time, Message_Date_Time, Recorded_Date_Time, Sending_Facility_ID_Source, Discharge_Disposition,
    Discharge_Date_Time, First_Patient_ID, Medical_Record_Number, Admit_Reason_Code, Chief_Complaint_Code,
    Diagnosis_Type, Administrative_Sex, Age_Reported, Age_Units_Reported, C_Patient_Age_Units, C_Patient_Age_Source,
    C_Patient_County, Ethnicity_Code, Ethnicity_Description, Patient_City, Patient_State, Patient_Country, Race_Code,
    Race_Description, Arrived_Date) %>%
  filter(C_Patient_Class == mft_filter) %>%
  filter(C_Visit_Date >= startDate) %>%
  mutate(
    C_Visit_Date = lubridate::as_date(C_Visit_Date),
    Arrived_Date = lubridate::as_date(Arrived_Date),
    diff = sql('DATEDIFF(day, C_Visit_Date, Arrived_Date)')
  ) %>%
  filter(diff >= extreme) %>%
  collect() %>%
  left_join(mft_ed, by = "C_Biosense_Facility_ID")

# Wrangle/Assess/Visualize
# You will probably want to tailor this to your use case and specific audience

old_ED_updates <- updates_to_ED_encounters %>%
  select(C_BioSense_ID, C_Biosense_Facility_ID, C_Chief_Complaint, Diagnosis_Code, Diagnosis_Description, Facility_Name, C_Visit_Date, C_Visit_Date_Time, Arrived_Date_Time,
         Arrived_Date) %>%
  mutate(
    lag = difftime(Arrived_Date, C_Visit_Date, units = "days")
  )

# Count number of affected encounters by hospital
delayed.EDmessages_by_facility <- old_ED_updates %>%
  group_by(C_Biosense_Facility_ID, Facility_Name) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

# Count number of affected encounters by hospital
delayed.EDencounters_by_facility <- old_ED_updates %>%
  group_by(C_Biosense_Facility_ID, Facility_Name) %>%
  summarize(n = n_distinct(C_BioSense_ID)) %>%
  ungroup() %>%
  arrange(desc(n))

# Mean Delay by hospital
mean_ED_message_delay <- old_ED_updates %>%
  group_by(C_Biosense_Facility_ID, Facility_Name) %>%
  summarize(mean_lag = mean(lag)) %>%
  ungroup() %>%
  arrange(desc(mean_lag))

library(reactable)
reactable(delayed.EDmessages_by_facility, defaultPageSize = 15)
reactable(delayed.EDencounters_by_facility, defaultPageSize = 15)
reactable(mean_ED_message_delay, defaultPageSize = 15)

# Delayed Messages by Date
ED_delays_by_date <- old_ED_updates %>%
  group_by(C_Visit_Date) %>%
  summarize(n = n()) %>%
  ungroup()

# Corresponding plot example
ggplot(ED_delays_by_date) +
  geom_line(aes(C_Visit_Date, n, color = "Delayed Messages")) +
  geom_point(aes(C_Visit_Date, n, color = "Delayed Messages"), size = 1.25) +
  labs(x = "C_Visit_Date",
       y = "Delayed Messages (n)",
       title = "Messages with Arrival Delay of >90 Days by C_Visit_Date") +
  scale_x_date(date_labels = "%b %d, %Y", expand = c(0, 0.1)) +
  scale_y_continuous() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Note:
# I would recommend assessing which message fields were updated, if any, between the previous message for each encounter
# and the most recent message received.
#
# If it was me, I would join on C_Biosense_ID in your site's PR_Processed table to pull older messages that
# refer to the relevant encounters you're interested in assessing.
#
# You could also pass a vector of C_Biosense_IDs to your site's PR_Processed table to pull all messages
# associated with those encounters and then compare the two most recent messages for differences.
#
# I'd suggest going through by hospital to limit processing time.


# Pull A08 Updates with Significant Delay - Inpatient Encounters ----------

# Note: The join to your facility names will only work if your inpatient data
# is sent through "Inpatient practice setting" ESSENCE feeds. In Kentucky,
# you'll need to keep the mft_filter set to "E."

# MFT/Patient Class Filter (to limit facilities included in join to record pull)
mft_filter <- "I"

mft_inp <- mft %>%
  filter(Patient_Class_Code == mft_filter) %>%
  select(-Patient_Class_Code, -Facility_Type)

updates_to_INP_encounters <- tbl(con, dbplyr::in_schema("dbo", paste0(site, '_PR_Processed'))) %>%
  select(
    C_Biosense_Facility_ID, C_Facility_ID, C_BioSense_ID, Visit_ID,
    C_Unique_Patient_ID, Message_ID, Message_Control_ID, Treating_Facility_ID, Sending_Facility_ID,
    Facility_Type_Code, C_FacType_Patient_Class, C_Patient_Class, Patient_Class_Code, Admit_Date_Time,
    Admit_Reason_Description, C_Chief_Complaint, Chief_Complaint_Text, C_Death, C_Patient_Age,
    C_Patient_Age_Years, Diagnosis_Code, Diagnosis_Description, Patient_Zip, Trigger_Event, Arrived_Date_Time,
    C_Visit_Date, C_Visit_Date_Time, Message_Date_Time, Recorded_Date_Time, Sending_Facility_ID_Source, Discharge_Disposition,
    Discharge_Date_Time, First_Patient_ID, Medical_Record_Number, Admit_Reason_Code, Chief_Complaint_Code,
    Diagnosis_Type, Administrative_Sex, Age_Reported, Age_Units_Reported, C_Patient_Age_Units, C_Patient_Age_Source,
    C_Patient_County, Ethnicity_Code, Ethnicity_Description, Patient_City, Patient_State, Patient_Country, Race_Code,
    Race_Description, Arrived_Date) %>%
  filter(C_Patient_Class == mft_filter) %>%
  filter(C_Visit_Date >= startDate) %>%
  mutate(
    C_Visit_Date = lubridate::as_date(C_Visit_Date),
    Arrived_Date = lubridate::as_date(Arrived_Date),
    diff = sql('DATEDIFF(day, C_Visit_Date, Arrived_Date)')
  ) %>%
  filter(diff >= extreme) %>%
  collect() %>%
  left_join(mft_inp, by = "C_Biosense_Facility_ID")

# Pull A08 Updates with Significant Delay for Outpatient Encounters

# MFT/Patient Class Filter (to limit facilities included in join to record pull)
mft_filter <- "O"

mft_out <- mft %>%
  filter(Patient_Class_Code == mft_filter)  %>%
  select(-Patient_Class_Code, -Facility_Type)

updates_to_OUTP_encounters <- tbl(con, dbplyr::in_schema("dbo", paste0(site, '_PR_Processed'))) %>%
  select(
    C_Biosense_Facility_ID, C_Facility_ID, C_BioSense_ID, Visit_ID,
    C_Unique_Patient_ID, Message_ID, Message_Control_ID, Treating_Facility_ID, Sending_Facility_ID,
    Facility_Type_Code, C_FacType_Patient_Class, C_Patient_Class, Patient_Class_Code, Admit_Date_Time,
    Admit_Reason_Description, C_Chief_Complaint, Chief_Complaint_Text, C_Death, C_Patient_Age,
    C_Patient_Age_Years, Diagnosis_Code, Diagnosis_Description, Patient_Zip, Trigger_Event, Arrived_Date_Time,
    C_Visit_Date, C_Visit_Date_Time, Message_Date_Time, Recorded_Date_Time, Sending_Facility_ID_Source, Discharge_Disposition,
    Discharge_Date_Time, First_Patient_ID, Medical_Record_Number, Admit_Reason_Code, Chief_Complaint_Code,
    Diagnosis_Type, Administrative_Sex, Age_Reported, Age_Units_Reported, C_Patient_Age_Units, C_Patient_Age_Source,
    C_Patient_County, Ethnicity_Code, Ethnicity_Description, Patient_City, Patient_State, Patient_Country, Race_Code,
    Race_Description, Arrived_Date) %>%
  filter(C_Patient_Class == mft_filter) %>%
  filter(C_Visit_Date >= startDate) %>%
  mutate(
    C_Visit_Date = lubridate::as_date(C_Visit_Date),
    Arrived_Date = lubridate::as_date(Arrived_Date),
    diff = sql('DATEDIFF(day, C_Visit_Date, Arrived_Date)')
  ) %>%
  filter(diff >= extreme) %>%
  collect() %>%
  left_join(mft_out, by = "C_Biosense_Facility_ID")
