# DQ Subcommittee R Walkthrough #2, Pull MFT and Operational Crosswalk---------------------------------------------

# This assumes you have loaded the relevant R packages and are already connected to BioSense from script #1.
# If you aren't, you'll to do so again, so uncomment the below code.
# con <- dbConnect(odbc::odbc(), dsn = "BioSense_Platform")

# Let's start by pulling your state's Master Facility Table and joining it to your state's Operational Crosswalk

# Replace "KY" with your site abbreviation

site <- "KY" 

MFT <- DBI::dbGetQuery(con, paste0("SELECT Facility_Name, C_Facility_ID, C_Biosense_Facility_ID, FacilityID_UUID, Facility_Type, Facility_Type_Code,
                  Facility_Status, Patient_Class_Code, Date_Onboarded, Date_Activated, Date_Last_Modified from ",site,"_MFT where Facility_Status='Active'"))

# This pulls all active facilities. I never use this in Kentucky, as ~2/3 of our 334 providers are outpatient offices.

# If you aren't very familiar with SQL, you might find it helpful to filter down to the facilities 
# you want using the pipe (%>%) operator inline following your initial SQL pull. I separated this example to use the paste0()
# query method I mentioned in the first script. 

query <- paste0("SELECT Facility_Name, C_Facility_ID, C_Biosense_Facility_ID, FacilityID_UUID, Facility_Type, Facility_Type_Code,
                  Facility_Status, Patient_Class_Code, Date_Onboarded, Date_Activated, Date_Last_Modified FROM ",site,"_MFT")

MFT <- DBI::dbGetQuery(con, query) %>% 
  filter(Facility_Status == "Active" & Patient_Class_Code == "E") %>% 
  distinct() %>% 
  mutate(Facility_Name = str_trim(Facility_Name, side = "both")) 

# Alternatively, you can include most of the necessary filters using SQL syntax within your initial query itself. Either way works,
# but filtering inside your SQL query will be faster. When filtering patient encounter data, be sure to include "with (nolock)", in any
# computationally-intensive SQL queries, otherwise your query will fail if anyone else queries BioSense while your query is running.

query <- paste0("SELECT Facility_Name, C_Facility_ID, C_Biosense_Facility_ID, FacilityID_UUID, Facility_Type, Facility_Type_Code,
                  Facility_Status, Patient_Class_Code, Date_Onboarded, Date_Activated, Date_Last_Modified FROM ",site,"_MFT 
                  WHERE Patient_Class_Code='E' AND Facility_Status='Active'")

MFT <- DBI::dbGetQuery(con, query) %>% 
  distinct() %>% 
  mutate(Facility_Name = str_trim(Facility_Name, side = "both")) %>% 
  filter(!Facility_Name == "Baptist Health Richmond, Inc.")

# Next, we need to pull the Operational Crosswalk. 

Crosswalk <- dbGetQuery(con, paste0("SELECT C_Biosense_Facility_ID FROM ",site,"_Operational_Crosswalk WHERE Facility_Status='Active' 
                                    AND Patient_Class_Code='E'")) %>% 
  distinct()

# Technically, NSSP joins the C_Facility_ID in HL7 messages to active Operational Crosswalk facilities' Output_FacilityID_UUIDs, 
# but I typically join on C_Biosense_Facility_ID for the sake of convenience and simplicity. Kentucky has several hospitals that send 2, 3, 
# or even 6 different hospitals through one individual feed, and as such, all share the same C_Biosense_Facility_ID so it has never been an issue
# for me. 

# This method mirrors the hospital names I can see in ESSENCE, but if your site has onboarded facilities differently, you may need to
# join on C_Facility_ID and Output_FacilityID_UUID.

# I'm going to select only the variables we need to join the facility names from the MFT table to the active facilities in the operational 
# crosswalk, then join the facility names to active operational crosswalk facilities. 

# The end result will give us active, emergency department-equipped hospitals with names.

names <- MFT %>% 
  select(Facility_Name, C_Biosense_Facility_ID) %>% 
  left_join(Crosswalk) %>% 
  distinct()
