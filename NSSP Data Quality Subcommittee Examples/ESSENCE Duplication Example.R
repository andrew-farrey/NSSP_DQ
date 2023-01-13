# De-Duplicating by Visit ID using the ESSENCE API

pacman::p_load(tidyverse, lubridate, data.table, kableExtra, Rnssp)

# Create Rnssp Credentials (or load them, if you already have them saved)
myProfile <- Credentials$new(askme("Enter my username: "), askme())

#saveRDS(myProfile, "myProfile.rds")
#myProfile <- readRDS("myprofile.rds")

# add facilities to limit to, if desired
fac <- ""

# Site geography of your site, to limit to state residents
site <- "ky"

# CCDD definition to search for
syndrome <- "cdc%20opioid%20overdose%20v3"

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=31Mar2022&geography=",site,"&percentParam=noPercent&datasource=va_er&startDate=1Jan2019",fac,"&medicalGroupingSystem=essencesyndromes&userId=2765&hospFacilityType=emergency%20care&aqtTarget=DataDetails&ccddCategory=",syndrome,"&geographySystem=state&detector=probrepswitch&timeResolution=daily&hasBeenE=1")

county_pull <- myProfile$get_api_data(url) %>% 
  pluck("dataDetails")

# Pull encounters that have more than one encounter per Visit_ID
visit_ids <- county_pull %>% 
  group_by(Visit_ID) %>% 
  count() %>% 
  filter(n > 1) %>% 
  select(Visit_ID) %>% 
  pull()

dups <- county_pull %>% 
  filter(Visit_ID %in% visit_ids) %>% 
  arrange()

# Filter to first row, to limit to one encounter per unique Visit_ID (so each patient encounter is counted once)
county_data <- county_pull  %>% 
  group_by(Visit_ID) %>% 
  filter(row_number() == 1) %>%
  ungroup()