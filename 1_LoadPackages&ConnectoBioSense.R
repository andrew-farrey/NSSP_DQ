# DQ Subcommittee R Walkthrough #1, Load R Packages and Connect to BioSense---------------------------------------------
# How to identify various DQ issues in BioSense/ESSENCE Syndromic Surveillance Data

# I would suggest tailoring the variables you monitor for data quality to the type of surveillance work you do.
# I'm primarily concerned with chief complaint and diagnosis code timeliness/completeness/validity because I
# monitor drug overdoses, and have built an alert system running off of syndromic data. If you monitor for other 
# syndromes or are concerned with more general data quality, you might be more concerned with completeness of age, 
# race, ethnicity, or general message parsing errors, for instance. 

# Necessary Packages ------------------------------------------------------
# I strongly recommend using the pacman package for package management. 

if (!require("pacman")) install.packages("pacman")

# The require statement should install and/or load it,
# then for every subsequent R script or .Rmd file you ever create, you can
# simply call "pacman::p_load()" with your list of packages inside the parentheses

pacman::p_load(tidyverse, lubridate, RODBC, odbc, DBI, sjmisc, Rnssp)

# Load Rnssp profile from current working directory
# If you don't have your AMC (NSSP) login saved, you'll need to create your profile object and save it for future use.
# Uncomment this code to do so
# myProfile <- Credentials$new(
# username = askme("Enter your username: "),
# password = askme())
# saveRDS("myProfile.rds")

myProfile <- readRDS("myProfile.rds")


# Connecting to BioSense --------------------------------------------------

# There are two ways to connect to BioSense that I use (I'm sure there are others).
 
# The first method uses the odbc library and is the suggested connection method in all NSSP
# documentation. This method requires you to be online to run the code. As far as I know, this connection method cannot be scheduled to run  
# (though it may be possible after editing the connection parameters. I've not tested this. CORRECTION: I HAVE TESTED THIS). I use this method when I'm physically using the 
# NSSP RStudio Workbench to investigate data quality issues and the like.

con <- dbConnect(odbc::odbc(), dsn = "BioSense_Platform")

# To connect to BioSense in a scheduled R script or Rmarkdown document (e.g, you will not physically run the script yourself, 
# you'll need to connect to BioSense using the RODBC package. 

# You can use keyring to hide your password, load a source file with your username and password and paste0() it in place,
# or leave your AMC username and password in your code (not recommended).

con <- RODBC::odbcConnect("BioSense_Platform", "BIOSENSE\\username", "password")

# I tested dbConnect() with a username and password. It still cannot be used to run scheduled scripts/reports while offline, even with those 
# with those parameters included.

con <- dbConnect(odbc::odbc(), dsn = "BioSense_Platform", user = "BIOSENSE\\username", password = "password")

# RODBC data pulls are noticeably slower, so with that in mind, I'd use DBI/odbc instead of RODBC.
# The practical difference between the two methods is a different function is required to query BioSense.

# Using odbc requires using the DBI::dbGetQuery() function.
# Using RODBC requires using RODBC::sqlQuery()

# The two functions are functionally identical, in that you refer to your SQL connection and then your SQL query text,
# which can either be pasted directly, or assigned to another variable name (e.g., "query")

# Do not uncomment the code below, as it is just for use as an example.
# 
#  EDvisits <- DBI::dbGetQuery(con, "Select * from XX_PR_Processed with (nolock)...")
#  EDvisits <- RODBC::sqlQuery(con, "Select * from XX_PR_Processed with (nolock)...")
#  
#  OR 
#  
#  query <- "Select * from XX_PR_Processed with (nolock)..."
#  
#  EDvisits <- DBI::dbGetQuery(con, query)
#  EDvisits <- RODBC::sqlQuery(con, query)
