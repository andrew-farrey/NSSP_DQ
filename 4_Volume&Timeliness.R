# DQ Subcommittee R Walkthrough #4, Volume & Timeliness Examples---------------------------------------------

# This assumes you have loaded the relevant R packages and are already connected to BioSense.
# If you aren't, you'll to do so again, so uncomment the below code.
# con <- dbConnect(odbc::odbc(), dsn = "BioSense_Platform")
# 

# Total ED Patient Volume is easiest to measure using the ESSENCE API
# Make sure your Rnssp profile is loaded first
# 
# Replace "site" with your site's abbreviation

startDate <- format(Sys.Date() - 7, "%d%b%Y")
endDate <- format(Sys.Date() - 1, "%d%b%Y")
site <- "ky"

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder?endDate=",endDate,
              "&geography=",site,"&percentParam=noPercent&datasource=va_hosp&startDate=",startDate,
              "&medicalGroupingSystem=essencesyndromes&userId=2765&hospFacilityType=emergency%20care&aqtTarget=TableBuilder&geographySystem=hospitalstate&detector=nodetectordetector&timeResolution=daily&hasBeenE=1&rowFields=timeResolution&columnField=hospitalGrouping")

facility.volume <- myProfile$get_api_data(url, fromCSV = FALSE) %>% 
  dplyr::select(date = timeResolution,
                hospital = hospitalGrouping,
                encounters = count) %>% 
  filter(grepl('KY-', hospital), encounters > 0) %>% 
  mutate(date = as.Date(date),
         hospital = gsub("^.{0,3}", "", hospital),
         hospital = str_trim(hospital, side = "both"))

facilities <- facility.volume %>% 
  group_by(date) %>% 
  summarize(hospitals = n())  %>% 
  ungroup()

# Plotly Plot of Total Facilities by Day

ab <- list(
  title = "Unique ED Facilities",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = TRUE,
  showgrid = TRUE,
  rangemode = "tozero"
)

plotly::plot_ly(facilities, 
        x = ~date, 
        y = ~hospitals, 
        type = "scatter",
        mode = 'lines+markers'
) %>% 
  plotly::layout(title = "Syndromic ED Facilities Online by Day, Last Seven Days",
                 yaxis = ab,
                 xaxis = list(title = 'Date'),
                 showlegend = F)

volume <- facility.volume %>% 
  group_by(date) %>% 
  summarize('Total Visits' = sum(encounters)) %>% 
  ungroup()

# Plotly plot of Total ED Visits by Day

ax <- list(
  title = "Number of Visits",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = TRUE,
  showgrid = TRUE,
  range = c(0,10000)
)

plotly::plot_ly(volume,
        x = ~date,
        y = ~`Total Visits`,
        name = 'Total Visits',
        type = "scatter",
        mode = 'lines+markers', 
        line = list(color = "green"),
        marker = list(color = "green")) %>%
  plotly::layout(title = "Total Syndromic ED Visits \nLast Seven Days",
                 yaxis = ax,
                 xaxis = list(title = 'Date')) 









# Timeliness is one of the more difficult things to analyze in RStudio because ESSENCE isn't able to help much here.

# NSSP has stipulated that the first message from every emergency patient encounter should be 
# received within 24 hours (80% site-wide, at a minimum), and classifies messages into one of three timeliness categories:

# < 24 Hours - first encounter message received less than 24 hours after patient encounter
# 24-48 Hours - first encounter message received at least 24, but less than 48 hours after patient encounter
# > 48 Hours - first encounter message received more than 48 hours after patient encounter

# So we need to get the earliest Arrived_Date_Time per unique patient encounter (C_Biosense_ID), and compare that to when the 
# encounter physically occurred, which is C_Visit_Date_Time (usually populated from Admit_Date_Time)

# Before we can calculate first message lag/delay, we need to adjust Arrived_Date_Time by however many hours your timezone is 
# offset from UTC/GMT. 

# C_Visit_Date_Time is submitted in your jurisdiction's time zone, while Arrived_Date_Time is formatted in UTC (or GMT + 0). 
# In order to calculate timeliness accurately, we need to adjust Arrived_Date_Time accordingly,
# or we'd be unfairly penalizing every message by 3 - 9 hours (depending on daylight savings time and time zone). 

# You can plug your time zone into the "tz = " slot below. I'm in Eastern time, so my time zone in R is "America/New_York." 
# If you were in Central time for instance, you'd put "America/Chicago." There should be other names that work (e.g., "America/Eastern")

# Note that the NSSP RStudio workbench has a time zone of UTC, so you can't use Sys.timezone() to obtain your own time zone name. 
# However, you can plug your location information into https://timezonedb.com/ to obtain the correct time zone name you'd need to use, 
# or any other method to obtain a list of tz database time zone names. 

offset <-
  abs(as.integer(
    as.POSIXct(as.character(Sys.time()), tz = "UTC") - as.POSIXct(as.character(Sys.time()), tz = "America/New_York")
  ))

# I typically apply the time adjustment to the entire ED dataset before slicing() so I don't have to do it again.

ED <- ED %>%
  mutate(mutate(Arrived_Date_Time = Arrived_Date_Time - lubridate::hours(paste0(offset))))

# Next, create a subset of ED messages with the first message received per patient

ED_timeliness_subset <- ED %>% 
  group_by(C_Biosense_ID) %>% 
  slice(which.min(Arrived_Date_Time)) %>% 
  slice(1) %>% 
  ungroup()

# Then calculate overall, or "first message" timeliness

Lag_patient <- ED_timeliness_subset %>%
  group_by(C_Biosense_ID) %>%
  # Find first message lag (calculate overall "timeliness") and round that value to 2 digits
  mutate(lag = round(as.numeric(
    difftime(Arrived_Date_Time, C_Visit_Date_Time, units = "hours")
  )), 2) %>%
  # Create NSSP timeliness categories
  mutate(TimelinessCategory = cut(
    lag,
    breaks = c(-Inf, 24, 48, Inf),
    labels = c("<24 Hours", "24-48 Hours", ">48 Hours")
  )) %>%
  ungroup()

# Calculate mean and median overall timeliness by facility
Lag_facility <- Lag_patient %>%
  group_by(Facility_Name) %>%
  summarize(mean.lag = round(mean(lag), 2),
            median.lag = round(median(lag), 2)) %>%
  ungroup()

# Check that results look reasonable
prop.table(table(Lag_patient$TimelinessCategory))

# Set up data for your plot
lagtable <- data.frame(table(Lag_patient$Facility_Name, Lag_patient$TimelinessCategory), stringsAsFactors = F)
colnames(lagtable) <- c("Facility","Timeliness Category", "Total")

# Reorder to display facility names alphabetically
lagtable$Facility <-
  factor(lagtable$Facility, levels = unique(lagtable$Facility)[order(lagtable$Facility, decreasing = TRUE)])

# Pivot your data wider for display in a plotly plot
delay_graph <- data.frame(
  lagtable %>%
    pivot_wider(names_from = `Timeliness Category`,
                values_from = Total),
  stringsAsFactors = F
)
# Fix the column names
colnames(delay_graph) <- c("Facility Name","<24 Hours", "24-48 Hours",">48 Hours")

# Set up plot attributes
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = TRUE,
  showgrid = FALSE
)

# create timeliness plot
plotly::plot_ly(
  delay_graph,
  y = ~ `Facility Name`,
  x = ~ `<24 Hours`,
  type = 'bar',
  name = '<24 Hours',
  marker = list(color = 'green')
) %>%
  plotly::add_trace(
    x = ~ `24-48 Hours`,
    name = '24-48 Hours',
    marker = list(color = "blue")
  ) %>%
  plotly::add_trace(
    x = ~ `>48 Hours`,
    name = '>48 Hours',
    marker = list(color = "red")
  ) %>%
  plotly::layout(
    yaxis = ax,
    xaxis = list(title = 'Counts by Timeliness Category'),
    barmode = 'stack'
  )

# This is just first message, or overall timeliness. You may also find it helpful to calculate chief complaint lag 
# and/or diagnosis code lag, especially if you have facilities that take their sweet time submitting completed diagnosis codes.