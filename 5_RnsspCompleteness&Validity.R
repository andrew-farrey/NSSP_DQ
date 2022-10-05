# DQ Subcommittee R Walkthrough #5, Rnssp Completeness & Validity---------------------------------------------

# This assumes you have loaded the relevant R packages and have loaded or created your AMC/NSSP profile object.

# I have to limit to certain ED facilities in Kentucky, but I've commented out my list
fac <- ""
#fac <- "&geography=24104&geography=15413&geography=15416&geography=15417&geography=15437&geography=15439&geography=15440&geography=15442&geography=15498&geography=15526&geography=15528&geography=15360&geography=15412&geography=15361&geography=15362&geography=15363&geography=15364&geography=15365&geography=15366&geography=23866&geography=23850&geography=15376&geography=15377&geography=23885&geography=23825&geography=15392&geography=15403&geography=23898&geography=17898&geography=15406&geography=15409&geography=15411&geography=15415&geography=19186&geography=15419&geography=19182&geography=15420&geography=15427&geography=15429&geography=19187&geography=15433&geography=23896&geography=15434&geography=15435&geography=19188&geography=28130&geography=23833&geography=15441&geography=15449&geography=15456&geography=15457&geography=15430&geography=15431&geography=15465&geography=15469&geography=15488&geography=15489&geography=15490&geography=15493&geography=19189&geography=15496&geography=15497&geography=15499&geography=15500&geography=15504&geography=15506&geography=15508&geography=19824&geography=15511&geography=15512&geography=15515&geography=15517&geography=15514&geography=15384&geography=23847&geography=15518&geography=15520&geography=15521&geography=15523&geography=15525&geography=15405&geography=15494&geography=33892"

# Start and End Dates formatted for ESSENCE API
startDate <- format(Sys.Date() - 10, "%d%b%Y")
endDate <- format(Sys.Date() - 2, "%d%b%Y")

# Adjust with your site abbreviation
state <- "ky"

# Chief Complaint Available (Site-wide) -----------------------------------
url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate, fac,"&percentParam=ccAvailable&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ccAvailable=1&timeResolution=daily&hasBeenE=1")

cc.available <- myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

# Simple ggplot to show site-wide CCAvailable
ggplot(cc.available) +
  geom_line(aes(date, count, color = "CCAvailable = 1")) +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Chief Complaint Completeness (ED Visits)") +
  scale_x_date(date_labels = "%b %d", expand = c(0, 0.1)) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(8),
    labels = scales::label_percent(accuracy = 1,
                                   scale = 1)
  ) +
  scale_color_manual(values = c("CCAvailable = 1" = "Blue")) +
  expand_limits(y = 0) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Chief Complaint Available by Facility (ED) ------------------------------

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate,fac,"&percentParam=ccAvailable&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ccAvailable=1&timeResolution=daily&hasBeenE=1&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=true&startMonth=January&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false")

cc.available.facility <-
  myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

cc.facility <- cc.available.facility %>%
  dplyr::select(date,
                pct = count,
                hospital = hospital_display) %>%
  mutate(
    hospital = gsub("^.{0,3}", "", hospital),
    alert = case_when(pct < 90 ~ "Alert",
                      pct > 90 ~ "None",
                      TRUE ~ as.character(pct))
  )

# If you have a bunch of facilities, you'll need to "Zoom" in on the plot and expand it to fit your screen
# The red points alert you to low chief complaint completeness for a particular hospital and day

ggplot(cc.facility, aes(date, pct)) +
  geom_line() +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Percentage of ED Visits with Chief Complaint Available (ED Only)") +
  scale_x_date(date_labels = "%b %d",
               expand = c(0, 0.1)) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_percent(accuracy = 1, scale = 1)) +
  facet_wrap( ~ hospital) +
  geom_point(data = subset(cc.facility, alert == "Alert"),
             color = "red") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 1),
    strip.text = element_text(size = 8)
  )

# Discharge Diagnosis Available (Site-wide) ------------------------------------

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate,fac,"&percentParam=ddAvailable&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ddAvailable=1&timeResolution=daily&hasBeenE=1")

dd.available <- myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

ggplot(dd.available) +
  geom_line(aes(date, count, color = "DDAvailable = 1")) +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Discharge Diagnosis Completeness (ED)") +
  scale_x_date(date_labels = "%b %d",
               expand = c(0, 0.1)) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = scales::pretty_breaks(8),
    labels = scales::label_percent(accuracy = 1,
                                   scale = 1)
  ) +
  scale_color_manual(values = c("DDAvailable = 1" = "Blue")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
    

# Discharge Diagnosis Available (by Facility) -----------------------------

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate, fac,"&percentParam=ddAvailable&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ddAvailable=1&timeResolution=daily&hasBeenE=1&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=true&startMonth=January&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false")

dd.available.facility <-
  myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

dd.facility <- dd.available.facility %>%
  dplyr::select(date,
                pct = count,
                hospital = hospital_display) %>%
  mutate(
    hospital = gsub("^.{0,3}", "", hospital),
    alert = case_when(pct < 90 ~ "Alert",
                      pct > 90 ~ "None",
                      TRUE ~ as.character(pct))
  )

# If you have a bunch of facilities, you'll need to "Zoom" in on the plot and expand it to fit your screen
# The red points alert you to low chief discharge diagnosis for a particular hospital and day

ggplot(dd.facility, aes(date, pct)) +
  geom_line() +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Percentage of ED Visits with Discharge Diagnosis (Code(s)) Available (ED Only)") +
  scale_x_date(date_labels = "%b %d",
               expand = c(0, 0.1)) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_percent(accuracy = 1, scale = 1)) +
  facet_wrap( ~ hospital) +
  geom_point(data = subset(dd.facility, alert == "Alert"),
             color = "red") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 1),
    strip.text = element_text(size = 8)
  )

# Chief Complaint Informative (Site-wide) ---------------------------------

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate, fac,"&percentParam=ccInformative&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ccInformative=1&timeResolution=daily&hasBeenE=1")

cc.informative <- myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

cc.informative <- cc.informative %>%
  select(date,
         pct = count) %>%
  mutate(alert = case_when(pct < 90 ~ "Alert",
                           pct > 90 ~ "None",
                           TRUE ~ as.character(pct)))

ggplot(cc.informative, aes(date, pct, color = "CCInformative = 1")) +
  geom_line() +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Chief Complaint Informative (ED Only)") +
  scale_x_date(date_labels = "%b %d",
               expand = c(0, 0.1)) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = scales::label_percent(accuracy = 1, scale = 1)) +
  scale_color_manual(values = c("CCInformative = 1" = "Blue")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Chief Complaint Informative (by Facility) -------------------------------

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate, fac,"&percentParam=ccInformative&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ccInformative=1&timeResolution=daily&hasBeenE=1&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=true&startMonth=January&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false")

cc.informative.facility <-
  myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

cc.inform.facility <- cc.informative.facility %>%
  dplyr::select(date,
                pct = count,
                hospital = hospital_display) %>%
  mutate(
    hospital = gsub("^.{0,3}", "", hospital),
    alert = case_when(pct < 90 ~ "Alert",
                      pct > 90 ~ "None",
                      TRUE ~ as.character(pct))
  )

ggplot(cc.inform.facility, aes(date, pct)) +
  geom_line() +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Chief Complaint Informative by Facility (ED Only)") +
  scale_x_date() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap( ~ hospital) +
  geom_point(data = subset(cc.inform.facility, alert == "Alert"),
             color = "red") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 1),
    strip.text = element_text(size = 8)
  )

# Discharge Diagnosis Informative (Site-wide) -----------------------------

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate, fac,"&percentParam=ddInformative&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ddInformative=1&timeResolution=daily&hasBeenE=1")

dd.informative <- myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

dd.informative <- dd.informative %>%
  select(date,
         pct = count) %>%
  mutate(alert = case_when(pct < 90 ~ "Alert",
                           pct > 90 ~ "None",
                           TRUE ~ as.character(pct)))

ggplot(dd.informative, aes(date, pct, color = "DDInformative = 1")) +
  geom_line() +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Discharge Diagnosis Informative (ED)") +
  scale_x_date(date_labels = "%b %d",
               expand = c(0, 0.1)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::label_percent(accuracy = 1, scale = 1)) +
  scale_color_manual(values = c("DDInformative = 1" = "Blue")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Discharge Diagnosis Informative (by Facility) ---------------------------

url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=",endDate, fac,"&percentParam=ddInformative&datasource=va_hosp&startDate=",startDate,"&medicalGroupingSystem=essencesyndromes&userId=2765&site=895&hospFacilityType=emergency%20care&aqtTarget=TimeSeries&geographySystem=hospital&detector=probrepswitch&ddInformative=1&timeResolution=daily&hasBeenE=1&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=true&startMonth=January&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=0&graphOptions=multipleSmall&seriesPerYear=false&startMonth=January&nonZeroComposite=false")

dd.informative.facility <-
  myProfile$get_api_data(url, fromCSV = FALSE) %>%
  pluck("timeSeriesData") %>%
  mutate(date = as.Date(date))

dd.inform.facility <- dd.informative.facility %>%
  select(date,
         pct = count,
         hospital = hospital_display) %>%
  mutate(
    hospital = gsub("^.{0,3}", "", hospital),
    alert = case_when(pct < 90 ~ "Alert",
                      pct > 90 ~ "None",
                      TRUE ~ as.character(pct))
  )

ggplot(dd.inform.facility, aes(date, pct)) +
  geom_line() +
  labs(x = "Date",
       y = "Percent of ED Visits",
       title = "Informative Discharge Diagnosis\nPercentage by Facility (ED Only)") +
  scale_x_date(date_labels = "%b %d",
               expand = c(0, 0.1)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::label_percent(accuracy = 1, scale = 1)) +
  facet_wrap( ~ hospital) +
  geom_point(data = subset(dd.inform.facility, alert == "Alert"),
             color = "red") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                size = 1),
    strip.text = element_text(size = 8)
  )

