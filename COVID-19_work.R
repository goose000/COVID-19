
#Environment cleanup
rm(list=ls())

#Install Packages if needed:
pkgs <- c(
  "dplyr", # package for data manipulation
  "ggplot2",## Libraries for Plotting our Results
  "gridExtra",## Libraries for Plotting our Results
  "tidyverse", # Library for data cleaning
  "caret",## Library for preprocessing, train, confusion matrix, and many other functions
  "lubridate",
  "plotly",
  "tidyr",
  "leaflet",
  "httr",
  "readr",
  "pracma"
  )
missingpkgs <- lapply(pkgs, require, character.only = TRUE)
missingpkgs <- unlist(missingpkgs)
if (sum(!missingpkgs)) {
  install.packages(pkgs[!missingpkgs])
  lapply(pkgs[!missingpkgs], library, character.only = TRUE)
}


#Store URLs
directory <- "~/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-"
 #paste0(directory,"Confirmed.csv")
global_confirmed_URL <-  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
#paste0(directory,"Deaths.csv")
global_deaths_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
#paste0(directory,"Recovered.csv")
global_recovered_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

US_confirmed_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"


US_deaths_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

#Open Connections
globalConfirmedCon <- url(global_confirmed_URL)
globalDeathsCon <- url(global_deaths_URL)
globalRecoveredCon <- url(global_recovered_URL)
USConfirmedCon <- url(US_confirmed_URL)
USDeathsCon <- url(US_deaths_URL)

#Load Data
confirmedCases <- read_csv(globalConfirmedCon)
recovered <- read_csv(globalDeathsCon)
deaths <- read_csv(globalRecoveredCon)
USConfirmedCases <- read_csv(USConfirmedCon)
USDeaths <- read_csv(USDeathsCon)

#Close Connections
close(globalConfirmedCon, globalDeathsCon, globalRecoveredCon, USConfirmedCon, USDeathsCon)

#Reshape the dataframe, clean up names, reorder variables and observations
tidyConfirmedCases <- confirmedCases %>%
        rename(provinceState = `Province/State`, countryRegion = `Country/Region`) %>%
        pivot_longer(-c(provinceState, countryRegion, Lat, Long), 
                     names_to = "Date", values_to = "cumulativeCases") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion,provinceState)

#Repeat Everything for Deaths and Recovered, and US Data
tidyDeaths <- deaths %>%
        rename(provinceState = `Province/State`, countryRegion = `Country/Region`) %>%
        pivot_longer(-c(provinceState, countryRegion, Lat, Long), 
                     names_to = "Date", values_to = "deaths") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion,provinceState)

tidyRecovered <- recovered %>%
        rename(provinceState = `Province/State`, countryRegion = `Country/Region`) %>%
        pivot_longer(-c(provinceState, countryRegion, Lat, Long), 
                     names_to = "Date", values_to = "recovered") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion,provinceState)

tidyUSConfirmed <- USConfirmedCases %>%
        rename(county = Admin2, provinceState = Province_State, 
               countryRegion = Country_Region, Long = Long_, combinedKey = Combined_Key) %>%
        select(-c(UID, iso3, code3, FIPS)) %>%
        pivot_longer(-c(iso2, county, provinceState, countryRegion, Lat, Long, combinedKey), 
                     names_to = "Date", values_to = "cumulativeCases") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, provinceState, county, everything())%>%
        arrange(Date, countryRegion, provinceState, county)
        
tidyUSDeaths <- USDeaths %>%
        rename(county = Admin2, provinceState = Province_State, 
               countryRegion = Country_Region, Long = Long_, combinedKey = Combined_Key) %>%
        select(-c(UID, iso3, code3, FIPS)) %>%
        pivot_longer(-c(iso2, county, provinceState, countryRegion, Lat, Long, 
                        combinedKey, Population), 
                     names_to = "Date", values_to = "deaths") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, provinceState, county, everything())%>%
        arrange(Date, countryRegion, provinceState, county)

#Merge US Dataframes
usDF <- tidyUSConfirmed %>%
        merge(tidyUSDeaths, all = T)

#Merge Global Dataframes
globalDF <- tidyConfirmedCases %>%
        merge(tidyDeaths) %>%
        merge(tidyRecovered, all = T) %>%
        unite(combinedKey, provinceState, countryRegion, sep = ", ", na.rm = T) %>%
        merge(usDF, all = T) %>%
        group_by(combinedKey)
        

#identify countries without provinces
noProvinces <- is.na(globalDF$provinceState)

#Replace empty province entries with country names       
globalDF$provinceState[noProvinces]<-globalDF$countryRegion[noProvinces]
       
#Replace NA with previous values
naRows <- is.na(globalDF$recovered)
globalDF$recovered[naRows] <- 0#lag(globalDF$recovered, default = 0)[naRows]        


#Convert Province and Country to factor
globalDF$provinceState <- globalDF$provinceState %>%
        as.factor()
globalDF$countryRegion <- globalDF$countryRegion %>%
        as.factor()

#Remove missing data (important for when case numbers have not yet updated)
#globalDF <- globalDF[complete.cases(globalDF),]

#Create Active Cases Variable
globalDF <- globalDF %>%
        mutate(activeCases = cumulativeCases - recovered - deaths)

#Create New Cases Variable
globalDF <- globalDF %>%
        mutate(newCases = cumulativeCases - lag(cumulativeCases, default = 0))

#Create Growth Variable
globalDF <- globalDF %>%
        mutate(caseGrowth = ifelse(activeCases>0,(newCases-deaths-recovered)/activeCases,0))

#Create a smoothed Growth Variable
globalDF <- globalDF %>%
        mutate(smoothGrowth = movavg(caseGrowth,5,"e"))

#Create a factor for the level of growth
globalDF <- globalDF %>%
        mutate(growthCat = cut(smoothGrowth, breaks = c(-Inf, 0, .05, .1, .2, .25, Inf), 
                                 labels = c("None/Decay", "Low", "M-Low", "Medium", "M-High", "High"),
                               ordered_result = T))

#Identify cumulative cases as of most recent download
current <- max(globalDF$Date)
currentCumulativeTotal <- filter(globalDF,Date==current)
pal <- colorFactor(palette = c("green", "yellowgreen", "yellow", "orange", "red", "red4"), 
                   domain= currentCumulativeTotal$growthCat)

#Create interactive map

currentCumulativeTotal %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
                weight = 1,
                color = ~pal(growthCat),
                radius = 2*log(currentCumulativeTotal$cumulativeCases),
                #clusterOptions = markerClusterOptions(), 
                popup = paste(currentCumulativeTotal$provinceState, "<br>",
                          "Confirmed Cases:",currentCumulativeTotal$cumulativeCases, "<br>",
                          "Deaths:",currentCumulativeTotal$deaths, "<br>",
                          "Recovered:",currentCumulativeTotal$recovered
                          )
                )# %>%

#Code for adding a date to the map
#        addCircleMarkers(lat = 75, lng = -120, label = today(),
#                   markerOptions(opacity = .001, radius = .01),
#                   labelOptions = labelOptions(noHide = T, textsize = "12px"))

#Aggregate Data
nationalCases <- globalDF %>%
        group_by(countryRegion,Date)%>%
        summarise(
          cumulativeCases = sum(cumulativeCases),
          activeCases = sum(activeCases)
        )%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion)

#Create a Growth Chart
fig <- plot_ly(nationalCases, x = ~Date,
        y = ~cumulativeCases, 
        color = ~countryRegion,
        type = "scatter",
        mode = "lines"
        )%>%
        layout(title = 'Confirmed Cases of COVID-19 per Country Over Time')
fig 
        

#Create an Active Cases Chart


fig <- plot_ly(nationalCases, x = ~Date,
               y = ~activeCases, 
               color = ~countryRegion,
               type = "scatter",
               mode = "lines"
)%>%
        layout(title = 'Active Cases of COVID-19 per Country Over Time')
fig 

#Create a Growth Chart
fig <- plot_ly(globalDF, x = ~Date,
               y = ~smoothGrowth, 
               color = ~provinceState,
               type = "scatter",
               mode = "lines"
)%>%
        layout(title = 'Growth in Cases of COVID-19 per Country Over Time')
fig 

      