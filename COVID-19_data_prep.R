
#Environment cleanup
rm(list=ls())

#Install Packages if needed:
pkgs <- c(
  "tidyverse", # package for data manipulation
  "ggplot2",## Libraries for Plotting our Results
  "gridExtra",## Libraries for Plotting our Results
  "caret",## Library for preprocessing, train, confusion matrix, and many other functions
  "lubridate",
  "plotly",
  "leaflet",
  "httr",
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
deaths <- read_csv(globalDeathsCon)
recovered <- read_csv(globalRecoveredCon)
USConfirmedCases <- read_csv(USConfirmedCon)
USDeaths <- read_csv(USDeathsCon)

#Close Connections
#close(globalConfirmedCon)
#close(globalDeathsCon)
#close(globalRecoveredCon)
#close(USConfirmedCon)
#close(USDeathsCon)

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

#Pull off US recovered numbers for later use
USRecovered <- tidyRecovered %>%
        filter(countryRegion == 'US') %>%
        select(recovered)

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
        unite(combinedKey, provinceState, countryRegion, sep = ", ", remove = F, na.rm = T) %>%
        filter(countryRegion !='US') %>%
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

#Feature Creation
globalDF <- globalDF %>%
        mutate(mortalityRate = ifelse(lag(cumulativeCases, k=3, default = 0)>0,
                                      (deaths/lag(cumulativeCases, k=3, default = 0)),0)) %>%
        mutate(activeCases = cumulativeCases - recovered - deaths) %>%#Create Active Cases Variable
        mutate(yesterdayCumulative = lag(cumulativeCases, default = 0)) %>%
        mutate(yesterdayActive = lag(activeCases, default = 0)) %>%
        mutate(newCases = cumulativeCases-yesterdayCumulative) %>% #Create New Cases Variable
        mutate(caseGrowth = ifelse(yesterdayActive>0,(activeCases - yesterdayActive)/yesterdayActive,0))  %>% #Create Growth Variable
        mutate(smoothGrowth = movavg(caseGrowth,5,"e"))%>% #Use a moving average to create a smoothed Growth Variable
        mutate(growthCat = cut(smoothGrowth, breaks = c(-Inf, 0, .05, .1, .2, .25, Inf), 
                               labels = c("None/Decay", "Low", "M-Low", "Medium", "M-High", "High"),
                               ordered_result = T)) #Classify growth rate for discrete colors

#temp <- globalDF %>%
#        mutate(d10 = ifelse(deaths >=10,1,0)) %>%
        
#test.df$first.login <- ave(test.df$date, test.df$id, FUN = min)

#Aggregate Data
nationalCases <- globalDF %>%
        group_by(countryRegion,Date)%>%
        summarise(
                cumulativeCases = sum(cumulativeCases),
                activeCases = sum(activeCases),
                yesterdayCumulative = sum(yesterdayCumulative),
                yesterdayActive = sum(yesterdayActive),
                newCases = sum(newCases),
                deaths = sum(deaths),
                recovered = sum(recovered)
        )%>%
        mutate(recovered = replace(recovered, countryRegion == 'US', USRecovered$recovered)) %>%
        mutate(mortalityRate = ifelse(lag(cumulativeCases, k=3, default = 0)>0,
                                      (deaths/lag(cumulativeCases, k=3, default = 0)),0)) %>%
        mutate(caseGrowth = ifelse(yesterdayActive>0,(activeCases - yesterdayActive)/yesterdayActive,0))  %>% #Create Growth Variable
        mutate(smoothGrowth = movavg(caseGrowth,5,"e"))%>% #Use a moving average to create a smoothed Growth Variable
        mutate(growthCat = cut(smoothGrowth, breaks = c(-Inf, 0, .05, .1, .2, .25, Inf), 
                               labels = c("None/Decay", "Low", "M-Low", "Medium", "M-High", "High"),
                               ordered_result = T)) %>% #Classify growth rate for discrete colors
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion)

#Create "days since 100th Case"
c100DF <- nationalCases %>%
        filter(cumulativeCases >= 100) %>%
        group_by(countryRegion) %>%
        mutate(minDate = min(Date), daysFrom100 = julian(Date) - julian(minDate)) #%>%

#Create "days since 10th death"
d10DF <- nationalCases %>%
        filter(deaths >= 10) %>%
        group_by(countryRegion) %>%
        mutate(minDate = min(Date), daysFrom10 = julian(Date) - julian(minDate)) #%>%
        

#Identify cumulative cases as of most recent download
current <- max(globalDF$Date)
currentNationalTotal <- filter(globalDF,Date==current)
pal <- colorFactor(palette = c("green", "yellowgreen", "yellow", "orange", "red", "red4"), 
                   domain= currentNationalTotal$growthCat)

      