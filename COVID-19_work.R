
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
  "readr"
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
confirmed_URL <-  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
 #paste0(directory,"Deaths.csv")
deaths_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  #paste0(directory,"Recovered.csv")
recovered_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

#Load Data
confirmedCases <- read_csv(url(confirmed_URL))
deaths <- read_csv(url(deaths_URL))
recovered <- read_csv(url(recovered_URL))

#Reshape the dataframe, clean up names, reorder variables and observations
tidyConfirmedCases <- confirmedCases %>%
        rename(provinceState = `Province/State`, countryRegion = `Country/Region`) %>%
        pivot_longer(-c(provinceState, countryRegion, Lat, Long), 
                     names_to = "Date", values_to = "cumulativeCases") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion,provinceState)%>%
        rename(latitude = Lat, longitude = Long )

#Repeat Everything for Deaths and Recovered
tidyDeaths <- deaths %>%
        rename(provinceState = `Province/State`, countryRegion = `Country/Region`) %>%
        pivot_longer(-c(provinceState, countryRegion, Lat, Long), 
                     names_to = "Date", values_to = "deaths") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion,provinceState)%>%
        rename(latitude = Lat, longitude = Long )

tidyRecovered <- recovered %>%
        rename(provinceState = `Province/State`, countryRegion = `Country/Region`) %>%
        pivot_longer(-c(provinceState, countryRegion, Lat, Long), 
                     names_to = "Date", values_to = "recovered") %>%
        mutate(Date = mdy(Date,tz="UTC"))%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion,provinceState)%>%
        rename(latitude = Lat, longitude = Long )

#Merge Dataframes
masterDF <- tidyConfirmedCases %>%
        merge(tidyDeaths) %>%
        merge(tidyRecovered)

#identify countries without provinces
noProvinces <- is.na(masterDF$provinceState)

#Replace empty province entries with country names       
masterDF$provinceState[noProvinces]<-masterDF$countryRegion[noProvinces]
       
#Convert Province and Country to factor
masterDF$provinceState <- masterDF$provinceState %>%
        as.factor()
masterDF$countryRegion <- masterDF$countryRegion %>%
        as.factor()

#Remove missing data (important for when case numbers have not yet updated)
masterDF <- masterDF[complete.cases(masterDF),]

#Create Active Cases Variable
masterDF <- masterDF %>%
        mutate(activeCases = cumulativeCases - recovered)

#Create New Cases Variable
masterDF <- masterDF %>%
        group_by(provinceState) %>%
        mutate(newCases = cumulativeCases - lag(cumulativeCases, default = 0))

#Create Growth Variable
masterDF <- masterDF %>%
        mutate(caseGrowth = ifelse(activeCases>0,newCases/activeCases,0))

#Create a smoothed Growth Variable
masterDF <- masterDF %>%
        mutate(smoothGrowth = smooth(caseGrowth))

#Create a factor for the level of growth
masterDF <- masterDF %>%
        mutate(growthCat = cut(smoothGrowth, breaks = c(Inf, .2, 0, -Inf), 
                                 labels = c("High", "Low", "Decay")))

#Identify cumulative cases as of most recent download
current <- max(masterDF$Date)
currentCumulativeTotal <- filter(masterDF,Date==current)
pal <- colorFactor(palette = c("green", "yellow", "red"), 
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
nationalCases <- masterDF %>%
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
fig <- plot_ly(masterDF, x = ~Date,
               y = ~smoothGrowth, 
               color = ~provinceState,
               type = "scatter",
               mode = "lines"
)%>%
        layout(title = 'Growth in Cases of COVID-19 per Country Over Time')
fig 

      