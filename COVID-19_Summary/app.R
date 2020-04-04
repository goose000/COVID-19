#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Environment cleanup
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)
library(leaflet)
library(httr)
library(pracma)

#reverse = function(x) sort(x, decreasing = TRUE)

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
#close(globalConfirmedCon, globalDeathsCon, globalRecoveredCon, USConfirmedCon, USDeathsCon)

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

#Identify newest date in dataframe, select associated cases.
current <- max(globalDF$Date)
currentNationalTotal <- filter(nationalCases,Date==current)

#use most current data to identify the worst 5 countries for the default
worst <- currentNationalTotal %>%
        arrange(desc(activeCases)) %>%
        select(countryRegion) %>%
        head(5)
worstCountries <- worst$countryRegion

countries <- levels(nationalCases$countryRegion)
start <- nationalCases$Date[1]
step <- as.POSIXct(today()) - as.POSIXct(today()-1)

#pal <- colorNumeric(palette = c("RdYlGn"), 
#                   domain= c(0,1), reverse = F)

pal <- colorFactor(palette = c("green", "yellowgreen", "yellow", "orange", "red", "red4"), 
                   domain= currentNationalTotal$growthCat)



library(shiny)

# Define UI for application that creates a growth chart and an animated map
ui <- fluidPage(
        
        # Application title
        titlePanel("COVID-19 Growth Data"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        h3("Select Countries for Growth Chart"),
                        selectInput("countrySelect", 
                                    "Select Countries", 
                                    countries,
                                    selected = worstCountries,
                                    multiple = T
                        ),
                        h3("Adjust date for Case Map"),
                        sliderInput("Date",
                                    "as of",
                                    min = start,
                                    max = current,
                                    value = start,
                                    step = step,
                                    round = T,
                                    animate = T,
                                    animationOptions(loop = T)
                        ),
                        h5("Click the play button to start Animation")
                ),
                # Show a plot of the generated distribution
                mainPanel(
                        tabsetPanel(type = "tabs",
                                tabPanel("Active Cases", h4('Select countries in sidebar'),
                                         plotlyOutput("Plot1")),
                                tabPanel("Cumulative Cases", h4('Select countries in sidebar'),
                                         plotlyOutput("Plot2")),
                                tabPanel("Growth Rate", h4('Select countries in sidebar'),
                                         plotlyOutput("Plot3")),
                                tabPanel("Deaths", h4('Select countries in sidebar'),
                                         plotlyOutput("Plot4")),
                                tabPanel("Mortality", h4('Select countries in sidebar'),
                                         plotlyOutput("Plot5")),
                                tabPanel("LogDeaths", h4('Select countries in sidebar'),
                                         plotlyOutput("LogPlot1")),
                                tabPanel("LogGrowth", h4('Select countries in sidebar'),
                                         plotlyOutput("LogPlot2")),
                                tabPanel("Map",  h4('Click Play in sidebar for growth animation'),
                                         leafletOutput("Map"))
                        ),
                        h5('Code to create this app will be uploaded to my github at https://github.com/goose000/COVID-19. Please let me know of any issues with the app, or any suggestions you have for further improvement. Coming soon: US focused charts.')
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
        #Create an Active Cases Chart
        selectedConfirmedCases <- reactive({
                nationalCases %>%
                        filter(countryRegion %in% input$countrySelect) #%>% 
        })
        
        output$Plot1 <- renderPlotly({
                plot1 <- plot_ly(
                        selectedConfirmedCases(), 
                        x = ~Date,
                        y = ~activeCases, 
                        color = ~countryRegion,
                        type = "scatter",
                        mode = "lines"
                )%>%
                        layout(title = 'Active Confirmed Cases of COVID-19')
                plot1
        })
        
        #Create an Cumulative Cases Chart

        output$Plot2 <- renderPlotly({
                plot2 <- plot_ly(
                        selectedConfirmedCases(), 
                        x = ~Date,
                        y = ~cumulativeCases, 
                        color = ~countryRegion,
                        type = "scatter",
                        mode = "lines"
                )%>%
                        layout(title = 'Cumulative Confirmed Cases of COVID-19')
                plot2
        })
        
        #Create a Growth Chart

        output$Plot3 <- renderPlotly({
                plot3 <- plot_ly(
                        selectedConfirmedCases(), 
                        x = ~Date,
                        y = ~smoothGrowth, 
                        color = ~countryRegion,
                        type = "scatter",
                        mode = "lines"
                )%>%
                        layout(title = 'Growth Rate of COVID-19 Over Time')
                plot3
        })
        
        #Create a Death Chart

        output$Plot4 <- renderPlotly({
                plot4 <- plot_ly(
                        selectedConfirmedCases(), 
                        x = ~Date,
                        y = ~deaths, 
                        color = ~countryRegion,
                        type = "scatter",
                        mode = "lines"
                )%>%
                        layout(title = 'Deaths from COVID-19')
                plot4
        })
        
        #Create a Mortality Chart
        
        output$Plot5 <- renderPlotly({
                plot5 <- plot_ly(
                        selectedConfirmedCases(), 
                        x = ~Date,
                        y = ~mortalityRate, 
                        color = ~countryRegion,
                        type = "scatter",
                        mode = "lines"
                )%>%
                        layout(title = 'Mortality Rate Over Time')
                plot5
        })
        
        #Filter Data for Log plots
        selectedDaysFrom10D <- reactive({
                d10DF %>%
                        filter(countryRegion %in% input$countrySelect) #%>% 
        })
        
        selectedDaysFrom100C <- reactive({
                c100DF %>%
                        filter(countryRegion %in% input$countrySelect) #%>% 
        })
        
        #Create Log Plots
        output$LogPlot1 <- renderPlotly({
                fig <- plot_ly(selectedDaysFrom10D(), x = ~daysFrom10,
                               y = ~deaths, 
                               color = ~countryRegion,
                               type = "scatter",
                               mode = "lines"
                )%>%
                        layout(title = 'Growth in Deaths Since 10th Death',
                               yaxis = list(type = 'log'))
                fig
        })
        
        output$LogPlot2 <- renderPlotly({
                fig <- plot_ly(selectedDaysFrom100C(), x = ~daysFrom100,
                               y = ~cumulativeCases, 
                               color = ~countryRegion,
                               type = "scatter",
                               mode = "lines"
                )%>%
                        layout(title = 'Growth in Cases Since 100th Case',
                               yaxis = list(type = 'log'))
                fig
        })
        
        #Create interactive map
        selectedCumulativeTotal <- reactive({
                filter(globalDF,Date==input$Date)
        })
        

        output$Map <- renderLeaflet({
                plot2 <- selectedCumulativeTotal() %>%
                        leaflet() %>%
                        addTiles() %>%
                        addCircleMarkers(
                                weight = 1,
                                color = ~pal(selectedCumulativeTotal()$growthCat),
                                radius = 2*log(selectedCumulativeTotal()$activeCases),
                                #clusterOptions = markerClusterOptions(), 
                                popup = paste(selectedCumulativeTotal()$combinedKey, "<br>",
                                              "Active Cases:",selectedCumulativeTotal()$activeCases, "<br>",
                                              "Deaths:",selectedCumulativeTotal()$deaths, "<br>",
                                              "Recovered:",selectedCumulativeTotal()$recovered
                                )
                        )%>%
                        addLegend(pal = pal, 
                                  values = ~selectedCumulativeTotal()$growthCat,
                                  title = "Daily Growth Rate",
                                  opacity = 1
                                  )
                
        })
}

# Run the application 
shinyApp(ui = ui, server = server)


