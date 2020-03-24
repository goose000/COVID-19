#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Environment cleanup


library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)
library(leaflet)
library(httr)



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

#Aggregate Data
nationalCases <- masterDF %>%
        group_by(countryRegion,Date)%>%
        summarise(
                cumulativeCases = sum(cumulativeCases),
                activeCases = sum(activeCases)
        )%>%
        select(Date,countryRegion, everything())%>%
        arrange(Date,countryRegion)

#Identify newest date in dataframe, select associated cases.
current <- max(masterDF$Date)
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

pal <- colorFactor(palette = c("green", "yellow", "red"), 
                   domain= masterDF$growthCat)


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
                                    max = as.POSIXct(today()),
                                    value = current,
                                    step = step,
                                    round = T,
                                    animate = T,
                                    animationOptions(loop = T)
                        ),
                        h5("Click the play button to start Animation")
                ),
                # Show a plot of the generated distribution
                mainPanel(
                        plotlyOutput("Plot1"),
                        leafletOutput("Plot2")
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
        #Create a Growth Chart
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
                        layout(title = 'Active Cases of COVID-19 per Country Over Time')
                plot1
        })
        
        #Create interactive map
        selectedCumulativeTotal <- reactive({
                filter(masterDF,Date==input$Date)
        })

        output$Plot2 <- renderLeaflet({
                plot2 <- selectedCumulativeTotal() %>%
                        leaflet() %>%
                        addTiles() %>%
                        addCircleMarkers(
                                weight = 1,
                                color = ~pal(selectedCumulativeTotal()$growthCat),
                                radius = 2*log(selectedCumulativeTotal()$activeCases),
                                #clusterOptions = markerClusterOptions(), 
                                popup = paste(selectedCumulativeTotal()$provinceState, "<br>",
                                              "Active Cases:",selectedCumulativeTotal()$activeCases, "<br>",
                                              "Deaths:",selectedCumulativeTotal()$deaths, "<br>",
                                              "Recovered:",selectedCumulativeTotal()$recovered
                                )
                        )%>%
                        addLegend(labels = c('No Growth', '0-20% Growth Rate', '>20% Growth Rate'),
                                  colors = c('green', 'yellow', 'red'))
                
        })
}

# Run the application 
shinyApp(ui = ui, server = server)


