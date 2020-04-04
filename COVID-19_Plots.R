#Create interactive map

currentNationalTotal %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
                weight = 1,
                color = ~pal(growthCat),
                radius = 2*log(currentNationalTotal$cumulativeCases),
                #clusterOptions = markerClusterOptions(), 
                popup = paste(currentNationalTotal$provinceState, "<br>",
                              "Confirmed Cases:",currentNationalTotal$cumulativeCases, "<br>",
                              "Deaths:",currentNationalTotal$deaths, "<br>",
                              "Recovered:",currentNationalTotal$recovered
                )
        )# %>%

#Code for adding a date to the map
#        addCircleMarkers(lat = 75, lng = -120, label = today(),
#                   markerOptions(opacity = .001, radius = .01),
#                   labelOptions = labelOptions(noHide = T, textsize = "12px"))


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
               color = ~countryRegion,
               type = "scatter",
               mode = "lines"
)%>%
        layout(title = 'Growth in Cases of COVID-19 per Country Over Time')
fig 

fig <- plot_ly(d10DF, x = ~daysFrom10,
               y = ~deaths, 
               color = ~countryRegion,
               type = "scatter",
               mode = "lines"
)%>%
        layout(title = 'Growth in Deaths Since 10th Death',
               yaxis = list(type = 'log'))
fig 

fig <- plot_ly(c100DF, x = ~daysFrom100,
               y = ~cumulativeCases, 
               color = ~countryRegion,
               type = "scatter",
               mode = "lines"
)%>%
        layout(title = 'Growth in Cases Since 100th Case',
               yaxis = list(type = 'log'))
fig 