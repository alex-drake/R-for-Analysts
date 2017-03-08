---
title: "LeaRn Reactive Shiny"
author: "Alex Drake (alexanderdrake@tfl.gov.uk)"
date: "14 March 2017"
output: 
  html_document:
    toc: yes
---

# Building Shiny Apps

We've already seen how to create a shiny application thanks to an [excellent tutorial](http://onelink.tfl.gov.uk/sites/IRUF/ranalytics/SiteAssets/SitePages/ISLR/shinyTut.html) by Reka Solymosi (01/07/2016) - here we'll look again at how to get the most out of it using reactive programming as it's an often overlooked but very powerful feature!

There is an excellent explanation from RStudio available at [http://shiny.rstudio.com/articles/reactivity-overview.html](http://shiny.rstudio.com/articles/reactivity-overview.html)

## Packages

I'll aim to use and abuse some of the packages covered recently in these sessions, in particular I'll use `tidyverse` for consistency, `jsonlite` to import data from the TfL API and `leaflet`/`shiny` to create the app.

```{r, echo=T, message=F,warning=F}
# load packages
library(jsonlite)
library(leaflet)
library(shiny)
library(tidyverse)
```

## TfL API

Note that you'll need to have access to the tfl API in order to get the ACCStats data. You can do that [here](https://api-portal.tfl.gov.uk/login) - you can then view your id/key on the 'API Credentials' page.

```{r, eval=F}
AppId <- "your app id here"
AppKey <- "your app key here"
```

## Quick example

Let's remind ourselves of what a shiny app looks like using the example from Reka's tutorial. Note that this will create an app that does...nothing (except give you an idea of the layout)!

```{r, eval=F}
ui <- fluidPage(
  titlePanel("title panel"),

  sidebarLayout(
    sidebarPanel( "sidebar panel"),
    mainPanel("main panel")
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
```

It's more efficient to create an app showing our 'fixed' elements and then render the additional elements on top using `reactive()` and `observe()`, so let's define our fixed elements and then add the reactive elements later.

```{r, eval = F}
choices <- c(2013,2014,2015)

ui <- fluidPage(
  headerPanel(title = h4("ACCStats Map")),
  sidebarPanel(
    selectInput(inputId = "years", 
                label = "Year: ",
                choices = choices,
                selected = max(choices),
                multiple = F),
    selectInput(inputId = "sev",
                label = "Severity",
                choices = c("Fatal","Serious","Slight"),
                selected = c("Fatal","Serious","Slight"),
                multiple = T)
  ),
  mainPanel(
    leafletOutput(outputId = "map", 
                  height = 500),
    br(),
    plotOutput(outputId = "plot",
               height = 350)
  )
)

server <- function(input, output, session){
  
  # define the fixed map feature
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(lng1 = -0.2, lng2 = 0.0,
                lat1 = 51.4, lat2 = 51.6)
  })
  
  # define the plot output
  output$plot <- renderPlot({
    # blank for now
  })
}

shinyApp(ui = ui,
         server = server)
```

# Reactive Expressions & Observers

## Reactive Expressions

Reactivity is important in Shiny as it allows the user to tailor the app's outputs to suit their needs but it's important to understand how it works if you want to create a slick application with minimal hangtime. For example, we could send a request to the API every time the user changes an input - whilst this would provide what the user wanted, it would be really inefficient and 'laggy'. The other option would be to do one import of a larger data set and then refine what is displayed within the app.

We can do this using reactive expressions, which cache the results of any procedure that happens in response to user input (such as accessing a database!). We can even use the reactive expression to carry out calculations or cleanse the data into a state so that it is useful for other elements within our app. 

```{r, eval=F}
acc_data <- reactive({
    
    # define request URL
    requestUrl <- paste0("https://api-argon.tfl.gov.uk/AccidentStats/", 
                         input$years,
                         "?app_id=", AppId, 
                         "&app_key=", AppKey)
    
    # get around the pesky corporate firewall
    l <- readLines(requestUrl, encoding="UTF-8", warn = FALSE)
    d <- fromJSON(l)
    
    # select required columns
    d_clean <- d %>% 
      select(id, lat, lon, location, date, severity, borough)
    
    # change the date format
    d_clean$date <- strptime(d_clean$date, 
                             format = "%Y-%m-%dT%H:%M:%SZ")
    
    # add hour column
    d_clean$hour <- as.numeric(format(d_clean$date, format = "%H"))
    
    # create hour summary
    df_hour <- d_clean %>%
      select(hour, severity) %>%
      group_by(hour, severity) %>%
      summarise(count = length(severity))
    
    # return a list of the required data
    return(list(df = d_clean, df_smry = df_hour))
  })

```

This function pulls in data from the TfL API based on the user input for year, removes columns not required, cleans the date format and also creates a summary table based on the hour that an incident occurred. We can pull both the cleaned data frame and the summary data frame from the function by using `return(list(...))` BUT will only do it when the user changes the year!

## A note on Observers

Observers are equally important when making an application that runs smoothly. They are similar to reactive expressions but also have some key differences - they can access reactive values and expressions but do not return any values. Instead, observers are used to send data to the web browser and there are a few ways of declaring them in your app script - we'll look at the main two.

## Observe with LeafletProxy

We've already created our map, as shown when we run our application. What we now want to do is update it to now display our user requested data ie the data passed in from our reactive expression, `acc_data()`. For efficiency, we do not want to redraw the basemap each time the data is refreshed and so we use `leafletProxy()` as our reactive element, and wrap it inside an `observe()` function.

We must define which element of `acc_data()` we want to use and then let R know which leaflet map we want to add the additional layers too ('map'). Note the use of `clearGroup()` to make sure we only draw the new data on the map.

```{r, eval=F}
# create the observer to render appropriate points on map
  observe({
    
    # create a list of dataframes returned from acc_data()
    df <- acc_data()
    
    # pull through the required dataframe from the list
    df <- df$df
    
    # add the layers to the leaflet map called 'map'
    leafletProxy("map") %>%
      clearGroup("accs") %>%
      addCircleMarkers(data = df,
                       lng = df$lon,
                       lat = df$lat,
                       weight = 1, 
                       color = "Blue",
                       fillOpacity = 0.4,
                       group = "accs",
                       clusterOptions = markerClusterOptions())
    
  })
```

We can take this observer function further by also reacting to changes in the 'Severity' level by adding an extra line. This will make sure that the map will display different data if either the 'Severity' level or 'Year' has changed.

```{r, eval=F}
# create the observer to render appropriate points on map
  observe({
    
    df <- acc_data()
    
    df <- df$df
    
    # limit the data to user 'Severity' choice
    df <- df[df$severity %in% input$sev,]
    
    leafletProxy("map") %>%
      clearGroup("accs") %>%
      addCircleMarkers(data = df,
                       lng = df$lon,
                       lat = df$lat,
                       weight = 1, 
                       color = "Blue",
                       fillOpacity = 0.4,
                       group = "accs",
                       clusterOptions = markerClusterOptions())
    
  })
```

## Observe with render...()

We can also use the `render...()` family of functions to observe/react to user changes, but to do this we must first assign it to an `output` object as follows:

```{r, eval=F}
output$foo <- renderText({ bar })
```

On first glance this would suggest that observers *do* return values however, the function `renderText()` is not an observer/endpoint unless it is assigned to `output$foo`. This assignment wraps `renderText()` into another function which *is* an observer - this is necessary to send the data to the browser.

We will demonstrate this by using the `renderPlot()` placeholder we created earlier and displaying the summary data from `acc_data()`. Note that we again limit the displayed data by the 'Severity' user input.

```{r, eval=F}
output$plot <- renderPlot({
    
    # create a list of dataframes returned from acc_data()
    df <- acc_data()
    
    # pull through the required dataframe from the list
    df <- df$df_smry
    
    # limit the data to user 'Severity' choice
    df <- df[df$severity %in% input$sev,]
    
    # create a stacked bar chart
    p <- ggplot(data = df, aes(x = hour, y = count, fill = severity)) + 
      geom_bar(stat="identity") +
      theme_classic()
    
    p
  })
```

# Complete app

Now let's put all of the code together to complete our app. Remember to declare your own `AppID` and `AppKey` otherwise you won't be able to access any of the data!

```{r, eval=F}
AppId <- "your app id here"
AppKey <- "your app key here"

choices <- c(2013,2014,2015)

ui <- fluidPage(
  headerPanel(title = h4("ACCStats Map")),
  sidebarPanel(
    selectInput(inputId = "years", 
                label = "Year: ",
                choices = choices,
                selected = max(choices),
                multiple = F),
    selectInput(inputId = "sev",
                label = "Severity",
                choices = c("Fatal","Serious","Slight"),
                selected = c("Fatal","Serious","Slight"),
                multiple = T)
  ),
  mainPanel(
    leafletOutput(outputId = "map", 
                  height = 500),
    br(),
    plotOutput(outputId = "plot",
               height = 350)
  )
)

server <- function(input, output, session){
  
  # define the fixed map feature
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(lng1 = -0.2, lng2 = 0.0,
                lat1 = 51.4, lat2 = 51.6)
  })
  
  # define our reactive expression
  acc_data <- reactive({
    
    # define request URL
    requestUrl <- paste0("https://api-argon.tfl.gov.uk/AccidentStats/", 
                         input$years,
                         "?app_id=", AppId, 
                         "&app_key=", AppKey)
    
    # get around the pesky corporate firewall
    l <- readLines(requestUrl, encoding="UTF-8", warn = FALSE)
    d <- fromJSON(l)
    
    # select required columns
    d_clean <- d %>% 
      select(id, lat, lon, location, date, severity, borough)
    
    # change the date format
    d_clean$date <- strptime(d_clean$date, 
                             format = "%Y-%m-%dT%H:%M:%SZ")
    
    # add hour column
    d_clean$hour <- as.numeric(format(d_clean$date, format = "%H"))
    
    # create hour summary
    df_hour <- d_clean %>%
      select(hour, severity) %>%
      group_by(hour, severity) %>%
      summarise(count = length(severity))
    
    # return a list of the required data
    return(list(df = d_clean, df_smry = df_hour))
  })
  
  # create the observer to render appropriate points on map
  observe({
    
    df <- acc_data()
    
    df <- df$df
    
    df <- df[df$severity %in% input$sev,]
    
    leafletProxy("map") %>%
      clearGroup("accs") %>%
      addCircleMarkers(data = df,
                       lng = df$lon,
                       lat = df$lat,
                       weight = 1, 
                       color = "Blue",
                       fillOpacity = 0.4,
                       popup = paste0("Date: ",df$date, br(),
                                      "Severity: ", df$severity),
                       group = "accs",
                       clusterOptions = markerClusterOptions())
    
  })
  
  # create observer to render plots
  output$plot <- renderPlot({
    
    df <- acc_data()
    
    df <- df$df_smry
    
    df <- df[df$severity %in% input$sev,]
    
    p <- ggplot(data = df, aes(x = hour, y = count, fill = severity)) + 
      geom_bar(stat="identity") +
      theme_classic()
    
    p
  })
}

shinyApp(ui = ui,
         server = server)
```

# Remember

* Observers (or endpoints) respond to reactive events but reactive expressions do not - if you want a reactive expression to execut then you need an observer as an output.
* Reactive expressions return values, observers do not!

*Now go away*


