---
title: "Building A Shiny App"
author: "Alex Drake"
date: "12 April 2017"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: "yeti"
---

# Introduction

We've previously had a look at how to manipulate our data, create functions and even map our data using `Leaflet`. The next step is to see how we can combine this to create an easy to understand output. One of the best ways to do this is to use `Shiny` to create an application.

First load the packages we need for this lesson - install `Shiny` if you haven't already got it!

```{r, eval=F}
library(jsonlite)
library(leaflet)
library(shiny)
library(tidyverse)
```

## About Shiny

`Shiny` allows you to create a powerful web application within an R environment and uses R functions to write the JavaScrip, HTML and CSS needed to display your app. What is great about `Shiny` is that you can perform any R calculation in the background and display the results as your output.

# Basic Shiny

In order for `Shiny` to perform properly, we need to create a *server function* to perform calculations and a *user interface* (UI) to display our outputs and allow user inputs. We're going to create a simple app to demonstrate how this works with an empty server and a UI with a basic set up. We'll then launch this so using `shinyApp` so we can see what it looks like.

Note that we could create the UI and server as separate R files and combine them later, however for simplicity we're going to look at **single file apps only**. You'll want to copy and paste this into your R session to run it as Rmarkdown won't render an interactive application!

```{r, eval=F}
# The User Interface
ui <- basicPage("Look, a Shiny app!")

# The Server
server <- function(input, output, session){
  # Currently blank
}

# Now launch the app
shinyApp(ui = ui,
         server = server)
```

# Creating your user interface (UI)

## Layout

`Shiny` uses Twitter Bootstrap to scaffold and provide minimal styling to your applications - running `shinyApp` then compiles your code into web-friendly languages. For the most part, the inbuilt UI functions are sufficient to create a powerful app but you'll eventually find the need to add some custom HTML/CSS/JavaScript in to get the best bang for your buck. We're only going to look at the inbuilt functions today but I can recommend [W3 Schools](https://www.w3schools.com/html/) for those who are curious/want to take their app to the next level.

## Using RStudio Layout Functions

There are lots of different layout styles available to you in `Shiny`, you can find a full list of them here [Shiny Application layout guide](http://shiny.rstudio.com/articles/layout-guide.html). We're going to focus on using `fluidPage` to create an app with a `sidebarPanel`, `mainPanel` and not much else. You can see this more clearly in the example below.

```{r, eval=F}
# UI
ui <- fluidPage(
  
  # Our page title
  titlePanel("Simple App")
  
  # Our page layout
  sidebarLayout(
    # sidebar contents
    sidebarPanel("sidebar panel")
    
    # main page contents
    mainPanel("main panel")
  )
  
)

# server
server <- function(input, output, session){
  # Currently blank
}

shinyApp(ui = ui,
         server = server)
```

So far so good. Now let's jazz it up a bit by adding a page title and some elements into out `sidebar` and `mainPanel`. We're going to use some ACCStats data from the TfL API so we'll create the appropriate elements from now on before testing our app IRL.

First we want to createt our header using `headerPanel()` and format the size using one of the inbuilt functions, `h4()` ie header size 4. The header functions work in a similar way to the styles in MS Word so have a play around with `h1()`, `h2()` etc if you want!

The next step is to define the user inputs based on potential queries to the TfL API. The ACCStats data is split by year and incident severity, so we'll allow the user to select these options with the `selectInput()` function. Note that we can select `multiple = T/F` if we do/don't want the user to be able to select multiple options.

We will also define where the outputs will be displayed using `leafletOutput` (for a map output) and `plotOutput` (a chart of our results). 

```{r, eval=F}
ui <- fluidPage(title = "ACCStats Map",
  headerPanel(title = h4("ACCStats Map")),
  sidebarPanel(
    selectInput(inputId = "years",
                label = "Year: ",
                choices = c(2013, 2014, 2015),
                multiple = F),
    selectInput(inputId = "sev",
                label = "Severity: ",
                choices = c("Fatal", "Serious", "Slight"),
                selected = c("Fatal", "Serious", "Slight"),
                multiple = T)
  ),
  mainPanel(
    leafletOutput(outputId = "map", height = 500),
    br(),
    plotOutput(outputId = "plot", height = 350)
  )
)

server <- function(input, output, session){
  
}

shinyApp(ui = ui,
         server = server)
```

You should have now created an application with a series of user inputs but no outputs in the `mainPanel` - this is because we need to define our outputs using the `server` function.

# Defining your server

Currently our users can interact with the application but without code in our server, it cannot react to any input changes. 

In order to react, we need to pass in our inputs, using the `inputId` found within our `selectInput` functions and also `render` our outputs (`leafletOutput` etc) using `Shiny` `render*` functions. 

For example, if we want to access our severity input we would call `input$sev` within our server and then combine this with either `renderLeaflet` or `renderPlot` - check the `Shiny` cheatsheet for other `render*` options!

That said, our first step is to finish defining our constants ie what we want the app to display regardless of user input. We can do this by creating a simple `leaflet` map in our sever.

```{r, eval=F}
ui <- fluidPage(title = "ACCStats Map",
  headerPanel(title = h4("ACCStats Map")),
  sidebarPanel(
    selectInput(inputId = "years",
                label = "Year: ",
                choices = c(2013, 2014, 2015),
                multiple = F),
    selectInput(inputId = "sev",
                label = "Severity: ",
                choices = c("Fatal", "Serious", "Slight"),
                selected = c("Fatal", "Serious", "Slight"),
                multiple = T)
  ),
  mainPanel(
    leafletOutput(outputId = "map", height = 500),
    br(),
    plotOutput(outputId = "plot", height = 350)
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

We now have a basic app ready to react to changes in our user input. We'll need to respond to these changes using the `reactive` and `observe` functions.

## Reactive Expressions

Reactivity is important in `Shiny` as it allows the user to tailor the app's outputs to suit their needs but it's important to understand how it works if you want to create a slick application with minimal hangtime. For example, we could send a request to the API every time the user changes an input - whilst this would provide what the user wanted, it would be really inefficient and 'laggy'. The other option would be to do one import of a larger data set and then refine what is displayed within the app.

We can do this using reactive expressions, which cache the results of any procedure that happens in response to user input (such as accessing a database!). We can even use the reactive expression to carry out calculations or cleanse the data into a state so that it is useful for other elements within our app.

First, define your TfL API AppId and AppKey. Remember you can get these from the 'API Credentials' page if you've forgotten them since last time.

```{r, eval=F}
AppId <- "your app id here"
AppKey <- "your app key here"
```

Note that the following functions won't work on their own as they must be called within your `server` function. However, it is useful to look at each section as a standalone before lumping everything together!

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

Observers are equally important when making an application that runs smoothly. They are similar to reactive expressions but also have some key differences - they can access reactive values and expressions but do not return any values. Instead, observers are used to send data to the web browser and there are a few ways of declaring them in your app script - today we'll look at two of them.

## Observe with LeafletProxy

We've already created our map, as shown when we run our application. What we now want to do is update it to display our user requested data ie the data passed in from our reactive expression, `acc_data()`. For efficiency, we do not want to redraw the basemap each time the data is refreshed and so we use `leafletProxy()` as our reactive element, and wrap it inside an `observe()` function.

We must define which element of `acc_data()` we want to use and then let R know which leaflet map we want to add the additional layers to ('map'). Note the use of `clearGroup()` to make sure we only render the new data on the map.

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

We can also use the `render...()` family of functions to observe/react to user changes, but to do this we must first assign it to an output object as follows:

```{r, eval=F}
output$foo <- renderText({ bar })
```

On first glance this would suggest that observers *do* return values however, the function `renderText()` is not an observer/endpoint unless it is assigned to output$foo. This assignment wraps `renderText()` into another function which is an observer and is required if we want to send the data to the browser.

We will demonstrate this by using the `renderPlot()` placeholder we created earlier and display the summary data from `acc_data()`. Note that we again limit the displayed data by the 'Severity' user input.

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

# Complete App

Now let's put all of the code together to complete our app. Remember to declare your own AppID and AppKey otherwise you won't be able to access any of the data!

Note that we can declare our AppId, AppKey and year choices *outside* of the UI as this makes it easier to change them at a later date

```{r, eval=F}
AppId <- "your app id here"
AppKey <- "your app key here"

choices <- c(2013,2014,2015)

ui <- fluidPage(title = "ACCStats Map",
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

Well done, you've created your first app! The world of `Shiny` is now open to you, so here is some useful reading material for your long and arduous journey

* [R Shiny Cheatsheet](http://shiny.rstudio.com/images/shiny-cheatsheet.pdf) - the official RShiny cheatsheet.
* [R Shiny Examples](https://shiny.rstudio.com/gallery/) - some shiny examples from the folks at RStudio.
* [ZevRoss Shiny Tutorial](http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/#shiny-at-its-simplest) - features some great tutorials with 40 examples.