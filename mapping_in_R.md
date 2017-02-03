# Data Visualisation - Mapping
Alex Drake  
10 November 2016  



# Introduction

As we've seen previously, R is a powerful tool when it comes to producing visualisations for your data in graphical form. How about when you want to visualise your data spatially? Well luckily there is a similar 'grammar for graphics' involved in mapping your data. 

We're going to look at creating a map using your data and then a few methods to display this; firstly as points and then by aggregating to polygons.

First things first, let's set up a working directory on our U: drive - we'll save any outputs here and it's a good idea to save your script there too!


```r
setwd("U:/R Session 3/")

# install these packages!!
packages <- c("broom",
              "maptools",
              "plyr",
              "rgdal",
              "rgeos")

# install.packages(packages, dependencies = TRUE)
```


We're going to introduce a few new packages for this session, so first make sure you have them installed by calling `install.packages()` and then run the above section to load them into your workspace - remember to uncomment the last line!

# Plotting Point Data

First, let's read in our data using `read.csv()`. We're going to use a sample of Metropolitan Police data from [https://data.police.uk](https://data.police.uk), this is an open data source detailing crime and policing in England, Wales and Northern Ireland. Note that we could have used their API to pull the data directly into R but more on that story later!

Once imported, have a quick look at the data using `View()`.


```r
df <- read.csv(file = "U:/R Session 3/mps crimes.csv",
               header = TRUE,
               na.strings = "")

View(df)
```


```r
# You needed to think about how to import your data by declaring your na values!
df <- read.csv(file = "U:/R Session 3/mps crimes.csv",
               header = TRUE,
               na.strings = c("","NA"))
```

You should see a data frame with 500 rows and 11 columns, some of which aren't particularly useful so let's start by getting rid of the `Crime.ID` column by setting it to `NULL`.


```r
df$Crime.ID <- NULL
```

Let's also have a look to see what categories exist within our data. Have a look at the Crime Type column - try and think how you'd do this in R and then list each unique crime type. What class of data is Crime Type stored as?



We can use the R base package to quickly plot the points into a chart - this isn't particularly useful but is a quick and dirty way to look at your data. We call it as we would a regular plot although we specify the Longitude and Latitdes as the X and Y coordinates.

Plot the crimes defined as 'Other theft' - you should see the plot below! If you don't see any points on your plot then check what class the data is stored as.


```r
other <- df[df$Crime.type=="Other theft",]

# set as numeric so R knows Long/Lat is a number!
plot(x = as.numeric(other$Longitude), y = as.numeric(other$Latitude),
     xlab = "Longitude", ylab = "Latitude")
```

![](mapping_in_R_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The next step is to convert the data into a `SpatialPointsDataFrame` but we'll want to clean the data first be removing the rows that do not have coordinates. How many rows have been removed?


```r
df <- df[!is.na(df$Longitude) & !is.na(df$Latitude), ]
```


```r
# 8 rows have been removed!
```

Now we need to create a unique ID for each crime as `SpatialPointsDataFrame` must have a unique ID for each entry.


```r
df$ID <- 1:nrow(df)
```

We can now load in the package `rgdal` so we can call the `SpatialPointsDataFrame` function. We also need to tell R what our projection is ie what format the coordinates are in. For this session we are using latlong so need to pass in the value `"+init=epsg:4326"`


```r
# load the package
library(rgdal)
```

```
## Warning: package 'rgdal' was built under R version 3.3.2
```

```
## Loading required package: sp
```

```
## Warning: package 'sp' was built under R version 3.3.2
```

```
## rgdal: version: 1.2-5, (SVN revision 648)
##  Geospatial Data Abstraction Library extensions to R successfully loaded
##  Loaded GDAL runtime: GDAL 2.0.1, released 2015/09/15
##  Path to GDAL shared files: U:/AppCfg/R 3.3.1/rgdal/gdal
##  Loaded PROJ.4 runtime: Rel. 4.9.2, 08 September 2015, [PJ_VERSION: 492]
##  Path to PROJ.4 shared files: U:/AppCfg/R 3.3.1/rgdal/proj
##  Linking to sp version: 1.2-4
```

```r
# define the coordinates
coords <- cbind(Longitude = as.numeric(as.character(df$Longitude)), 
                Latitude = as.numeric(as.character(df$Latitude)))

# create the SpatialPointsDataFrame
df.sp <- SpatialPointsDataFrame(coords = coords,
                                data = df,
                                proj4string = CRS("+init=epsg:4326"))
```

Now plot the points to see how our data differs from before. Hmm, looks like we have a straggler. Perhaps we could query the data so we *only* look at crimes within London.


```r
plot(df.sp)
```

![](mapping_in_R_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Note that you can also interrogate your data as normal using the `View` function although you must add an extra element so R knows which part of your spatial data frame to look at. You can do this by calling `View(df.sp@data)`.

# Spatial Queries in R

We've imported a csv file and created some points from the coordinates. Now we want to import a shapefile so that we can query the data and find out which crimes have occurred in which Borough (yes I know that TECHNICALLY it's already in the data set but y'know, play along).

Let's start by loading in our shapefile, again using `rgdal`. We'll need to read in the shapefile and then convert it so it is in the same coordinate system as our data points.


```r
# Read in the shapefile
boro <- readOGR(dsn = "Q:/R-DEV/01 Useful GIS files/Boroughs_region.shp",
                layer = "Boroughs_region")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "Q:/R-DEV/01 Useful GIS files/Boroughs_region.shp", layer: "Boroughs_region"
## with 33 features
## It has 13 fields
## Integer64 fields read as strings:  NUMBER0 NUMBER1 POLYGON_ID UNIT_ID DEFECTSTOT
```

```r
# convert to lat-long
boro <- spTransform(boro, CRS("+init=epsg:4326"))
```

Quickly check to make sure that your data is in the correct (same!) coordinate system. You can do this by calling plot.


```r
# plot boroughs
plot(boro)

# add the data points
plot(df.sp, add = TRUE)
```

![](mapping_in_R_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Hopefully you've been able to recreate the image above. Now let's query the data by getting R to select the points the lie within each borough. We can do this by using the function `over()`. Have a look at the data afterwards as well using `head()`.


```r
# Which points lie in which polygon?
o <- over(df.sp, boro)

# check the data
head(o, 10)
```

```
##                 BOROUGH NUMBER     DESCRIPTIO                FILE_NAME
## 1                Bexley     18 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 2  Barking and Dagenham     16 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 3        Waltham Forest     13 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 4                Barnet     30 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 5              Lewisham      7 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 6             Greenwich      6 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 7                Ealing     27 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 8                Camden      2 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 9             Islington      3 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 10 Kingston upon Thames     23 LONDON BOROUGH GREATER_LONDON_AUTHORITY
##    NUMBER0 NUMBER1 POLYGON_ID UNIT_ID      CODE HECTARES    AREA
## 1       42    1075      50891   10759 E09000004 6428.649 370.619
## 2       93    1432      51295   10949 E09000002 3779.934 169.150
## 3       49    1123      50687   11213 E09000031 3880.793   0.000
## 4       29     965     119320   11378 E09000003 8674.837   0.000
## 5       36    1032      51209   11039 E09000023 3531.706  16.795
## 6       39    1052      50909   10777 E09000011 5044.190 310.785
## 7       14     848      50523   11399 E09000009 5554.428   0.000
## 8       77    1312      50632   11244 E09000007 2178.932   0.000
## 9       84    1349      50581   11281 E09000019 1485.664   0.000
## 10       3     749      50448   11412 E09000021 3726.117   0.000
##    DEFECTSTOT RPU_AREA
## 1           0    South
## 2           0     East
## 3           0     East
## 4           0    North
## 5           0    South
## 6           0    South
## 7           0    North
## 8           0  Central
## 9           0  Central
## 10          0     West
```

It looks like we're missing some data ie the crime details!!! This is because we now need to add the borough attributes to the crime data. We can do this using column bind (call `cbind()`) which will bind our query results to the original data.


```r
df.sp@data <- cbind(df.sp@data, o)
```

We could do this more efficiently by writing it as one line and using the `%over%` call, which is similar to the `IN` call in SQL/MapInfo. 

If you've already run the code above, don't run the section below as you'll dupicate some of the columns!!


```r
df.sp@data <- cbind(df.sp@data, df.sp %over% boro)
```

# Aggregating Data to Your Polygons

So we've determined which crimes have occurred in which Boroughs and have displayed them on a map. However, this isn't particularly useful for presenting to others. Let's have a look at counting the total number of crimes per borough and then adding this as a value to our borough data frame.

We'll start by cleaning the crime data to crimes only in London and then aggregating to borough level using the `plyr` package. This can be done by selecting the crimes where Borough is not NA and then summing the number of points in each borough using the `ddply` function. 

We need to tell `ddply` which data frame to look at and then which variable column we want to aggregate/sum on - for this we will use the Borough column! We also need to provide a name for the column where the summed data will be stored - here I've called it 'Total.Crimes'. Note that because the data we are counting is stored as a factor, we need to tell R to count the number of rows within each Borough by using the `length` function. We could have done this using `mean`, `sum` etc. if we wanted to create summaries for numerical data.


```r
# Clean the data
df.agg <- df.sp@data[!is.na(df.sp@data$BOROUGH),]

# load plyr
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.3.2
```

```r
# summarise by borough
df.agg <- ddply(df.agg, "BOROUGH", summarise, "Total.Crimes" = length(Crime.type))

# Check the data
head(df.agg)
```

```
##                BOROUGH Total.Crimes
## 1 Barking and Dagenham           15
## 2               Barnet           18
## 3               Bexley           13
## 4                Brent           20
## 5              Bromley            6
## 6               Camden           22
```

So we have a list of our boroughs and the total number of crimes in a new data frame called `df.agg`. We now want to join this onto our borough spatial data frame so we can map our data. We do this by using `join` functions from within the `dplyr` package - these work in the same way as joins in SQL so we won't go into detail of the different type of joins!

Let's join the aggregate data onto the `boro` data frame using the common column 'BOROUGH'.


```r
# join using dplyr
boro@data <- dplyr::left_join(boro@data, df.agg, by=c("BOROUGH"="BOROUGH"))

# check the data!
head(boro@data)
```

```
##                BOROUGH NUMBER     DESCRIPTIO                FILE_NAME
## 1 Kingston upon Thames     23 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 2              Croydon     20 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 3              Bromley     19 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 4             Hounslow     25 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 5               Ealing     27 LONDON BOROUGH GREATER_LONDON_AUTHORITY
## 6             Havering     15 LONDON BOROUGH GREATER_LONDON_AUTHORITY
##   NUMBER0 NUMBER1 POLYGON_ID UNIT_ID      CODE  HECTARES    AREA
## 1       3     749      50448   11412 E09000021  3726.117   0.000
## 2       6     776      51330   10896 E09000008  8649.554   0.000
## 3       9     800      50904   10772 E09000006 15013.487   0.000
## 4      12     823      51138   11489 E09000018  5658.541  60.755
## 5      14     848      50523   11399 E09000009  5554.428   0.000
## 6      18     869      50851   10807 E09000016 11445.735 210.763
##   DEFECTSTOT RPU_AREA Total.Crimes
## 1          0     West            9
## 2          0    South           18
## 3          0    South            6
## 4          0     West           15
## 5          0    North           22
## 6          0     East           17
```

You should now see the 'Total.Crimes' column in your borough spatial data frame!

# Mapping Your Data

So far we've used the R base package to plot our data. However, we're going to take this further and use some our `ggplot2` knowledge to make the graph a bit more snazzy. It's the same rules as before, we're just using the lat/long as our x/y coordinates.


```r
# load ggplot2
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
map <- ggplot(data = boro)
```

```
## Regions defined for each Polygons
```

Hmm, looks like something went wrong there! This is because in order to use `ggplot2` for mapping, we must first use the `tidy` function to convert the spatial polygons data frame into a regular data frame and then rejoin the crime data! (Confusing I know...)


```r
library(broom)
library(rgeos)
```

```
## Warning: package 'rgeos' was built under R version 3.3.2
```

```
## rgeos version: 0.3-21, (SVN revision 540)
##  GEOS runtime version: 3.5.0-CAPI-1.9.0 r4084 
##  Linking to sp version: 1.2-3 
##  Polygon checking: TRUE
```

```r
library(maptools)
```

```
## Warning: package 'maptools' was built under R version 3.3.2
```

```
## Checking rgeos availability: TRUE
```

```r
gg.boro <- tidy(boro, region = "BOROUGH")
gg.boro <- dplyr::left_join(gg.boro, df.agg, by = c("id" = "BOROUGH"))
```

```
## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
## factor and character vector, coercing into character vector
```

```r
ggplot(data = gg.boro, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "dark grey", size = 0.3)
```

![](mapping_in_R_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

We're now going to look at how we can change the appearance of our map by using the functions in ggplot2, so first we'll create our basemap and then we'll tag different elements to it.


```r
map <- ggplot(data = gg.boro, aes(x = long, y = lat, group = group))
```

First let's change the default fill from black to white.


```r
w <- map + geom_polygon(aes(x = long, y = lat, group = group),
                        color = "dark grey", fill = "white")
w
```

![](mapping_in_R_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

Let's add a map projection...


```r
p <- w + coord_map("polyconic")
p
```

![](mapping_in_R_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

It's looking better - let's create a title and colour the map by the crime data


```r
p <- map + geom_polygon(color = "dark grey", size = 0.3, 
                        aes(x = long, y = lat, group = group, fill = gg.boro$Total.Crimes)) +
  labs(title = "Crime Map of London")
p
```

![](mapping_in_R_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Now we can remove the background and axis, then add the projection back in - this will make it look more like a map and less like a graph! To do this, we need to tell R that the axis elements and panel (grid/background) are blank using `element_blank()`.


```r
p + xlab("") + 
  ylab("") + 
  theme(axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  coord_map("polyconic")
```

![](mapping_in_R_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

For the final touch, let's change the colour scheme using `scale_fill_gradient` and add a legend title that looks a little bit nicer. Note that if we had discrete data, we could instead use `scale_fill_brewer` - check the help file to find out more!


```r
p + xlab("") + 
  ylab("") + 
  theme(axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  coord_map("polyconic") +
  scale_fill_gradient(low = "Yellow", 
                      high = "Red",
                      name = "Total Crimes")
```

![](mapping_in_R_files/figure-html/unnamed-chunk-26-1.png)<!-- -->


# Close

So we've seen how to convert data to a spatial data frame, import a shapefile and also how to combine the two to query your data spatially. We've then seen how to aggregate data based on common variables using `ddply` and tag it onto your spatial data using `left_join`. Using the `ggplot2` package, we then created a series of maps to visualise our aggregate data.

# Next steps

Next time we'll look at how to use our spatial data to create an interactive map using the R package `leaflet`. In my opinion, mapping using `leaflet` is much easier than trying to use `ggplot2` to do this, in part because there are fewer steps and also because the syntax is easier to read.
