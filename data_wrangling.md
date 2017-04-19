---
title: "Data Wrangling Vol. II"
author: "Alex Drake"
date: "15 March 2017"
output: 
  html_document:
    theme: yeti
    number_sections: yes
    toc: yes
    toc_float: true
---

# Introduction

We've already seen how to import a csv file and do some basic data wranglig/manipulation so now we're going to look at it in a bit more detail. For this course you're going to need to install a few more packages and request access to the [Transport for London Unified API](https://api.tfl.gov.uk/) so we can request some data.

Note that the code to retrieve data from the TfL API was featured in an TfLR LeaRn Together in July 2016.

## Required Packages

Please make sure to install the following packages before we start!

Some of you may recall `tidyverse` from a recent TFL lunchtime session by Ewen Henderson, if not then please read through his notes for a bit more information.

```{r, echo=T, eval=FALSE}
# package list
packages <- c("jsonlite", "tidyverse")

# install packages and any others required to run them
install.packages(packages,
                 dependencies = TRUE)
```

## The TfL APIs

You can access the TFL API through the link in the introduction but in order to use it (ie request data) you'll need to register and get an app id and key. You can do that [here](https://api-portal.tfl.gov.uk/login) - it's pretty quick and painless but I still recommend doing this before starting the session! You can then view your id/key on the 'API Credentials' page.

# Getting the data (Or How To Get Around Your Corporate Firewall)
## API Call

To make a call to the TfL API, we have to form a URL with our query. For this we need to know the structure for the request, which you can see on the [unified API website](https://api.tfl.gov.uk/swagger/ui/index.html?url=/swagger/docs/v1#!/AccidentStats/AccidentStats_Get), your search parameters and your app id and key.

We're going to request some collision data for 2015 as an example, the call structure is below:

```{r, eval=F}
library(tidyverse)
library(jsonlite)

year <- "2015"
AppId <- "your app id here"
AppKey <- "your app key here"

requestUrl <- paste0("https://api-argon.tfl.gov.uk/AccidentStats/", year, "?app_id=", AppId, "&app_key=", AppKey)
```

The `requestUrl` should therefore return the collisions data for 2015 in JSON format. You can see what this looks like by copying and pasting the URL you just made into your browser (messy!). We now need to read that into R in a format that we can understand...

## Reading data from APIs

There are plenty of ways to do this but the recommended package for use with tidyverse is `jsonlite`. It is a "fast JSON parser and generator optimized for statistical data and the web". We can therefore use this to parse in JSON data returned by the TfL API call. If you were to try this at home you could do it in one simple step:

```{r, eval=FALSE}
d <- fromJSON(requestUrl)
```

However, trying this on a corporate computer might give you a connection error due to the firewall.

![](img/no_entry.jpg)

Luckily there is a work around so that we can still get our data and all it needs is another line of code!

```{r, echo=T}
l <- readLines(requestUrl, encoding="UTF-8", warn = FALSE)
d <- fromJSON(l)
```

You should get something that looks like this:

```{r, echo=F, message=F, warning=F}
library(knitr, quietly = T)
kable(head(d,4))
```

# Data Manipulation
## Data Cleansing

We've just read the data into our workspace but some of the columns look a bit funky. For now, let's ignore the 'casualties' and 'vehicles' columns - we can remove these columns using the `select()` from `dplyr`.

```{r, echo=T}
d_clean <- d %>% select(id, lat, lon, location, date, severity, borough)
```

We may also want to clean the date column as it's in a strange format. We can do this a few different ways - the first relies on us telling `as.Date()` what format the date is currently in, and then forming a new (readable!) date from this. Whilst we're at it, let's create an hour column too using `format()` and `strptime()`.

```{r, echo=T}
# cleaned date
d_clean$myDate <- as.Date(d_clean$date, 
                          format = "%Y-%m-%dT%H:%M:%SZ") # current date format

# hour
d_clean$Hour <- as.numeric(format(strptime(d_clean$date, 
                                format = "%Y-%m-%dT%H:%M:%SZ"), # current date format
                       format = "%H")) # new format
```

We could also strip out the date using the `substr()` and stating which positions within the string we want to extract, counting from the right. This isn't perhaps the best way to deal with a date-time but you can see how it may be useful for other types of data.

```{r, echo=T, eval=F}
# cleaned date
d_clean$myDate <- substr(d_clean$date, 
                         1, # start position
                         10) # end position
d_clean$myDate <- as.Date(d_clean$date,
                          format = "%Y-%m-%d")

```

## Sorting a data frame

If you look at the new data frame using `View()` you'll notice that the collisions are not in order by date. Let's look at changing this so we have the oldest incidents at the top, and the most recent at the bottom. For this, we'll need to use the function `order()`.

```{r, echo=T}
# sort by date
d_clean <- d_clean[order(d_clean$myDate), ]
```

We can also do a reverse sort by using the argument `decreasing=TRUE`.

```{r, echo=T, eval=F}
d_clean <- d_clean[order(d_clean$myDate, decreasing = TRUE), ]
```

We can sort the data frame by multiple columns by declaring these in our call - a sensible option might be to sort by data AND hour.

```{r, echo=T}
d_clean <- d_clean[order(d_clean$myDate, d_clean$Hour), ]
```

```{r, echo=F, message=F, warning=F}
kable(head(d_clean,3))
```

## Data summaries

Perhaps we wanted to know how many incidents had happened per Borough and at what severity. We can do this quite simply by using the `summarise()` function and pairing it with the `group_by()` function.

```{r, echo=T}
mySummary <- d_clean %>%
                group_by(borough, severity) %>%
                summarise(count = length(severity))
```

We can take this further by limiting which Boroughs we are interested in. We do this by subsetting the data using the `%in%` operator as follows:

```{r, echo=T}
boroughs <- c("City of London", "Southwark", "Tower Hamlets")

mySummary <- d_clean[d_clean$borough %in% boroughs,] %>%
                group_by(borough, severity) %>%
                summarise(count = length(severity))
```

Note that we can carry out many other summary functions such as `mean`, `max` etc by placing them within the `summarise()` function.

## Logical Subsetting

We can use logical subsetting to extract rows from a data frame where certain conditions are met. Let's recreate our Borough summary and then have a look at the Boroughs where there have been 5 or more fatal incidents.

```{r, echo=T}
mySummary <- d_clean %>%
                group_by(borough, severity) %>%
                summarise(count = length(severity))
df <- mySummary[mySummary$severity == "Fatal" & mySummary$count >= 5,]
```

```{r, echo=F}
kable(df)
```

# Loops in R

A loop is a way to repeat a sequence of instructions under specific conditions. You can therefore use them to automate parts of your code where repetition is needed (as otherwise it could get quite painful to do manually!).

Note that you will read plenty of sites that tell you **NOT** to use loops in R - this is because R supports vectorisation which allows for much faster calculation using the `apply` suite of functions such as `lapply` and `sapply`. However, it is important to have an understanding of loops and how to write them.

It should make more sense when we have a look at a practical example so let's revisit the casualty data that we previously ignored. Let's select the appropriate colums and then create a `for` loop to pull through the mode used by the casualty. 

Note that we'll need to declare the correct list element by name in our loop. We also want the loop to work on every row in the data frame and unpack the list line by line, hence the `for(i in 1:length(df))` below.

```{r, echo=T}
# select required columns
d_clean <- d %>% select(id, lat, lon, location, date, severity, borough, casualties)

# create an empty column for mode
d_clean$mode <- NA

for(i in 1:length(d_clean$id)){
  d_clean$mode[i] <- d_clean$casualties[[i]]["mode"]
}
```

As you can see from the results, we've unpacked the first list but created another one in the process due to the way casualties are recorded. Perhaps using the `apply` family will help?

```{r, echo=T}
d_clean$mode2 <- lapply(d_clean$casualties, "[[", "mode")
```

It looks as though `lapply` is doing the same thing, although more quickly than our `for` loop. We can have a look at how to unpack these additional elements in both instances. For the following examples we'll need to use the function `unlist` which will create a vector out of list elements, and then `paste` so we can turn that vector into a character string. If we didn't do this we'd end up with a vector that has more elements than rows in our original data frame = *COMPUTER SAYS NO!*

```{r, echo=T}
# clear the mode column
d_clean$mode <- NA

for(i in 1:length(d_clean$id)){
  d_clean$mode[i] <- paste(unlist(d_clean$casualties[[i]]["mode"]), collapse = ", ")
}
```

Now let's repeat using `lapply`. If you read the help file, you'll notice that `lapply` will accept custom functions - we can use this for our `paste(unlist(...))` function so we avoid having to make another `for` loop.

```{r, echo=T}
# clear the mode2 column
d_clean$mode2 <- NA

d_clean$mode2 <- lapply(d_clean$casualties, "[[", "mode")
d_clean$mode2 <- sapply(d_clean$mode2, function(x) paste(unlist(x), collapse = ", "))
```

```{r, echo=F}
kable(head(d_clean,6))
```

You should notice that using `lapply` is *slightly* faster than our `for` loop. Check out `?lapply` for more details to find out the specifics for each member of the `apply` family.

Well done, you've extracted some data from the TfL API, cleaned it and carried out some basic summary functions! 

For further information please see the cheatsheet on data wrangling [here](file://///onelondon.tfl.local/shared/rnperformance/R-DEV/04 Cheat Sheets and Guides/dplry and tidyr Cheatsheet.pdf)
