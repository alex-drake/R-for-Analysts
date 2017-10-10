---
title: "Introduction to ![](img/logo.png)"
author: "Alex Drake"
date: "14 October 2016"
output: 
  html_document: 
    number_sections: yes
    toc: yes
---

# Introduction

Welcome to the OIA introduction to R and RStudio. As with the introduction to SQL, the content is provided in sections, with each section introducing some new ideas.

This course assumes some basic R knowledge but please do ask if you haven't understood anything!

Please make sure you have access to R and RStudio - these can be obtained via the [IM Service Request Area.](http://prodwebremedy:8080/arsys/forms/prodappremedy.onelondon.tfl.local/SRS%3AServiceRequestConsole/enduser/)

## R Documentation

There are many online resources available if you get stuck with R. Some key examples are shown below.

[Cookbook for R](http://www.cookbook-r.com/)

[Advanced R](http://adv-r.had.co.nz/)

[TfL R Users](http://onelink.tfl.gov.uk/sites/IRUF/ranalytics/SitePages/Home.aspx)

Also remember that Google is your best friend (other search engines are available) - if you have any issues it is likely that somebody else has encountered these problems before you so search for it and see if the online community can be of assistance!

## An example R command

```{r, eval=FALSE}
summary(cars)
```

The above code will create a summary for the table within R called 'cars'. We're going to concentrate on a few of our own datasets, as previously covered in the SQL course.

# Start using R

First we want to create a working directory so we know where our data and scripts are stored. To do this, create a folder on your U: drive called "R Session 1".

Open up RStudio and then open a new script window (see image). We will now type all of our commands into the script window so we have a record of them for later use!

Top tip: open a secondary script window and use this as a 'key code' directory. You can then store any useful bits of code (with a generic description) so you can reference them later on.

![](img/r_script.png)

```{r, eval=FALSE}
setwd("U:/R Session 1/")
```

Notice that we have used a forward slash within the filepath rather than the backward slash usually seen. If we wanted, we could have used a double backward slash - use whichever suits you (we will use forward slash within this document).

## Installing a package

R has a number of base functions available without having to install any additional packages. However, if you want to make your life as easy as possible then it's always worth installing a few. This is done by using the install.packages function. A few key ones are detailed below:

```{r, eval=FALSE}
install.packages("dplyr") # for data manipulation beyond plyr
install.packages("ggplot2") # for graphs with a bit more swagger
install.packages("plyr") # for data manipulation
install.packages("RColorBrewer") # for defining your own colour palettes
install.packages("RJDBC") # provides access to databases through JDBC interface
install.packages("rmarkdown") # for writing documents like this!
```

Don't worry about installing them all today - we'll only be using RJDBC (so make sure you *HAVE* installed that one!)

## Reading .csv data

There are several different ways to read data into the R environment. The first one we'll look at is read.csv(). This is part of the base package so don't worry about installing any extra items.

You should have received a link to a file called 'lcap 01-09-2016.csv'. Save a copy of this to your working directory so you have your own version to work from.

Now let's load it into R following these steps:

Tell R where to look by creating the file path (use the paste function). Note that as we have already defined the working directory, we should be able to ignore this step. However, it is useful in the longterm to know how to do this!

```{r, eval=FALSE}
myfile <- paste0("U:/R Session 1/lcap 01-09-2016.csv")
```

Now let's read the file into the environment and have a look. 

```{r, eval=FALSE}
myfile <- read.csv(myfile, header = FALSE)

View(myfile)
```

Notice we have told R that our data doesn't have headers - by default, R assumes your data has no headers but it is good practice to pass through the information anyway.

As we have set the working directory, we can also called the following.

```{r, eval=FALSE}
myfile <- read.csv("lcap 01-09-2016.csv", header = FALSE)

View(myfile)
```

Well, this doesn't look quite right! We need to pass more information into the read.csv function to make sure our data comes in as intended.

Let's call read.csv again, but this time add a bit more information.

```{r, eval=FALSE}
myfile <- read.csv("lcap 01-09-2016.csv", 
                   header = FALSE, 
                   sep = "\t", 
                   row.names = NULL,
                   quote = "")
```

Good, this looks better although the column names aren't particularly helpful. If there was a way to specify the column names on import then we would be able to tell which columns are which straight away... 

### Challenge 

Have a look through the help file by typing '?read.csv' into the console and see if you can add column names during import. The names are as follows:
Date, Capture Time, Vehicle Group, Link ID, Patch Type, Duration, Observations

*Solution*
```{r, eval=FALSE}
myfile <- read.csv("lcap 01-09-2016.csv",
                   header = FALSE,
                   sep = "\t",
                   row.names = NULL,
                   quote = "",
                   col.names = c("Date","CapTime","VehGroup","LinkID","PatchType","Duration","Observation"))
```

## Can I use other sources? 
This is great but the downside is that we'd need to create csv files for a lot of data. 

Note that we could also import data from MS Excel using other packages but we won't cover that here as it can be quite clunky/slow.  If you really do need to import from MS Excel then consider using the 'XLConnect' or 'xlsx' packages and ask for help if needed.

There must be a more efficient way to do this, if only we could connect to a database like we did for the SQL session...

# Connect R to a database
## Set-up the connection

The following section will set up the connection information so that we can go into Oracle and extract lcap data directly from there (no csvs!). In order to do this in the R environment, we must first install and load the following package. Notice that we must use the call 'library' to load the package into the R environment - you can also use the 'require' call but we'll stick with 'library' here as they have slightly different uses.

```{r, eval=FALSE}
install.packages("RJDBC")

library(RJDBC)
```

We now need to tell the RJDBC package where to look (as we did at the start of the SQL course). We don't have a lovely GUI for this section but we can write it into our script as follows.

```{r, eval=FALSE}

drv <- JDBC(driverClass = "oracle.jdbc.OracleDriver",
              classPath = "Q:/R-DEV/02 Tools and Functions/Data Warehouse/ojdbc5.jar",
              " ")
  
# Authorisation arguments:
host <- "" # Our host
port <- "" # Port
dsn <- "" # Service Name
conn <- paste0("jdbc:oracle:thin:@", host, ":", port, ":", dsn) # Our connection information
uid <-"" # Username
pwd <- "" # Password

```

Don't worry about how the above works, just remember that it is doing the equivalent of the new/select database connection in oracle (below).

![](img/connection.png)


## Define the query

Let's tell R which date we require by creating a new variable. Remember that the lcap database likes dates in the format *dd-mmm-yyyy* so we will write *01-09-2016* as *01-Sep-2016*.

The simplest way to provide the date is shown below. Note that R will recognise this as a string and not a date.

```{r, eval=FALSE}
mydate <- "01-Sep-2016"
```

We could also have done this using code and specifying the formats. If you know the format codes then it is quite simple to change between formats! Let's have a look before moving on. 

First tell R that we are dealing with a date and the format that we are providing it in.

```{r, eval=FALSE}
mydate <- "01-09-2016"
mydate <- as.Date(mydate, format = "%d-%m-%Y")
```

Look at the variable **mydate** to see how R has changed it to yyyy-mm-dd

Now let's tell R to change it again so it is in the format required by the lcap database. We can use the latest **mydate** output for this.

```{r, eval=FALSE}
mydate <- format(mydate, "%d-%b-%Y")
```

Look at **mydate** again and see how it now looks like the string we typed earlier. If you have time, look in the appendix for further format codes.

### Challenge
Try manipulating the dates as below using a combination of 'format' and 'as.Date'. Look at the dates to determine what the initial format is and what you need to to tell R to change it to!

* 01/09/2016 to 2016-09-01
* 01/09/2016 to 01-Sep-16
* What day of the week was 01/09/2016?

*Solutions*
```{r, eval=FALSE}
#1
date1 <- format(as.Date("01/09/2016", format = "%d/%m/%Y"), "%Y-%m-%d")

#2
date2 <- format(as.Date("01/09/2016", format = "%d/%m/%Y"), "%d-%b-%y")

#3
date3 <- format(as.Date("01/09/2016", format = "%d/%m/%Y"), "%A")
```

We've sorted the date so let's carry on with the extract. First, let's use our SQL knowledge to write a query that will extract all LCAP data for the date in question. 

Remember your SELECT queries!

```{r, eval=FALSE}
lcap <- paste0("Select * from SLOW_TABLE4 WHERE CAPTUREDATE = '",mydate,"'")
```

By using paste0 we have created a string that includes our query, and the date we want to extract data for. Prove this to yourself by typing **lcap** into the console and hitting 'Return' - it looks like our SQL queries! 

## Get the data

Now we need to connect to the database and send our query using functions from the RJDBC package. First we will connect (dbConnect) and then send the query using the dbGetQuery functions. 

Note that in the R environment, it is always good practice to close the connection once we've finished with it. We can do this with the dbDisconnect function.

```{r, eval=FALSE}
# connect to the db
conn <- dbConnect(drv, conn, uid, pwd)

# query and pull the data into R
lcap <- dbGetQuery(conn,lcap)

# close the connection
dbDisconnect(conn)
```

Well done, you've now extracted your data directly from Oracle into the R environment without having to make any interim .csv files!

### Challenge
Compare **myfile** to **lcap** - what differences are there? Which elements are the same?

# Basic data manipulation in R

We now have some data so let's remind ourselves how to select data by using conditions. Note that although the process is similar to the SQL queries we used to extract the data, the base R syntax is different! 

## Help!
If you struggle to get to grips with the R syntax then try using a package that allows you to use SQL in the R environment. Remember to search on Google or StackOverflow etc. to determine which packages best suit your needs.

### Challenge 
* Can you find a package that will allow you to use SQL queries in the R environment?

```{r, eval=FALSE}
# install package
install.packages("sqldf")

# load package into the R environment
library(sqldf)
```

## Using R for SELECT and WHERE type queries

Let's look at a few basic queries using the **lcap** dataframe we've just extracted. We don't want to overwrite the **lcap** data as we'll be using this a few times - let's create a new dataframe instead (here we'll call it **df**).

We'll first look at the data for link 882 only. In SQL this would be written as:

```{r, eval=FALSE}
SELECT * FROM lcap WHERE linkid = 882;
```

Instead, we want to use the R syntax - remember to use a comma after the condition so R knows we want all columns.

```{r, eval=FALSE}
df <- lcap[lcap$LINKID == 882, ]
```

Hopefully you should now have a dataframe called **df** with 8 columns and 575 rows. Now let's have a look at selecting the journey time data for link 882. This means we want to select the column 'Duration' from the **lcap** table where the link ID is equal to 882.

Look at the statement below - can you spot the difference with the above? 

```{r, eval=FALSE}
df <- lcap$DURATION[lcap$LINKID == 882]
```

We can also extract the Duration information by passing the column name into the square brackets. Bonus points if you can see why this might be helpful later...

```{r, eval=FALSE}
df <- lcap[lcap$LINKID == 882, "DURATION"]
```

Great, we now have several ways to extract a column of data but with no context to what it actually is. Perhaps it would be better to select *multiple* columns...

### Challenge: 
* Extract the journey times and capture time for link 882
* Extract the journey times, capture time and observations for 882 where patchtype is less than 2 (HINT: see appendix for logic operators)

```{r, eval=FALSE}
# 1
df <- lcap[lcap$LINKID == 882, c("DURATION","CAPTURETIME")]

# 2
df <- lcap[lcap$LINKID == 882 & lcap$PATCHTYPE < 2, c("DURATION", "CAPTURETIME", "OBSERVATIONS")]
```

### Stretch Challenge:
If you have time, try a few of these challenges. They're meant to be slightly more difficult than the previous ones so don't worry if you get stuck - you can always ask for help!

* Using your last extract, show the summary statistics for the data.
* Create a dataframe for link 1736, removing vehicle group '1001'. You can do this in two stages but try to do it in one!
* Calculate the mean journey time for link 882.
* Determine the maximum journey time during the AM peak for link 2240 (hint: capture time should be between 25200 and 36000).

```{r, eval=FALSE}
# 1
summary(df)

# 2
df <- lcap[lcap$LINKID == 1736 & lcap$VEHICLEGROUP != 1001,]

# 3
mean(lcap$DURATION[lcap$LINKID==882])

# 4
max(lcap$DURATION[lcap$LINKID==2240 & lcap$CAPTURETIME >= 25200 & lcap$CAPTURETIME < 36000])

# or

max(lcap[lcap$LINKID==2240 & lcap$CAPTURETIME >= 25200 & lcap$CAPTURETIME < 36000,
         "DURATION"])
```

***

![](img/party.jpg)

Well done, you've survived the introduction to R/RStudio session! Next time we'll move onto more complex commands and functions so please try to install the recommended packages before then. 

Get ready for some more data wrangling!

***


# Appendix

## Logical Operators
Operator | Description
---------|------------
<        |less than
<=       |less than or equal to
>        |greater than
>=       |greater than or equal to
==       |exactly equal to
!=       |not equal to
!x       |not x
x | y    |x OR y
x & y    |x AND y
is.na    |Is NA
!is.na   |Is not NA
%in%     |Group membership (is in)

Note that the 'IN' operation is written as follows...
```{r, eval=FALSE}
x %in% c(a, b, c)
```
...and Is not IN is written like this...
```{r, eval=FALSE}
!(x %in% c(a, b, c))
```
...but we'll look at this in more detail later.

## Date formats
![](img/dateformats.png)
