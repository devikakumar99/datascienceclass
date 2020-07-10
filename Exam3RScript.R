
###EXAM 3 RSCRIPT
#Devika Kumar, UTEID: dck649
#im so sorry if this is a complete mess :(

setwd("~/Desktop/RESEARCH/DataSci")

#clear the environment
rm(list=ls(all=TRUE))

#load the tidycensus data
library(tidycensus)

#my API key: 7762cd9c0921e309bd9f445bf349535d18411b34
census_api_key("7762cd9c0921e309bd9f445bf349535d18411b34", 
               install = TRUE,
               overwrite = TRUE)

#restart R session 
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#import the ACS data for 2010
v10 <- load_variables(year = 2010,
                      "acs5") 

#import the ACS data for 2015
v15 <- load_variables(year = 2015,
                      "acs5") # tell it the dataset you want

#import in the state-level inequality Gini estimates for 2010 and 2015
gini10 <- get_acs(geography = "state",
                  variables = c(gini = c("B19083_001")),
                 year = 2010)

gini15 <- get_acs(geography = "state",
                  variables = c(gini = c("B19083_001")),
                  year = 2015)

#add a year variable
gini10$year = "2010"
gini15$year = "2015"

#append gini10 and gini15 into a single panel dataset
library(tidyverse)
inequality_panel <- bind_rows(gini10, gini15)

#rename the variables
library(data.table)
setnames(inequality_panel, "estimate", "gini")
setnames(inequality_panel, "NAME", "state")

#take a peek at the data
head(inequality_panel)

#reshape the data into a wide format
inequality_wide <- inequality_panel %>%
  pivot_wider(id_cols = c("GEOID", "state", "year"), # unique IDs
              names_from = "year", # names for new wide vars
              values_from = "gini", #data to put in the new wide vars
              names_prefix= "year_") #prefix to add before years (bad practice to have numeric)

#take a peek at the data
head(inequality_wide)


#reshape back into long data
inequality_long <-
  inequality_wide %>%
  pivot_longer(cols = starts_with("year"), # use columns starting with "year" (from dplyr)
               names_to ="year", # name of new column
               names_prefix = "year_", # part of string to drop
               values_to = "gini", # where to put numeric values 
               values_drop_na = FALSE) # don't drop NAs


#take a peek at the data
head(inequality_long)


#do inequality_wide and inequality long have the same number of observations? 
dim(inequality_long)
dim(inequality_wide)

#create inequality_collapsed which is collapsed by the mean

inequality_collapsed <-
  inequality_long %>%
  group_by(GEOID, state, year, gini) %>% # tell R the unique IDs 
  summarize(, mean)) # summarize numeric vars by mean





#WDI package and the GDP variable
library(WDI)
gdp_current = WDI(country = "all",
                   indicator = c("NY.GDP.MKTP.CD"), # indicator from web 
                   start = 2015, end = 2015, extra = FALSE, cache = NULL)

#use the World Bank's GDP deflator
# https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS
deflator_data = WDI(country = "all", indicator = c("NY.GDP.DEFL.ZS"),
                    start = 2010, # start of foreign aid data 
                    end = 2010, # end of of foreign aid data 
                    extra = FALSE, cache = NULL)
#rename the deflator variable
library(data.table)
setnames(deflator_data, "NY.GDP.DEFL.ZS", "deflator")

#since we are using USD we only select the US Data
#remember that 100 = the base year, which varies per country
usd_deflator = subset(deflator_data, country == "United States")

#clean up the environment
rm(deflator_data)
#drop unnecessary variables
usd_deflator$iso2c = NULL
usd_deflator$country = NULL

#merge the data frames
deflated_data = left_join(gdp_current,
                          usd_deflator, 
                          by = c("year"))
#NEXT, actually DEFLATE the data
deflated_data$deflated_amount = deflated_data$gdp_current/ 
  (deflated_data$deflator/100)

head(deflated_data)



#PDF text analysis:
#MODULE 14.5 PDF DATA FROM THE WEB
library(pdftools)
library(tidyr) 
library(tidytext) 
library(dplyr) 
library(stringr) 
library(ggplot2)

#online pdf file
armeniatext = pdf_text(pdf = "https://pdf.usaid.gov/pdf_docs/PA00TNMG.pdf")
#my text is a character vector
armeniatext


#turn the text into a data frame
armeniatext =as.data.frame(armeniatext, stringsAsFactors=FALSE)
armeniatext$page=c(1:65)
colnames(armeniatext)[which(names(armeniatext) == "armeniatext")] <- "text" #change the column name

#in order to tokenize text into words:
armeniatext = armeniatext %>% 
  unnest_tokens(word, text)

#in order to get rid of stop words:
data(stop_words)
armeniatext <- armeniatext %>% 
  anti_join(stop_words)

#top 5 most used words
armeniafreq <- armeniatext %>% 
  count(word, sort = TRUE)

head(armeniafreq, 5)


#top100fromBillboard
#load the appropriate packages for webscraping
library(rvest)
library(dplyr)
library(ggplot2)

#scraping the billboard hot 100: read in the page
hot100page <- "https://www.billboard.com/charts/hot-100" 
hot100exam <- read_html(hot100page)

#quick peek at the data
print(hot100exam)
str(hot100exam)

#using the rvest package to diagnose the different nodes contained in hot100
body_nodes <- hot100exam %>% 
  html_node("body") %>% 
  html_children() #this helps you see inside the nodes

#see all the "div class" are all the elements and we'll drill further into this
print(body_nodes)

#extract the specific information that you want and turn them into R objects
#Billboard Top 100 
# *****THIS IS THE KEY CODE FOR SCRAPING!!*****

#get the rank and notice the syntax
rank <- hot100exam %>%
  rvest::html_nodes('body') %>% xml2::xml_find_all("//span[contains(@class,
'chart-element__rank__number')]") %>%
  rvest::html_text()

#get the artist
artist <- hot100exam %>% rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class,
'chart-element__information__artist')]") %>%
  rvest::html_text()

#get the title
title <- hot100exam %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class,
                    'chart-element__information__song')]") %>%
  rvest::html_text()

#get the "last week info"
last_week <- hot100exam %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class,
                    'chart-element__information__delta__text text--last')]") %>%
  rvest::html_text()

#combine them into a dataframe
chart_df <- data.frame(rank, title, artist, last_week)

#you can use stargazer or RMD knitr to make a nice table
knitr::kable(
  chart_df %>% head(10))

View(chart_df)















