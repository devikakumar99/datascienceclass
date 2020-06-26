getwd()
setwd("/Users/devikakumar/Desktop/RESEARCH/DataSci")

#MODULE 8 DATA CLEANING WITH CROSS SECTIONAL DATA

#We can also take a quick peak our data:
  #country primary_enroll  year
rm(list=ls(all=TRUE))
library(rio)
education_data = import("education.xlsx", which = 1) # first tab

#View education
head(education_data)

#correct misspellings
education_data$country[education_data$country=="Argentinaa"] = "Argentina" 
education_data$country[education_data$country=="Afghanistanae"] = "Afghanistan"
#View again with the changes
head(education_data)

#check the class of year
class(education_data$year)
#correcting the strange H in the Year
education_data$year[education_data$year=="2015H"] <- "2015"
#NOW you can change the class of the year
education_data$year = as.numeric(education_data$year) 
class(education_data$year)

#NEXT, sort the data using order
education_data = education_data[order(education_data$country),]
head(education_data)

#MODULE 8.3 ACCENTS AND FILE ENCODING

#changing the file encoding before dealing with the accents

# define the function
remove.accents <- function(s) {
  #1 character subs
  old1 <- "áé"
  new1 <- "ae"
  s1 <- chartr(old1, new1, s)
  
  #2 character subs (hinted that it'll be here for the exam)
  old2 <- c("ß")
  new2 <- c("ss")
  s2 <- s1

  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
  
  s2
    
}

# finish the accent FIX
education_data$country = remove.accents(education_data$country)

#correct the spelling for Algeria
education_data$country[education_data$country=="Algerie"] = "Algeria" 
head(education_data)

#check the class of these vars
class(education_data$country)
class(education_data$primary_enroll)
class(education_data$year)

#MODULE 8.4 SUMMARY AND STARGAZER REVIEW 
summary(education_data)
#for cleaner summary statistics (something you could output on RMarkdown)
library(stargazer)
stargazer(education_data, type = "text")

#MODULE 8.5 ADDING COUNTRY CODES

#loading the country code package with the ISO-3 country codes
library(countrycode)
education_data$country_code = countrycode(sourcevar = education_data$country,
                                          origin = "country.name",
                                          destination = "iso3c",
                                          warn = TRUE)
#it couldn't find Channel Islands and Kosovo
#add the country code for Kosovo
education_data$country_code[education_data$country == "Kosovo"] = "RKS"

#MODULE 8.6 ADDING WDI DATA DIRECTLY INTO R

#social and economic data from the World Bank that we can use directly in R
#this is the package we need to import the world development data
library(WDI)
# After Googling: https://data.worldbank.org/indicator/SP.POP.TOTL 
#the concatenate works even if you don't have multiple indicators
population_data = WDI(country = "all",
indicator = c("SP.POP.TOTL"), # indicator from web 
start = 2015, end = 2015, extra = FALSE, cache = NULL)

#MODULE 8.7 RENAMING VARIABLES

#installing and loading the data.table package
library(data.table)
setnames(population_data,"SP.POP.TOTL", "population")

#MODULE 8.8 FILTERING AND PIPING/country code redux
library(countrycode)
population_data$country_code = countrycode(sourcevar = population_data$iso2c,
                                           origin = "iso2c",
                                           destination = "iso3c",
                                           warn = TRUE)

# returns lots of warnings so lets view population_data
View(population_data)

#so to solve this, let's run a filter command:
library(tidyverse)
#pipe the data through the filter like a function **IMPORTANT**
population_data <-
  population_data %>% 
  dplyr::filter(!(country_code=="NA"))

#check that everythign went through with subset
subset(population_data, country_code == "NA")

#MODULE 8.9 MERGING DROPPING VARIABLES, + MORE TIDYVERSE
#merging dataframes using left_join in dplyr
library(tidyverse)
merged_data = left_join(x=population_data,
                        y=education_data,
                        by =c("country_code", "year"))

#but then we have a country_x and a country_y which we need to resolve
#check the names of the merged data
names(merged_data)
# create countries spelling match variable using mutate
# note: mutate is a tidyverse command for create/generate # we include it here so you know how to use it library(tidyverse)
merged_data <-
  merged_data %>%
  mutate(countries_match = ifelse(country.x == country.y, "yes",
                                  "no"))
# list the instances in which the spelling doesn't match
subset(merged_data, countries_match =="no")

#drop country.x, and rename country.y as country
merged_data <- 
  merged_data %>%
  select(-c("country.x")) %>% # drop country.x 
  rename("country" = "country.y")

#sort the variables again
library(dplyr) 
merged_data <-
  merged_data %>%
  relocate("country", "iso2c", "country_code", "year", "primary_enroll", "population")

#at this point you can remove the education and pop data frames
rm(population_data,education_data)

#MODULE 8.10 TAKING THE LOG/BASIC ANALYSIS
#check for NAs
table(merged_data$population, exclude = TRUE)
table(merged_data$primary_enroll, exclude = TRUE)
subset(merged_data, is.na(population))
subset(merged_data, is.na(primary_enroll))

#finding correlation as we learned earlier
cor(merged_data$primary_enroll, merged_data$population, use = complete.obs)

#another way we could do that
data_no_NAs <- na.omit(merged_data, select=c("primary_enroll", "population"))
cor(data_no_NAs$primary_enroll, data_no_NAs$population)

#use a simple ggplot2 to visualize this data
library(ggplot2) # also part of tidyverse
scatterplot = ggplot(data = data_no_NAs, aes(x=population, y=population)) +
  geom_point(stat = "identity") + labs(x= "",
                                       y= "Population (Inhabitants)",
                                       title = "Distribution of Population Data") 
  print(scatterplot)

#we can see from the plot that we have outliers
  
subset(data_no_NAs, country=="China")
subset(data_no_NAs, country=="Luxembourg")

#NEXT, create a variable that represents the log of the population to change the scale
#as a safety net you can take the log and 1 to remove any undefineds
data_no_NAs$log_population = log(data_no_NAs$population + 1)

library(ggplot2) # also part of tidyverse
scatterplot2 = ggplot(data = data_no_NAs, aes(x=log_population, y=log_population)) +
  geom_point(stat = "identity") + labs(x= "",
                                       y= "Log Population (Inhabitants)",
                                       title = "Log Distribution of Population Data") 
print(scatterplot2)
#run the corr again
cor(data_no_NAs$log_population, data_no_NAs$primary_enroll)

#MODULE 8.11 LABELING THE VARIABLES AND SAVING THE LABELED DF
library(labelled)

# label the variables of merged_data
#install.packages("labelled")
library(labelled)
var_label(data_no_NAs) <- list(`country` = "Country",
                               `year` = "year",
                               `primary_enroll` = "Gross Primary Enrollment Rate",
                               `population` = "Population (No. of Inhabitants)",
                               `iso2c`= "ISO-2 Country Code",
                               `country_code` = "ISO-3 Country Code")

# save the dataset in Stata format with the labels
library(rio)
export(data_no_NAs, file = "clean_dataset.dta")
export(data_no_NAs, file = "clean_dataset.csv")



  
