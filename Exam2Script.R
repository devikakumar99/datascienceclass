getwd()

#Exam 2 R Script UTEID: dck649

#clear the environment, load the rio package, and import the inequality dataset
rm(list=ls(all=TRUE))

library(rio)
inequality_data = import("inequality.xlsx") 

#confirming that this is cross-sectional data
library(stargazer)
stargazer(inequality_data, type = "text")

#provide the subset for the inequality Gini index for Denmark and Sweden
subset(inequality_data, country == "Denmark")
subset(inequality_data, country == "Sweden")

#provide the subset for the inequality Gini index for Brazil
subset(inequality_data, country == "Brazil")

#next, we take a quick peek at the data: 
head(inequality_data)

#create an accent.remove function and apply it to Belarus
# define the function
remove.accents <- function(s) {
  #1 character subs
  old1 <- "áéú"
  new1 <- "aeu"
  s1 <- chartr(old1, new1, s)
  
  #2 character subs (hinted that it'll be here for the exam)
  old2 <- c("ß")
  new2 <- c("ss")
  s2 <- s1
  
  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
  
  s2
  
}

# finish the accent FIX
inequality_data$country = remove.accents(inequality_data$country)

#peek at the data after the change
head(inequality_data)

#sort by inequality_gini and run head() again
inequality_data = inequality_data[order(inequality_data$country), ]

head(inequality_data)

#mean of data
mean(na.omit(inequality_data$inequality_gini))


#create two variables that represent a high and low inequality_gini score:
ifelse(test = inequality_data$inequality_gini <= 36.8, yes = 0 , no = 1)
high_inequality <- ifelse(test = inequality_data$inequality_gini <= 36.8, yes = 0 , no = 1)
low_inequality <-  ifelse(test = inequality_data$inequality_gini >= 36.8, yes = 0 , no = 1)

library(doBy)
summaryBy(high_inequality ~ low_inequality, data=inequality_data, FUN=c(mean,length))

#create a for loop that names the IO actors
#create an organization vector
orgs <- c('World Bank', 'The African Development Bank', 'The Bill and Melinda Gates Foundation') 
# Create the for statement
for ( i in orgs){
  print(i) 
}

#importing directly into RSTudio
library(WDI)
highten_data = WDI(country = "all",
                      indicator = c("SI.DST.10TH.10"), # indicator from web 
                      start = 2015, end = 2015, extra = FALSE, cache = NULL)

#rename the var
#installing and loading the data.table package
library(data.table)
setnames(highten_data,"SI.DST.10TH.10", "Highest 10% Share")

#merge the datasets
library(tidyverse)
merged_df = left_join(x=inequality_data,
                        y=highten_data,
                        by =c("country", "year"))
#however, we have repeats of the country code iso2c, so we will remove them: 

#check the names of the merged data
names(merged_df)
# create countries spelling match variable using mutate
# note: mutate is a tidyverse command for create/generate # we include it here so you know how to use it library(tidyverse)
merged_df <-
  merged_df %>%
  mutate(cc_match = ifelse(iso2c.x == iso2c.y, "yes",
                                  "no"))
merged_df <- 
  merged_df %>%
  select(-c("iso2c.x")) %>% # drop iso2c.x 
  rename("iso2c" = "iso2c.y")

#now remove all the missing data 
no_NA_df <- na.omit(merged_df, select=c("inequality_gini", "Highest 10% Share"))

#filter the data
library(tidyverse)
#pipe the data through the filter like a function 
data_greater_30 <-
  merged_df %>% 
  dplyr::filter(inequality_data$inequality_gini > 30)

#count the number of "ai"
grep(data_greater_30$country, pattern = "ai")

#apply function to sum inequality_gini
sapply(data_greater_30$inequality_gini, sum)


# label the variables of merged_data
#install.packages("labelled")
library(labelled)
var_label(data_greater_30) <- list(`country` = "Country",
                               `inequality_gini` = "Gini Index Score",
                               `year` = "year",
                               `iso2c`= "ISO-2 Country Code",
                               `Highest 10% Share` = "Highest 10% Share"
                                )


#export to Stata
#save the dataset in Stata format with the labels
library(rio)
export(data_greater_30, file = "finaldata.csv")
