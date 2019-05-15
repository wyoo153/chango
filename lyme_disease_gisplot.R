# -----------------------------------
# Day 2 morning @8:30am
# -----------------------------------
#
# Packages
library(tidyverse)  # This automatically loads dplyr, ggplot, etc
library(magrittr)   # for piping
# tidyverse and magrittr are two packages mostly used
library(dplyr)  # three functions: select, gather, and mutate
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

# Case study ---------------------
#setwd("C:/Users/wyoo9/Desktop/workshop/wshop1")

# Task 1: Read in all three csv files as tibble data frames. For consistency 
# with these notes, we’ll assign their dataframes to be called “ld”, “pop” 
# and “prism”, resectively.

ld <- read.csv('lyme.csv')
pop <- read.csv('pop.csv')
prism <- read.csv('climate.csv')

# names(ld); names(pop); names(prism);
# head(lyme); head(pop); head(prism);


# Task 2: By inspecting the ‘pop’ data, and talking with your neighbors 
# and instructors, articulate in which way(s) these data fail to conform 
# to the tidy data format?

# Task 3: Before running the code, read it to see if you can work out or 
# guess what each line is doing. Before running a line of code, remind  
# yourself of the current format of pop by typing it’s name in the console  
# window and hitting return. Then run each line, one by one, and after each 
# line has been executed, look again at pop to see what it did. Once you’re 
# clear about what each line is doing, add a comment line above each code 
# line to remind you, in your own words, what the code does.

pop %<>% select(fips,starts_with("pop2")) 
# We usually do not need this step because this includes only flips
# This order to select all variables which start with "pop2"(population on 2xxx year)
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
# transpose all "pop2" variables to longitudinal format with new variables 
# of "str_year" and "size"
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
# create a new variable of year which has deleted "pop" words 
pop %<>% mutate(year=as.integer(year))
# defind "year" as an integer
# What's a code for checking variable type???
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
# why do this step?
pop %<>% mutate(fips=as.integer(fips))
# why do this step?
# The code is now in tidy data format


# Task 4: Write a code chunk to convert the Lyme disease data to tidy data format.
#
# Hint: You are going to want a column that has the full FIPS codes for each county, 
# as explained above. You should write a function that takes two arguments: state code 
# and county code. The function should return a 4 or 5 digit FIPS code (integer data type). 
# For county codes that are 1 or 2 digits, you’ll need to ‘pad’ them with the appropriate 
# number of zeros (this will require an if-else flow control). You can determine how long 
# a character string is using the str_length function of the stringr package 
# (e.g. str_length(“disease”)=7). As you might expect, the command to paste two character 
# strings together is paste (remember, you can get help with ?paste). To apply your function 
# to every cell you can use the rowwise function which is part of the dplyr package 
# (as used in the presentation example).
#
# Use ‘rename’ to rename the columns “STNAME” and “CTYNAME” with “state” and “county”,
# respectively (useful for a later join-operation for mapping)

ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,county=CTYNAME)

# Combining two variables of STCODE and CTYCODE
# Original code:
#fipcode <- function(st, ct){
#  if(str_length(ct) == 3){
#    fips <- paste(st, ct,sep="")
#  }
#  else if(str_length(ct) == 2){
#   fips <- paste(st, "0", ct,sep="")  
#  }
#  else{
#    fips <- paste(st, "00", ct,sep="")     
#  }
# return(fips)
#}

fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE)) # takes about 10 seconds
ld %<>% select(-c(STCODE,CTYCODE,str_year))  # delete three variables


# Task 5 (Combining data sets): Join the Lyme disease data frame and PRISM (climate) 
# data frame together to form a new data frame, and in such a way that it retains 
# county-year combinations for which we have both disease and clime data.

ld.prism<- inner_join(ld,prism)
names(lymeclim)
dim(lymeclim)

# Task 6: Write a line of code to additionally combine the demographic data 
# with the Lyme disease and climate data.

ld.prism.pop<-inner_join(ld.prism,pop)

# Task 7: (Obtaining summary information)  Write two lines of code that create 
# two new data frames: 
# (1) to determine how many cases of Lyme disease were reported each year, 
# (2) the average number of cases in each state - averaged across county and year. 
# What was the worst year? Which three states have been most impacted on average?

# (1)
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
cases_by_year
# (2)
avCases_by_state <- ld %>% ungroup %>% group_by(state) %>%
  summarize(avCases=mean(cases)) %>% arrange(desc(avCases))
avCases_by_state

# Task 8: use save to create an Rda file of the data frame and use write_csv 
# to create a csv file of the same (remember to try ?save and ?write_csv 
# in the console if you’re not sure how these functions work).

write_csv(ld.prism.pop,"LdPrismPop.csv")
save(ld.prism.pop,file="get_LdPrismPop.Rda")


# Task 9: Add annotations to the following lines of code with comment lines 
# to explain what they are achieving.
# Note: in the code line with “group_by(ld.prism.pop)” you need to replace
# the data frame in parentheses with the name of your data frame in which 
# you combined Lyme disease, climate and demography data (Task 6)

# Using FIPS and built-in mapping tools to visualize geographic data
# get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")

## ANNOTATE FROM HERE
ag.fips <- group_by(ld.prism.pop,fips)
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))


