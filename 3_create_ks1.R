setwd("C:/Users/ASUS/Documents/GitHub/ECHILD_Synthetic")

if(!file.exists("2_spine.csv") & !file.exists("1_people.csv")){
  source("2_spine.R")
}else{
  people <- read.csv("1_people.csv")
  
  people$KS1_ACADYR <- "2010/2011"
  people$KS1_YEAROFBIRTH <- ifelse(people$month_of_birth %in% c("sep","oct","nov","dec"),2004,2005 )
  
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "jan", 1, NA)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "feb", 2, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "mar", 3, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "apr", 4, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "may", 5, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "jun", 6, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "jul", 7, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "aug", 8, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "sep", 9, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "oct", 10, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "nov", 11, people$KS1_MONTHOFBIRTH)
  people$KS1_MONTHOFBIRTH <- ifelse(people$month_of_birth == "dec", 12, people$KS1_MONTHOFBIRTH)
  
  people$KS1_GENDER <- ifelse(people$gender == 0, "M","F")
}
