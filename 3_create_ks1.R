setwd("C:/Users/stitch/Documents/GitHub/ECHILD_Synthetic")

if(!file.exists("2_spine.csv") & !file.exists("1_people.csv")){
  source("2_spine.R")
}else{
  people <- read.csv("1_people.csv")
  
  people$KS1_AGE_START <- ifelse(people$month_of_birth == "sep", 6, 5)
  
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
  
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "sep", 12, NA)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "oct", 11, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "nov", 10, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "dec", 9, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "jan", 8, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "feb", 7, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "mar", 6, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "apr", 5, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "may", 4, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "jun", 3, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "jul", 2, people$KS1_MONTH_PART)
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "aug", 1, people$KS1_MONTH_PART)
    
  people$KS1_GENDER <- ifelse(people$gender == 0, "M","F")
  
  people$gestational_age <- ordered(people$gestational_age, levels = c("less_than_28","28_29","30_31","32","33","34","35","36","37","38","39","40","41","42"))
  people$gestational_age_num <- as.numeric(people$gestational_age)
  people$gestational_age_num_randomness <- sample(runif(nrow(people), min = 0.5, max = 1.5), nrow(people))

  increments = rep((1-5)/12, nrow(people))
  increments_randomness <- runif(nrow(people), min = 0.7, max = 1.2)
  
  people$score_temp <- ifelse(people$gestational_age_num >= 1 & people$gestational_age_num <= 12, 5 - (people$gestational_age_num * increments * people$gestational_age_num_randomness), NA)
  people$score_temp <- ifelse(people$gestational_age_num > 12, 5 - ((people$gestational_age_num - ((people$gestational_age_num - 12)*2))*increments * people$gestational_age_num_randomness), people$score_temp)
  people$score_adjustment_gender <- runif(nrow(people), min = 0, max = 0.1)
  people$score_adjustment_gender <- ifelse(people$gender == 0,0, people$score_adjustment_gender)
  
  people$score_adjustment_imd <- runif(nrow(people), min = 0, max = 0.01)
  people$score_adjustment_imd <-  people$imd_deciles * people$score_adjustment_imd
  
  people$score_adjustment_month_of_birth <- runif(nrow(people), min = 0, max = 0.01)
  people$score_adjustment_month_of_birth <- people$score_adjustment_month_of_birth * people$KS1_MONTH_PART
  
  people$score_temp <- people$score_temp + (people$score_temp * people$score_adjustment_gender) + (people$score_temp *people$score_adjustment_imd) + (people$score_temp * people$score_adjustment_month_of_birth)
  people$score_normalised <- (people$score_temp - mean(people$score_temp) )/sd(people$score_temp)
  
  rands <- runif(nrow(people), -0.1, 0.1)
  people$score_normalised <- people$score_normalised + rands
  people$KS1_MATH <- people$score_normalised
  
  for(i in unique(people$gestational_age_num)){
      coinflip <- sample(c(0,1),1)
      
      if(coinflip == 0){
        random <- runif(1, min = -1, max = 1)
        people$KS1_MATH <- ifelse(people$gestational_age_num == i, people$KS1_MATH + random,people$KS1_MATH)
      }
      
  }
  
  plot(people$gestational_age_num, people$KS1_MATH)
  
  people$KS1_MONTH_PART <- ifelse(people$month_of_birth == "sep", 0, people$KS1_MONTH_PART)
  
  KS1_DATA <- subset(people, select = c("PupilMatchingRefAnonymous","KS1_ACADYR","KS1_YEAROFBIRTH","KS1_MONTHOFBIRTH","KS1_AGE_START","KS1_MONTH_PART","KS1_GENDER","KS1_MATH"))
  write.csv(KS1_DATA, "3_KS1.csv", row.names=FALSE)
}
