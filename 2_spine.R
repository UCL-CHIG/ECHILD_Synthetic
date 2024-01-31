setwd("C:/Users/ASUS/Documents/GitHub/ECHILD_Synthetic")

if(!file.exists("1_people.csv")){
  source("1_main.R")
}else{
 people <- read.csv("1_people.csv") 
 
 n_white_british <- nrow(subset(people, ethnicity == "white_british"))
 n_not_white_british <- nrow(subset(people, ethnicity != "white_british"))
 
 people$linked <- ifelse(people$ethnicity == "white_british", rbinom(n_white_british, 1, 0.99), rbinom(n_not_white_british, 1, 0.95))
 
 for(eth in unique(people$ethnicity)){
   if(eth != "white_british"){
     perc <- runif(1, min = 0.90, 0.975)
     n_temp <- nrow(subset(people, ethnicity == eth))
     people$linked <- ifelse(people$ethnicity == eth, rbinom(n_temp, 1, perc), people$linked)
   }
 }
 
 linked <- subset(people, linked == 1)
 
 spine <- subset(linked, select = c("PupilMatchingRefAnonymous","tokenid"))
 write.csv(spine, "2_spine.csv", row.names=FALSE)
}
