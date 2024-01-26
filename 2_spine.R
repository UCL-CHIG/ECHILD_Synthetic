setwd("C:/Users/stitch/Documents/GitHub/ECHILD_Synthetic")

if(!file.exists("1_people.csv")){
  source("1_main.R")
}else{
 people <- read.csv("1_people.csv") 
 spine <- subset(people, select = c("PupilMatchingRefAnonymous","tokenid"))
 write.csv(spine, "2_spine.csv", row.names=FALSE)
}
