setwd("C:/Users/ASUS/Documents/GitHub/ECHILD_Synthetic")
source("1_main.R")

spine <- subset(people, select = c("PupilMatchingRefAnonymous","encrypted_hesid"))
write.csv(spine, "spine.csv", row.names=FALSE)
write.csv(people, "people.csv", row.names=FALSE)
