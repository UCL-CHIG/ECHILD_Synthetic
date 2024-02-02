setwd("C:/Users/ASUS/Documents/GitHub/ECHILD_Synthetic/R")
library(scales)

if(file.exists("3_KS1.csv")){
  source("3_create_ks1.R")
}else{
  ks1 <- read.csv("3_KS1.csv")
  ks2 <- ks1
  colnames(ks2) <- gsub("KS1","KS2", colnames(ks2))
  ks2$KS2_ACADYR <- "2014/2015"
  ks2$KS2_MATH_new <- ks2$KS2_MATH * runif(nrow(ks2), 0.95, 1.05)
  ks2$KS2_math_random <- round(runif(nrow(ks2), 1, 500))
  ks2$KS2_crazy <- runif(nrow(ks2), 0,2 )
  ks2$KS2_MATH_new <- ifelse(ks2$KS2_math_random == sample(1:500,1), ks2$KS2_MATH_new * ks2$KS2_crazy, ks2$KS2_MATH_new)
  ks2$KS2_MATH_new <- rescale(ks2$KS2_MATH_new, to = c(0,100))

  ks2$KS2_SEN_new <- ifelse(ks2$KS2_MATH <= 30, 1, ks2$KS2_SEN)
  ks2$ks2_sen_random <- round(runif(nrow(ks2), 1,30),0)
  ks2$KS2_SEN_new <- ifelse(ks2$ks2_sen_random == sample(1:30,1),ks2$KS2_SEN_new + 1, ks2$KS2_SEN_new)
  ks2$KS2_SEN_new <- ifelse(ks2$KS2_SEN_new == 2, 0, ks2$KS2_SEN_new)
  
  ks2$KS2_MATH <- ks2$KS2_MATH_new
  ks2$KS2_SEN <- ks2$KS2_SEN_new
  
  ks2 <- subset(ks2, select = c(-KS2_MATH_new, -KS2_math_random, -KS2_crazy, -KS2_SEN_new, -ks2_sen_random))
  write.csv(ks2, "5_KS2.csv", row.names=FALSE)
}