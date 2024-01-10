setwd("C:/Users/stitch/Documents/GitHub/ECHILD_Synthetic")
library(sjPlot)
library(gtsummary)
library(dplyr)

spine <- read.csv("2_spine.csv")
npd <- read.csv("3_KS1.csv")
hes <- read.csv("4_HES.csv")

combine <- merge(npd, spine, by = "PupilMatchingRefAnonymous", all.x=TRUE)
combine <- merge(combine, hes, by = "encrypted_hesid", all.x=TRUE)

#tabulating gender in NPD and Sex in HES
table(combine$KS1_GENDER, combine$sex)

#set imd levels
combine$imd04_decile <- ordered(combine$imd04_decile, levels = c("Least Deprived 10%", "Less Deprived 10% - 20%", "Less Deprived 20% - 30%","Less Deprived 30% - 40%", "Less Deprived 40% - 50%", "More Deprived 40% - 50%","More Deprived 30% - 40%", "More Deprived 20% - 30%",  "More Deprived 10% - 20%" , "Most Deprived 10%"   ))

#set gestat levels
combine$gestat <- ordered(combine$gestat, levels = c("less_than_28","28_29","30_31","32","33","34","35","36","37","38","39","40","41","42"))


#calculating mean and ci for ks1_math given birth characteristics
gestat_and_ks1 <- data.frame()
for(iGestat in unique(combine$gestat)){
  temp <- subset(combine, gestat == iGestat)
  
  n <- nrow(temp)
  ks1_mean <- mean(temp$KS1_MATH, na.rm=TRUE)
  ks1_sd <- sd(temp$KS1_MATH, na.rm=TRUE)
  ks1_se <- ks1_sd/sqrt(n)
  
  t_score <- 1.962349
  margin_error <- t_score * ks1_se
  
  lower <- ks1_mean - margin_error
  upper <- ks1_mean + margin_error
  
  temp <- data.frame(gestat = iGestat, mean = ks1_mean, lower = lower, upper = upper)
  gestat_and_ks1 <- rbind(temp, gestat_and_ks1)
}
gestat_and_ks1$gestat <- as.factor(gestat_and_ks1$gestat)
gestat_and_ks1$gestat <- ordered(gestat_and_ks1$gestat, levels = c("less_than_28","28_29","30_31","32","33","34","35","36","37","38","39","40","41","42"))

plot(as.numeric(gestat_and_ks1$gestat) ,gestat_and_ks1$mean,xaxt = 'n', xlab = "Gestational Age", ylab = "Standardised KS1 Maths score", ylim = c(min(gestat_and_ks1$lower), max(gestat_and_ks1$upper)), type = "p")
axis(1, at = c(as.numeric(gestat_and_ks1$gestat)), labels = c(as.character(gestat_and_ks1$gestat)))
segments(as.numeric(gestat_and_ks1$gestat), gestat_and_ks1$lower, as.numeric(gestat_and_ks1$gestat), gestat_and_ks1$upper)

tbl_summary(combine, include = c(gestat,KS1_MONTHOFBIRTH,KS1_GENDER,KS1_MATH,imd04_decile), by = gestat) %>% add_n() %>% add_p()


glm_ks1 <- glm(KS1_MATH ~ gestat + KS1_MONTH_PART  + as.factor(KS1_GENDER) + as.factor(ethnos) + imd04_decile, data =combine)
tab_model(glm_ks1, ci_method = "wald")
