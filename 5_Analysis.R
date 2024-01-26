setwd("S:/ICH_PPP_CENB_CEBCH/Matthew/TEACHING/ECHILD/ECHILD_Synthetic-main")
library(sjPlot)
library(gtsummary)
library(dplyr)

spine <- read.csv("2_spine.csv")
npd <- read.csv("3_KS1.csv")
hes <- read.csv("4_HES.csv")

combine <- merge(npd, spine, by = "PupilMatchingRefAnonymous", all.x = TRUE)
combine <- merge(combine, hes, by = "tokenid", all.x = TRUE)
combine$any_comorbidity <- ifelse(!is.na(combine$diag_02), "Yes", "No")

#tabulating gender in NPD and Sex in HES
table(combine$KS1_GENDER, combine$sex)


generate_aggregate_results <- function(column_name, levs){
  out <- data.frame()
  for(val in unique(combine[[column_name]])){
    temp <- subset(combine, combine[[column_name]] == val)
    out <- rbind(out, data.frame(variable = val, 
                                 median = median(temp$KS1_MATH, na.rm=TRUE),
                                 lower = quantile(temp$KS1_MATH, 0.25, na.rm=TRUE),
                                 upper =quantile(temp$KS1_MATH, 0.75, na.rm=TRUE)
                                 
                                 ))
  }
  
  out$variable <- ordered(out$variable, levels = levs)
  out <- out[order(out$variable),]
  row.names(out) <- NULL
  colnames(out) <- c(column_name, "median","lower","upper")
  return(out)
}

gestat_and_ks1 <- generate_aggregate_results("gestat", c("less_than_28","28_29","30_31","32","33","34","35","36","37","38","39","40","41","42"))
imd_and_ks1 <- generate_aggregate_results("imd04_decile", c("Least Deprived 10%", "Less Deprived 10% - 20%", "Less Deprived 20% - 30%","Less Deprived 30% - 40%", "Less Deprived 40% - 50%", "More Deprived 40% - 50%","More Deprived 30% - 40%", "More Deprived 20% - 30%",  "More Deprived 10% - 20%" , "Most Deprived 10%"))
gender_and_ks1 <- generate_aggregate_results("KS1_GENDER",c("M","F"))
month_and_ks1 <- generate_aggregate_results("KS1_MONTH_PART", c(0,11,10,9,8,7,6,5,4,3,2,1))
comorbidity_and_ks1 <- generate_aggregate_results("any_comorbidity", c("No","Yes"))

plotit <- function(x_values, y_values, lower_ci, upper_ci, xLab, yLab, Main){
  plot(as.numeric(x_values) ,y_values,xaxt = 'n', xlab = xLab, ylab = yLab, ylim = c(min(lower_ci), max(upper_ci)), type = "p", main = Main)
  axis(1, at = c(as.numeric(x_values)), labels = c(as.character(x_values)))
  segments(as.numeric(x_values), lower_ci, as.numeric(x_values), upper_ci)
  lm <- glm(y_values ~ as.numeric(x_values))
  abline(lm)
}


par(mfcol = c(2,3))
hist(combine$KS1_MATH, main = "Histogram of KS1 Scores", xlab = "KS1 Score", ylab = "Frequency")
plotit(imd_and_ks1$imd04_decile, imd_and_ks1$median, imd_and_ks1$lower, imd_and_ks1$upper, "IMD Decile","Median KS1 Maths Score (z-score) [IQR]", "IMD Decile and Standardised median score")
plotit(gestat_and_ks1$gestat, gestat_and_ks1$median, gestat_and_ks1$lower, gestat_and_ks1$upper, "Gestational Age","Median KS1 Maths Score (z-score) [IQR]", "Gestational Age and Standardised median score")
plotit(gender_and_ks1$KS1_GENDER, gender_and_ks1$median, gender_and_ks1$lower, gender_and_ks1$upper, "Gender","Median KS1 Maths Score (z-score) [IQR]", "Gender and Standardised median score")
plotit(month_and_ks1$KS1_MONTH_PART, month_and_ks1$median, month_and_ks1$lower, month_and_ks1$upper, "Relative Month of Birth", "Median KS1 Maths Score (z-score) [IQR]", "Relative Age and Standardised median score")
plotit(comorbidity_and_ks1$any_comorbidity, comorbidity_and_ks1$median, comorbidity_and_ks1$lower, comorbidity_and_ks1$upper, "Any comorbidities at birth", "Median KS1 Maths Score (z-score) [IQR]", "Comorbidity at birth and Standardised median score")


combine$KS1_MONTH_PART <- ordered(combine$KS1_MONTH_PART, levels = c(0,11,10,9,8,7,6,5,4,3,2,1))
combine$imd04_decile <- ordered(combine$imd04_decile, levels = c("Least Deprived 10%", "Less Deprived 10% - 20%", "Less Deprived 20% - 30%","Less Deprived 30% - 40%", "Less Deprived 40% - 50%", "More Deprived 40% - 50%","More Deprived 30% - 40%", "More Deprived 20% - 30%",  "More Deprived 10% - 20%" , "Most Deprived 10%"))
combine$gestat <- ordered(combine$gestat, levels = c("less_than_28","28_29","30_31","32","33","34","35","36","37","38","39","40","41","42"))
combine$resgor <- ordered(combine$resgor, levels = c("A", "B", "D", "E", "F", "G", "H", "J", "K", "S", "W"))

tbl_summary(combine, include = c(gestat,KS1_MONTHOFBIRTH,
                                 KS1_GENDER,KS1_MATH,
                                 imd04_decile,
                                 resgor),
            by = gestat) %>% add_n() %>% add_p()
