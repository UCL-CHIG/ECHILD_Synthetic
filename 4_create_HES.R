setwd("S:/ICH_PPP_CENB_CEBCH/Matthew/TEACHING/ECHILD/ECHILD_Synthetic-main")
options(scipen = 999)

random_epikey_generator <- function(n) {
  epikeys <-100000000000:(200000000000 - 1)
  epikeys <- sample(epikeys, n)
  return(epikeys)
}


if (!file.exists("2_spine.csv") & !file.exists("1_people.csv")) {
  source("2_spine.R")
} else {
  people <- read.csv("1_people.csv")
  people$gestat <- people$gestational_age
  
  people$diag_01 <- "Z38"
  people$startage <- 7001
  people$epitype <- 3
  people$admimeth <- 82
  people$epiorder <- 1
  
  people$ethnos <- people$ethnicity
  people$ethnos <- ifelse(people$ethnos == "white_british", "A", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "white_irish", "B", people$ethnos)
  people$ethnos <- ifelse(people$ethnos %in% c("white_other", "white_gypsie_or_irish_trav", "white_roma"), "C", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "mixed_white_black_carribean", "D", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "mixed_white_black_african", "E", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "mixed_white_asian", "F", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "mixed_other", "G", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "asian_indian", "H", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "asian_pakistani", "J", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "asian_bangleshi", "K", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "asian_other", "L", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "black_carribean", "M", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "black_african", "N", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "black_other", "P", people$ethnos)
  people$ethnos <- ifelse(people$ethnos == "asian_chinese", "R", people$ethnos)
  # HES does not measure these groups
  people$ethnos <- ifelse(people$ethnos %in% c("other_any_other","other_arab"), "S", people$ethnos)
  people$epikey <- random_epikey_generator(nrow(people))
  
  
  people$year_of_birth<- ifelse(people$month_of_birth %in% c("sep","oct","nov","dec"),2004,2005 )
  
  people$dob <- paste0("15", people$month_of_birth, people$year_of_birth)
  people$dob_reformatted <- as.Date(people$dob, format = "%d%b%Y")
  people$epistart <- people$dob_reformatted
  people$admistart <- people$dob_reformatted
  
  people$sex <- people$gender + 1
  
  people$gestational_age <- ordered(people$gestational_age, levels = c("less_than_28","28_29","30_31","32","33","34","35","36","37","38","39","40","41","42"))
  people$gestational_age_num <- as.numeric(people$gestational_age)
  people$gestational_age_num_reverse <- max(people$gestational_age_num) - people$gestational_age_num
  people$gestational_age_num_reverse_half <- round(people$gestational_age_num_reverse/2)
  people$gestational_age_random <- sample(c(0,1), replace = TRUE, size = nrow(people))
  people$disdate <- people$admistart + 1 + (people$gestational_age_num_reverse_half * people$gestational_age_random)
  people$epiend <- people$disdate
  
  people$diff <- as.numeric(people$disdate - people$admistart)
  people$diag_02 <- ifelse(people$diff >= 3, sample(c(NA, "Q43","Q24","Q35", "Q37"), replace = TRUE, size = nrow(people)), NA)
  
  people$imd04_decile <- ifelse(people$imd_deciles == 1, "Most Deprived 10%", NA)
  people$imd04_decile <- ifelse(people$imd_deciles == 2, "More Deprived 10% - 20%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 3, "More Deprived 20% - 30%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 4, "More Deprived 30% - 40%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 5, "More Deprived 40% - 50%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 6, "Less Deprived 40% - 50%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 7, "Less Deprived 30% - 40%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 8, "Less Deprived 20% - 30%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 9, "Less Deprived 10% - 20%", people$imd04_decile)
  people$imd04_decile <- ifelse(people$imd_deciles == 10, "Least Deprived 10%", people$imd04_decile)
  
  people$resgor <- people$resgor_birth
  people$resgor <- ifelse(people$resgor == "north_east", "A", people$resgor)
  people$resgor <- ifelse(people$resgor == "north_west", "B", people$resgor)
  people$resgor <- ifelse(people$resgor == "yorkshire_humber", "D", people$resgor)
  people$resgor <- ifelse(people$resgor == "east_midlands", "E", people$resgor)
  people$resgor <- ifelse(people$resgor == "west_midlands", "F", people$resgor)
  people$resgor <- ifelse(people$resgor == "east", "G", people$resgor)
  people$resgor <- ifelse(people$resgor == "london", "H", people$resgor)
  people$resgor <- ifelse(people$resgor == "south_east", "J", people$resgor)
  people$resgor <- ifelse(people$resgor == "south_west", "K", people$resgor)
  people$resgor <- ifelse(people$resgor == "scotland", "S", people$resgor)
  people$resgor <- ifelse(people$resgor == "wales", "W", people$resgor)
  
  hes <- subset(people, select = c("tokenid", "epikey", "epitype", "epiorder",
                                   "epistart", "epiend", "admistart", "disdate",
                                   "admimeth", "sex", "ethnos", "resgor", "imd04_decile", 
                                   "gestat", "diag_01","diag_02"))
  
  write.csv(hes, "4_HES.csv", row.names=FALSE)
}