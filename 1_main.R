setwd("S:/ICH_PPP_CENB_CEBCH/Matthew/TEACHING/ECHILD/ECHILD_Synthetic-main")
options(scipen = 999)

set.seed(1234)

n_people <- 500000


#' Convert from decimal to alternate base "numbers"
#'
#' @param numbers Decimal (integer) numbers to convert from
#' @param alt_base The alternative base
#' @param num_length Maximum length (number of digits) of the alternative base numbers
#' @param digits The set of digits/characters for the alternative base number system
#'
#' @return Character vector of alternative base numbers
#' @export
#'
#' @examples
#' decimal2AltBase(c(1,15,32), 16, 2, c(0:9, LETTERS[1:6]))
decimal2AltBase <- function(numbers, alt_base, num_length, digits) {
  
  
  alt_vals <- sapply(numbers, function(num, alt_base, num_length, digits) {
    digit_vals <- integer(num_length)
    for(i in num_length:1 - 1) {
      divisor <- alt_base^i
      digit_vals[num_length - i] <- floor(num / divisor)
      num <- num - digit_vals[num_length - i] * divisor
    }
    
    alt_vals <- paste0(digits[digit_vals + 1], collapse = "")
    return(alt_vals)
  }, alt_base = alt_base, num_length = num_length, digits = digits)
  
  return(alt_vals)
}

#' Generate IDs
#'
#' @param number How many IDs to generate
#' @param digits The set of characters to use as digits
#' @param id_length The length of the ID (excluding prefix and suffix)
#' @param prefix Prefix to IDs
#' @param suffix Suffix to IDs
#'
#' @return Character vector of IDs
#' @export
#'
#' @examples 
#' generateIDs(1, c(0:9, LETTERS), 3, "00", "-0")
generateIDs <- function(number, digits, id_length, prefix = "", suffix = "") {
  
  n_digits <- length(digits)
  
  combinations <- n_digits^id_length
  
  if(combinations < number) {
    stop("Not possible to generate requested number of distinct IDs based on digits and id_length.")
  }
  
  if(combinations < .Machine$integer.max) {
    sample_vals <- sample.int(n = combinations,
                              size = number)
    
    sample_ids <- decimal2AltBase(sample_vals, n_digits, id_length, digits)
    
    
  } else if(exp(- number^2 / 2 / combinations) > 0.999) {
    # Random chance of generating unique sequence is > 99.9%
    repeat {
      sample_ids <- sapply(1:number, function(n, digits, id_length) {
        return(paste0(sample(x = digits, 
                             size = id_length, 
                             replace = TRUE),
                      collapse = ""))
      }, digits = digits, id_length = id_length)
      
      if(anyDuplicated(sample_ids) == 0) break
    }
    
  } else {
    stop("Generating a random sequence based on these inputs is too difficult.")
  }
  
  sample_ids <- paste0(prefix, sample_ids, suffix)
  return(sample_ids)
}

#gender = 50/50
gender <- sample(c(0, 1), n_people, replace=TRUE)

#ethnicity
#https://www.ethnicity-facts-figures.service.gov.uk/uk-population-by-ethnicity/national-and-regional-populations/population-of-england-and-wales/latest
ethnicity_asian_bangleshi <- 1.1
ethnicity_asian_chinese <-  0.7
ethnicity_asian_indian <-  3.1
ethnicity_asian_pakistani <- 2.7
ethnicity_asian_other <- 1.6
ethnicity_black_african <- 2.5
ethnicity_black_carribean <- 1.0
ethnicity_black_other <-0.5
ethnicity_mixed_white_asian <-0.8
ethnicity_mixed_white_black_african <-  0.4
ethnicity_mixed_white_black_carribean <- 0.9
ethnicity_mixed_other <-  0.8
ethnicity_white_gypsie_or_irish_trav <- 0.1
ethnicity_white_roma <-  0.2
ethnicity_white_irish <- 0.9
ethnicity_white_other <- 6.2
ethnicity_other_arab <- 0.6
ethnicity_other_any_other <-  1.6

all_ethnicity_objects <- ls()
all_ethnicity_objects <- all_ethnicity_objects[grepl("ethnicity_",all_ethnicity_objects)]
all_ethnicity_data <- c()
for(i in all_ethnicity_objects){
  temp <- get(i)
  temp <- rep(i, n_people * temp/100)
  all_ethnicity_data <- c(all_ethnicity_data,temp)
}

ethnicity_white_british <- rep("ethnicity_white_british", n_people - length(all_ethnicity_data))
all_ethnicity_data <- c(all_ethnicity_data, ethnicity_white_british)
rm(list = all_ethnicity_objects)
rm(ethnicity_white_british)
rm(all_ethnicity_objects)

#month of birth
#https://www.statista.com/chart/5814/the-months-of-the-year-with-the-most-births/
month_of_birth_jan <- 54951
month_of_birth_feb <- 51826
month_of_birth_mar <- 53561
month_of_birth_apr <- 53264
month_of_birth_may <- 56071
month_of_birth_jun <- 55027
month_of_birth_jul <- 57754
month_of_birth_aug <- 56773
month_of_birth_sep <- 57035
month_of_birth_oct <- 55169
month_of_birth_nov <- 54054
month_of_birth_dec <- 54754

month_of_birth_objects <- ls()
month_of_birth_objects <- month_of_birth_objects[grepl("month_of_birth", month_of_birth_objects)]
sum <- 0
for(i in month_of_birth_objects){
  temp <- get(i)
  sum <- sum + temp
}
month_of_birth_data <- c()
for(i in month_of_birth_objects){
  temp <- get(i)
  temp <- temp/sum
  temp <- round(n_people*temp,0)
  temp <- rep(i, temp)
  month_of_birth_data <- c(month_of_birth_data, temp)
}
month_of_birth_data <- sample(month_of_birth_data, n_people)
rm(list = month_of_birth_objects)

imd_deciles <- 1:10
imd_deciles <- rep(imd_deciles, n_people/length(imd_deciles))

#gestational age distribution https://www.bmj.com/content/371/bmj.m4075
gestational_age_less_than_28 <- 0.2
gestational_age_28_29 <- 0.2
gestational_age_30_31 <- 0.3
gestational_age_32 <- 0.3
gestational_age_33 <- 0.4
gestational_age_34 <- 0.7
gestational_age_35 <- 1.1
gestational_age_36 <- 2.3
gestational_age_37 <- 5.3
gestational_age_38 <- 13.5
gestational_age_39 <- 22.7
gestational_age_40 <- 28.3
gestational_age_41 <- 20.6
gestational_age_42 <- 4.3
gestational_age_objects <- ls()
gestational_age_objects <- gestational_age_objects[grepl("gestational_age_",gestational_age_objects)]
gestational_age_data <- c()
for(ga in gestational_age_objects){
  temp <- get(ga)
  temp <- n_people * (temp/100)
  temp <- rep(ga, temp)
  gestational_age_data <- c(gestational_age_data, temp)
}
gestational_age_data <- sample(gestational_age_data,n_people)

# resgor
# based on 2008 data
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/birthsbyareaofusualresidenceofmotheruk

resgor_north_east <- 4
resgor_north_west <- 13
resgor_yorkshire_humber <- 10
resgor_east_midlands <- 8
resgor_west_midlands <- 11
resgor_east <- 11
resgor_london <- 19
resgor_south_east <- 15
resgor_south_west <- 9

resgor_objects <- ls()
all_resgor_objects <- resgor_objects[grepl("resgor_", resgor_objects)]

all_resgor_data <- c()
for(i in all_resgor_objects){
  temp <- get(i)
  temp <- rep(i, n_people * temp/100)
  all_resgor_data <- c(all_resgor_data, temp)
}

all_resgor_data <- sample(all_resgor_data, n_people)
rm(list = all_resgor_objects)

all_resgor_data[sample(1:n_people, n_people * 0.005)] <- "resgor_scotland"
all_resgor_data[sample(1:n_people, n_people * 0.005)] <- "resgor_wales"


pmrs <- generateIDs(n_people, c(0:9, LETTERS[1:6]), 16, prefix = "CCF")

token_ids <- generateIDs(n_people, c(0:9, LETTERS), 11, prefix = "0000")


people <- data.frame(
  PupilMatchingRefAnonymous = pmrs,
  tokenid = token_ids,
  month_of_birth = sample(month_of_birth_data, n_people),
  gender = sample(gender, n_people),
  ethnicity = sample(all_ethnicity_data, n_people),
  imd_deciles = sample(imd_deciles, n_people),
  gestational_age = sample(gestational_age_data, n_people),
  resgor_birth = sample(all_resgor_data, n_people)
)

people$ethnicity <- gsub("ethnicity_","", people$ethnicity)
people$month_of_birth <- gsub("month_of_birth_","",people$month_of_birth)
people$gestational_age <- gsub("gestational_age_","", people$gestational_age)
people$resgor_birth <- gsub("resgor_","", people$resgor_birth)

write.csv(people, "1_people.csv", row.names=FALSE)

all_items <- ls()
all_items <- all_items[!(all_items %in% c("people"))]
rm(list = all_items)
rm(all_items)
