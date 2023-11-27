n_people <- 500000

#gender = 50/50
gender <- sample(c(0,1),n_people, replace=TRUE)

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

generate_pmr <- function(i){
  temp <- paste0(sample(c(0:9, LETTERS[1:6]), 16, T), collapse = '')
  temp <- paste0("CCF",temp)
}

pmrs <- lapply(1:n_people*2,generate_pmr)
pmrs <- unlist(pmrs)
pmrs <- unique(pmrs)

generate_ehis <- function(i){
  temp <- paste0(sample(c(0:9, LETTERS[1:6]), 16, T), collapse = '')
  temp <- paste0("0000",temp)
}

ehids <- lapply(1:n_people, generate_ehis)
ehids <- unique(unlist(ehids))


people <- data.frame(
  PupilMatchingRefAnonymous = pmrs,
                     encrypted_hesid = ehids,
                     month_of_birth = sample(month_of_birth_data, n_people),
                     gender = sample(gender, n_people),
                     ethnicity = sample(all_ethnicity_data, n_people),
                     imd_deciles = sample(imd_deciles, n_people)
)
people$ethnicity <- gsub("ethnicity_","", people$ethnicity)
people$month_of_birth <- gsub("month_of_birth_","",people$month_of_birth)

