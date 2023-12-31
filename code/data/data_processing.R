
# libraries needed
pacman::p_load(readr, dplyr, tidyverse, caret)

# source functions needed
source("code/functions/multicollinearity_checker.R")

# load in raw data
vacc_perc_data <- readr::read_csv(file = "raw_data/Survey Summary.csv", 
                                  locale=locale(encoding="latin1")) |>
                  tibble::as_tibble()

# select columns and rows of interest
vacc_perc_data <- vacc_perc_data[2:2418, c(6:11, 13:14, 16:17)]

# make column names more succinct
colnames(vacc_perc_data) <- c("age", "gender", "location", "education", "class", "primary SM",
                              "dly use", "exposed", "trust", "anti-vacc")

# make column row entries more succinct
vacc_perc_data <- vacc_perc_data |>
  dplyr::mutate(
    location = case_when(location == "North America, United States" ~ "NA U.S.",
                         location == "North America, Other" ~ "NA other",
                         location == "South America" ~ "SA",
                         location == "Australia/ Oceania" ~ "Aus/Ocea",
                         location == "Europe" ~ "EU",
                         location == "Africa" ~ "Afr",
                         location == "Antarctica" ~ "Ant"),
    education = case_when(education == "No formal schooling" ~ "None",
                          education == "Elementary school (grade level 1-8)" ~ "K-8",
                          education == "High school (grade level 9-12/13)" ~ "9-13",
                          education == "Associates Degree (2 year college/university degree)" ~ "Assoc",
                          education == "Bachelor Degree (4 year college/university degree)" ~ "Bach",
                          education == "Master's Degree" ~ "Master",
                          education == "Professional/Doctoral Degree (PhD, MD, DC etc..)" ~ "Highest"),
    class = case_when(class == "Upper Class" ~ "Upp",
                      class == "Middle Class" ~ "Mid",
                      class == "Lower Class" ~ "Low"),
    `primary SM` = case_when(`primary SM` == "Twitter" ~ "Twitt",
                             `primary SM` == "Facebook" ~ "FB",
                             `primary SM` == "Instagram" ~ "IG",
                             `primary SM` == "Other (please specify)" ~ "Other"),
    `dly use` = case_when(`dly use` == "0-2" ~ "0-2",
                          `dly use` == "4-Mar" ~ "3-4",
                          `dly use` == "6-May" ~ "5-6",
                          `dly use` == "8-Jul" ~ "7-8",
                          `dly use` == "9+" ~ "9+"),
    exposed = case_when(exposed == "Yes" ~ 1,
                        exposed == "No" ~ 0),
    trust = case_when(trust == "Doctors" ~ "Doc",
                      trust == "Internet" ~ "Web",
                      trust == "Peers/Friends" ~ "Peers",
                      trust == "The government" ~ "Gov",
                      trust == "Family" ~ "Fam",
                      trust == "Social Media" ~ "SM"),
    `anti-vacc` = case_when(`anti-vacc` == "I believe this is NOT true" ~ 0,
                             T ~ 1)) |>
  dplyr::filter(gender != "Other / Prefer not to answer")

# coerce typings
vacc_perc_data <- vacc_perc_data |>
                  dplyr::mutate(
                    age = as.factor(age),
                    gender = as.factor(gender),
                    location = as.factor(location),
                    education = as.factor(education),
                    class = as.factor(class),
                    `primary SM` = as.factor(`primary SM`),
                    `dly use` = as.factor(`dly use`),
                    trust = as.factor(trust)
                  ) |>
                  na.omit()

# one-hot encode categorical variables
dummy <- caret::dummyVars(" ~ .", data = vacc_perc_data)
ohe_vacc_perc_data <- data.frame(predict(dummy, newdata = vacc_perc_data)) |>
                      tibble::as_tibble()
rm(dummy)

## create concise naming for one-hot encoded variables
colnames(ohe_vacc_perc_data) <- c("age_18_24", "age_25_34", "age_35_44", "age_45_54", "age_55_64", "age_65_plus", 
                                  "f", "m", "afr", "aus_ocea", "eu", "na_oth", "na_us", "sa", "grade_9_13", "assoc", 
                                  "bach", "highest", "k_8", "mast", "no_ed", "low", "mid", "upp", "fb", "ig", "sm_oth", 
                                  "twit", "hr_0_2", "hr_3_4", "hr_5_6", "hr_7_8", "hr_9_plus", "exposed", "doc", "fam", 
                                  "gov", "peer", "web", "anti_vacc")

## check for multicollinearity in ohe data
mc_checker(ohe_vacc_perc_data) |> as.data.frame() |> view()

## remove the f and 0_2_hr variables to deal with severe multicollinearity
ohe_vacc_perc_data <- ohe_vacc_perc_data |>
                      select(-c(f, `hr_0_2`))

# store processed data
file.create("processed_data/vacc_data.csv")
file.create("processed_data/ohe_vacc_data.csv")
readr::write_csv(vacc_perc_data, file = "processed_data/vacc_data.csv")
readr::write_csv(ohe_vacc_perc_data, file = "processed_data/ohe_vacc_data.csv")
