library(tidyverse)
library(qualtRics)
library(rjson)
library(here)

# fetch survey data from qualtrics
survey_dat <- fetch_survey(surveyID = "***REMOVED***")
# read in themes from qualatative analysis
themes <- fromJSON(file = here("index", "analysis", "themes.json"))
gender_themes <- themes$gender
pronouns_themes <- themes$pronouns
sexuality_themes <- themes$sexual

# recode likert values
recode_likert <- function(data) {
  data %>%
    mutate_at(vars(starts_with(c("Q18", "Q11", "Q33", "Q30"))), 
              funs(
                recode(.,
                                 "Strongly agree" = 5,
                                 "Somewhat agree" = 4,
                                 "Neither agree nor disagree" = 3,
                                 "Strongly disagree" = 2,
                                 "Somewhat disagree" = 1
                       )
                  )
              ) %>%
    mutate_at(vars(starts_with(c("Q17", "Q14"))), 
              funs(recode(.,
                                 "Always" = 5,
                                 "Often" = 4,
                                 "Sometimes" = 3,
                                 "Rarely" = 2,
                                 "Never" = 1
                          )
                   )
              ) %>%
    mutate_at(vars(starts_with(c("Q16"))), 
              funs(recode(.,
                          "Very" = 5,
                          "Considerably" = 4,
                          "Somewhat" = 3,
                          "Slightly" = 2,
                          "Not at all" = 1
              )
              )
    )
}

convert_factors <- function(data) {
  data %>%
    mutate_at(vars(starts_with(c("Q21", "Q3", "Q2", "Q19", "Q8", "Q11", "Q33", "Q30"))), 
              funs(factor)
    ) %>%
    mutate_at(vars(starts_with(c("Q11", "Q33", "Q30"))), 
              funs(as.numeric)
    )
}

rename_data <- function(data) {
  data %>%
    rename(timestamp = RecordedDate) %>%
    rename(age = Q21) %>%
    rename(pronouns = `Q3 - Topics`) %>%
    rename(gender = `Q2 - Topics`) %>%
    rename(sexuality = `Q19 - Topics`) %>%
    rename(major = `Q8 - Topics`) %>%
    rename(year = Q7) %>%
    rename(comfort_general = Q11_1) %>%
    rename(comfort_reed = Q11_2) %>%
    rename(comfort_class = Q11_3) %>%
    rename(desire_general = Q11_4) %>%
    rename(desire_reed = Q11_5) %>%
    rename(desire_class = Q11_6) %>%
    rename(comfort_withsimilargenders = Q11_7) %>%
    rename(comfort_someoneelsefirst = Q11_8) %>%
    rename(comfort_proffirst = Q11_9) %>%
    rename(attention_general = Q33_1) %>%
    rename(attention_reed = Q33_2) %>%
    rename(attention_class = Q33_3) %>%
    rename(gender_perception_consistent = Q33_4) %>%
    rename(gender_pronouns_consistent = Q33_5) %>%
    rename(pronouns_represent = Q33_6) %>%
    rename(willmisgender_ifnopronouns = Q33_7) %>%
    rename(assume_pronouns_correctly = Q33_8) %>%
    rename(understand_better = Q33_9) %>%
    rename(profs_share = Q30_1) %>%
    rename(students_share = Q30_2) %>%
    rename(reed_support = Q30_3) %>%
    rename(misgendering_freq = Q14_1) %>%
    rename(misgendering_stigma = Q16_1)
}

recode_data  <- function(data) {
  data %>%
    # paste together ethnicity labels
    mutate(ethnicity = paste(Q20_1, Q20_2, Q20_3, Q20_4, Q20_5, Q20_6_TEXT, sep = ", "),
           # remove NA values
           ethnicity = gsub("(NA|, NA|NA ,)","",ethnicity),
           # remove leading commas
           ethnicity = gsub("^, ","",ethnicity),
           # recode blank rows as prefer not to say
           ethnicity = gsub("^$", "Prefer not to say", ethnicity)
    ) %>%
    # recode raw values
    mutate(nonbinary = recode(Q6, "Yes" = TRUE, "No" = FALSE, .default = FALSE)) %>%
    mutate(trans = recode(Q5, "Yes" = TRUE, "No" = FALSE, .default = FALSE)) %>%
    mutate(cis = recode(Q4, "Yes" = TRUE, "No" = FALSE, .default = FALSE)) %>%
    # convert to factors and add labels
    mutate(
      nonbinary = factor(nonbinary),
      trans = factor(trans),
      cis = factor(cis)
    )
}

calc_scores <- function(data) {
  data %>%
    # calculate congruence values
    mutate(
      Q18_6 = Q18_6 * -1,
      Q18_8 = Q18_8 * -1,
      Q18_10 = Q18_10 * -1,
      congruence = rowSums(select(., Q18_1, Q18_2, Q18_3, Q18_4, Q18_5, Q18_6, Q18_7, Q18_8, Q18_9, Q18_10, Q18_11, Q18_12)),
      inclusive_behavior = rowSums(select(., Q17_1, Q17_2, Q17_3, Q17_4, Q17_5, Q17_6, Q17_7, Q17_8, Q17_9, Q17_10, Q17_11, Q17_12, Q17_13, Q17_14))
      ) 
}

select_cols <- function(data) {
  data %>%
    select(c(
      timestamp,
      age,
      ethnicity,
      pronouns,
      gender,
      sexuality,
      major,
      year,
      nonbinary,
      trans,
      cis,
      congruence,
      inclusive_behavior,
      comfort_general,
      comfort_reed,
      comfort_class,
      desire_general,
      desire_reed,
      desire_class,
      comfort_withsimilargenders,
      comfort_someoneelsefirst,
      comfort_proffirst,
      attention_general,
      attention_reed,
      attention_class,
      gender_perception_consistent,
      gender_pronouns_consistent,
      pronouns_represent,
      willmisgender_ifnopronouns,
      assume_pronouns_correctly,
      understand_better,
      profs_share,
      students_share,
      reed_support,
      misgendering_freq,
      misgendering_stigma
      )
    )
}

# if you updated the themes in qualtrics you will need to re-export them and run the themes.js script
# convert qualatatively coded themes into discrete cols with boolean values
conv_qual <- function(theme_input, themes) {
  # add word boundaries so regex doesn't accidentally match "woman" when searching for "man" theme, etc
  regex_themes <- themes %>% lapply(function (theme) { paste("\\b", theme, "\\b", sep = "") })
  # match themes in each row
  data <- do.call(rbind.data.frame,
    lapply(theme_input, function(match_string) {
      lapply(regex_themes, function(t) {
        if (is.na(match_string)) {
          # if participant left write in blank, set all values to false
          factor(FALSE, levels = c(FALSE, TRUE))
        } else {
          # otherwise, check to see if there is a match for each theme
          factor(str_detect(as.character(match_string), t), levels = c(FALSE, TRUE))
        }
      })
    })
  )
  # replace NA's with FALSE
  data <- data %>%
    replace(is.na(data), FALSE)
  # set col names to themes
  colnames(data) <- themes
  data
}

# process qualatatively identified themes
# converts string of themes eg. woman,trans,non-binary into bool vals in cols
process_qual <- function(data) {
  gender_data <- conv_qual(data$gender, gender_themes)
  pronouns_data <- conv_qual(data$pronouns, pronouns_themes) %>%
    rename("he" = `he/him`, "she" = `she/her`, "they" = `they/them`)
  sexuality_data <- conv_qual(data$sexuality, sexuality_themes)
  
  data <- cbind(data, gender_data, pronouns_data, sexuality_data)
}

bin_genders <- function(data) {
  data %>%
    # bin genders into cis man, cis woman, trans man, trans woman, cis 
    mutate(gender_bin = factor(case_when(
      # cis == TRUE & nonbinary == FALSE & trans == FALSE & man == FALSE & woman == FALSE ~ "Cis",
      cis == TRUE & nonbinary == FALSE & trans == FALSE & man == TRUE & woman == FALSE ~ "Cis Man",
      cis == TRUE & nonbinary == FALSE & trans == FALSE & man == FALSE & woman == TRUE ~ "Cis Woman",
      cis == FALSE & trans == TRUE & man == TRUE & woman == FALSE ~ "Trans Man",
      cis == FALSE & trans == TRUE & man == FALSE & woman == TRUE ~ "Trans Woman",
      cis == TRUE & nonbinary == TRUE & trans == FALSE ~ "Cis Non-binary",
      # non-binary bin includes participants who only marked non-binary and participants who marked none
      # i also decided to fold non-binary men and non-binary women into the non-binary group since there
      # didn't seem to be a lot of differences (left commented tests at bottom of file)
      cis == FALSE & nonbinary == TRUE & trans == FALSE ~ "Non-binary",
      cis == FALSE & nonbinary == FALSE & trans == FALSE ~ "Non-binary",
      cis == FALSE & nonbinary == TRUE & trans == TRUE & man == FALSE & woman == FALSE ~ "Trans Non-binary",
      # cis == FALSE & nonbinary == TRUE & trans == FALSE & man == TRUE & woman == FALSE ~ "Non-binary Man",
      # cis == FALSE & nonbinary == FALSE & trans == FALSE & man == TRUE & woman == FALSE ~ "Non-binary Man",
      # cis == FALSE & nonbinary == TRUE & trans == FALSE & man == FALSE & woman == TRUE ~ "Non-binary Woman",
      # cis == FALSE & nonbinary == FALSE & trans == FALSE & man == FALSE & woman == TRUE ~ "Non-binary Woman",
      TRUE ~ "other"
    ), levels = c("Cis", "Cis Man","Cis Woman","Trans Man","Trans Woman", "Cis Non-binary", "Non-binary","Trans Non-binary", "Non-binary Man", "Non-binary Woman")))
}

mod_data <- survey_dat %>%
  filter(Finished == TRUE) %>%
  recode_likert() %>%
  convert_factors() %>%
  rename_data() %>%
  recode_data() %>%
  calc_scores() %>%
  select_cols() %>%
  process_qual() %>%
  bin_genders() %>%
  filter(year != "Other") %>%
  arrange(timestamp)

# mod_data %>%
#   filter(gender_bin == "Non-binary" | gender_bin == "Cis Non-binary") %>%
#   aov(congruence ~ gender_bin, .) %>%
#   TukeyHSD() %>%
#   tidy() %>%
#   view()
# mod_data %>%
#   select(cis, nonbinary, trans, man, woman, gender_bin) %>%
#   view()
