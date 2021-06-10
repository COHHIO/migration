# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

source(here("reading_severance.R"))


# Getting filtered data sets ----------------------------------------------

vispdat_subs <- da_recordset %>%
  filter(
    active == TRUE &
      question %in% c("VI-SPDAT v2.0", "VI-FSPDAT v2.0", "TAY-VI-SPDAT v1.0")
  ) %>% 
  rename("subassessment_name" = question,
         "sub_is_active" = active,
         "sub_date_added" = date_added,
         "sub_provider_created" = provider_creating_id,
         "sub_user_created" = user_creating_id) 

vispdat_answers <- da_recordset_answer %>%
  filter(active == TRUE) %>%
  rename("question_name" = question,
         "answer_active" = active,
         "answer_date_added" = date_added,
         "answer_provider_created" = provider_creating_id,
         "answer_user_created" = user_creating_id) %>%
  semi_join(vispdat_subs,
            by = "recordset_id") %>%
  mutate(corrected_date_effective = if_else(question_name == "Start Date",
                                            val, NULL))

# Correcting date_effective in severance file with Start Date -------------

get_start_dates <- vispdat_answers %>%
  left_join(vispdat_subs, by = "recordset_id") %>%
  select(recordset_id, date_effective, corrected_date_effective) %>%
  filter(!is.na(corrected_date_effective)) %>%
  mutate(corrected_date_effective = corrected_date_effective) %>%
  select(-date_effective) %>% unique()

corrected_subs <- vispdat_subs %>%
  left_join(get_start_dates, by = "recordset_id") %>%
  mutate(date_effective = format.Date(corrected_date_effective, "%Y-%m-%d")) %>%
  group_by(client_id, date_effective, subassessment_name) %>%
  slice_max(sub_date_added) %>%
  ungroup() %>%
  select(-corrected_date_effective) %>% unique()

# Deduplicating subs and then answers based on correct date_effective -----

deduplicated <- corrected_subs %>%
  left_join(vispdat_answers, by = "recordset_id") %>%
  filter(
    !question_name %in% c(
      "PRE-SURVEY",
      "A. HISTORY OF HOUSING AND HOMELESSNESS",
      "B. RISKS",
      "C. SOCIALIZATION & DAILY FUNCTIONS",
      "D. WELLNESS",
      "E. FAMILY UNIT",
      "GRAND TOTAL"
    )
  ) %>%
  group_by(recordset_id, question_name) %>%
  slice_max(answer_date_added) %>% 
  ungroup()

# building vi-fspdat ------------------------------------------------------

fspdat_translator <- tibble(
  sp = c(
    "1. How many children under the age of 18 are currently with you?",
    "1. Is either head of household 60 years of age or older?",
    "10. Have you or anyone in your family threatened to or tried to harm themselves or anyone else in the last year?",
    "11. Do you or anyone in your family have any legal stuff going on right now that may result in them being locked up, having to pay fines, or that make it more difficult to rent a place to live?",
    "12. Does anybody force or trick you or anyone in your family to do things you do not want to do?",
    "13. Do you or anyone in your family ever do things that may be considered to be risky, like exchange sex for money, run drugs for someone, have unprotected sex with someone they don't know, share a needle, or anything like that?",
    "14. Is there any person, past landlord, business, bookie, dealer, or government group like the IRS, that thinks you or anyone in your family owe them money?",
    "15. Do you or anyone in your family get any money from the government, a pension, an inheritance, working under the table, a regular job, or anything like that?",
    "16. Does everyone in your family have planned activities, other than just surviving, that make them feel happy and fulfilled?",
    "17. Is everyone in your family currently able to take care of basic needs like bathing, changing clothes, using a restroom, getting food and clean water and other things like that?",
    "18. Is your family's current homelessness in any way caused by a relationship that broke down, an unhealthy or abusive relationship, or because other family or friends caused your family to become evicted?",
    "19. Has your family ever had to leave an apartment, shelter program, or other place you were staying because of the physical health of you or anyone in your family?",
    "2. How many children under the age of 18 are not currently with your family, but you have reason to believe they will be joining you when you get housed?",
    "2. How many parents are included in this family?",
    "20. Do you or anyone in your family have any chronic health issues with their liver, kidneys, stomach, lungs, or heart?",
    "21. If there was space available in a program that specifically assists people that live with HIV or AIDS, would that be of interest to you or anyone in your family?",
    "22. Does anyone in your family have any physical disabilities that would limit the type of housing you could access, or would make it hard to live independently because you'd need help?",
    "23. When someone in your family is sick or not feeling well, does your family avoid getting medical help?",
    "24. Has drinking or drug use by anyone in your family led your family to being kicked out of an apartment or program where you were staying in the past?",
    "25. Will drinking or drug use make it difficult for your family to stay housed or afford your housing?",
    "26. a) A mental health issue or concern?",
    "26. b) A past head injury?",
    "26. c) A learning disability, developmental disability, or other impairment?",
    "27. Do you or anyone in your family have any mental health or brain issues that would make it hard for your family to live independently because help would be needed?",
    "28. a) Does any single member of your household have a medical condition, mental health concern, and experience with problematic substance use?",
    "29. Are there any medications that a doctor said you or anyone in your family should be taking that, for whatever reason, they are not taking?",
    "3. IF HOUSEHOLD INCLUDES A FEMALE: Is any member of the family currently pregnant?",
    "30. Are there any medications like painkillers that you or anyone in your family don't take the way the doctor prescribed or where they sell the medication?",
    "31. YES OR NO: Has your family's current period of homelessness been caused by an experience of emotional, physical, psychological, sexual, or other type of abuse, or by any other trauma you or anyone in your family have experienced?",
    "32. Are there any children that have been removed from the family by a child protection service within the last 180 days?",
    "33. Do you have any family legal issues that are being resolved in court or need to be resolved in court that would impact your housing or who may live within your housing?",
    "34. In the last 180 days have any children lived with family or friends because of your homelessness or housing situation?",
    "35. Has any child in the family experienced abuse or trauma in the last 180 days?",
    "36. IF THERE ARE SCHOOL-AGED CHILDREN: Do your children attend school more often than not each week?",
    "37. Have the members of your family changed in the last 180 days, due to things like divorce, your kids coming back to live with you, someone leaving for military service or incarceration, a relative moving in, or anything like that?",
    "38. Do you anticipate any other adults or children coming to live with you within the first 180 days of being housed?",
    "39. Do you have two or more planned activities each week as a family, such as outings to the park, going to the library, visiting other family, watching a family movie, or anything like that?",
    "4. a) ages 6 or younger?",
    "4. b) ages 11 or younger?",
    "4. c) You may use this area to provide a list of children's names and ages:",
    "40. a) 3 or more hours per day for children aged 13 or older?",
    "40. b) 2 or more hours per day for children aged 12 or younger?",
    "41. a) Do your older kids spend 2 or more hours on a typical day helping their younger siblings(s) with things like getting ready for school, helping with homework, making them dinner, bathing them, or anything like that?",
    "5. Where do you and your family sleep most frequently? (choose one)",
    "6. How long has it been since you and your family lived in permanent stable housing?",
    "7. In the last three years, how many times have you and your family been homeless?",
    "8. a) Received health care at an emergency department/room?",
    "8. b) Taken an ambulance to the hospital?",
    "8. c) Been hospitalized as an inpatient?",
    "8. d) Used a crisis service, including sexual assault crisis, mental health crisis, family/intimate violence, distress centers and suicide prevention hotlines?",
    "8. e) Talked to police because they witnessed a crime, were the victim of a crime, or the alleged perpetrator of a crime, or because the police told them that they must move along?",
    "8. f) Stayed one or more nights in a holding cell, jail, or prison, whether that was a short-term stay like the drunk tank, a longer stay for a more serious offense, or anything in between?",
    "9. Have you or anyone in your family been attacked or beaten up since they've become homeless?",
    "If Other, please specify",
    "Start Date"
  ),
  bf = c(
    "client_assessment_demographics.vi_f_spdat_children_num",
    "wrong_older_than_60",
    "client_assessment_demographics.vi_spdat_q9",
    "client_assessment_demographics.vi_spdat_q10",
    "client_assessment_demographics.vi_spdat_q11",
    "client_assessment_demographics.vi_spdat_q12",
    "client_assessment_demographics.vi_spdat_q14",
    "client_assessment_demographics.vi_spdat_q15",
    "client_assessment_demographics.vi_spdat_q17",
    "client_assessment_demographics.vi_spdat_q13_v2",
    "client_assessment_demographics.vi_spdat_q14_v2",
    "client_assessment_demographics.vi_spdat_q15_v2",
    "wrong_younger_than_18",
    "wrong_parents",
    "client_assessment_demographics.vi_spdat_q16_v2",
    "client_assessment_demographics.vi_spdat_q17_v2",
    "client_assessment_demographics.vi_spdat_q18_v2",
    "client_assessment_demographics.vi_spdat_q19_v2",
    "client_assessment_demographics.vi_spdat_q21_v2",
    "client_assessment_demographics.vi_spdat_q22_v2",
    "client_assessment_demographics.vi_spdat_q23a_v2",
    "client_assessment_demographics.vi_spdat_q23b_v2",
    "client_assessment_demographics.vi_spdat_q23c_v2",
    "client_assessment_demographics.vi_spdat_q24_v2",
    "client_assessment_demographics.vi_f_spdat_q28_v2",
    "client_assessment_demographics.vi_spdat_q49",
    "client_assessment_demographics.vi_f_spdat_pregnancy",
    "client_assessment_demographics.vi_spdat_q26_v2",
    "vi_spdat_q50",
    "vi_f_spdat_q32_v2",
    "client_assessment_demographics.vi_f_spdat_q33_v2",
    "client_assessment_demographics.vi_f_spdat_q34_v2",
    "client_assessment_demographics.vi_f_spdat_q35_v2",
    "client_assessment_demographics.vi_f_spdat_q36_v2",
    "client_assessment_demographics.vi_f_spdat_q37_v2",
    "client_assessment_demographics.vi_f_spdat_q38_v2",
    "client_assessment_demographics.vi_f_spdat_q39_v2",
    "wrong_6_or_younger",
    "wrong_11_or_younger",
    "wrong_text_kids",
    "client_assessment_demographics.vi_f_spdat_q40a_v2",
    "client_assessment_demographics.vi_f_spdat_q40b_v2",
    "client_assessment_demographics.vi_f_spdat_q41_v2",
    "client_assessment_demographics.vi_spdat_q1_v2",
    "client_assessment_demographics.vi_spdat_q1",
    "client_assessment_demographics.vi_spdat_q2",
    "client_assessment_demographics.vi_spdat_q3",
    "client_assessment_demographics.vi_spdat_q5",
    "client_assessment_demographics.vi_spdat_q7",
    "client_assessment_demographics.vi_spdat_q6",
    "client_assessment_demographics.vi_spdat_q4",
    "client_assessment_demographics.vi_spdat_q4f_v2",
    "client_assessment_demographics.vi_spdat_q8",
    "client_assessment_demographics.vi_spdat_q1a_v2",
    "client_assessment_demographics.assessment_date"
  )
)

fspdat_data <- deduplicated %>%
  filter(
    subassessment_name == "VI-FSPDAT v2.0" &
      !question_name %in% c(
        "PRE-SURVEY",
        "A. HISTORY OF HOUSING AND HOMELESSNESS",
        "B. RISKS",
        "C. SOCIALIZATION & DAILY FUNCTIONS",
        "D. WELLNESS",
        "E. FAMILY UNIT",
        "GRAND TOTAL"
      )
  ) %>% 
  select("PersonalID" = client_id,
         sub_provider_created, 
         "InformationDate" = date_effective,
         question_name, 
         val) %>%
  left_join(fspdat_translator, by = c("question_name" = "sp")) %>%
  mutate(question = bf,
         question_name = NULL,
         bf = NULL) %>%
  pivot_wider(
    names_from = question,
    values_from = val) %>%  
  mutate(
    client_assessment_demographics.vi_f_spdat_children_num =
      case_when(
        client_assessment_demographics.vi_f_spdat_children_num == "3 or more" ~ "4",
        client_assessment_demographics.vi_f_spdat_children_num == "Refused" |
          is.na(client_assessment_demographics.vi_f_spdat_children_num) ~ "0",
        TRUE ~ client_assessment_demographics.vi_f_spdat_children_num
      ),
    client_assessment_demographics.vi_f_spdat_children_num = 
      as.numeric(client_assessment_demographics.vi_f_spdat_children_num),
    wrong_younger_than_18 = case_when(
      wrong_younger_than_18 == "3 or more" ~ "4",
      wrong_younger_than_18 == "Refused" |
        is.na(wrong_younger_than_18) ~ "0",
      TRUE ~ wrong_younger_than_18
    ),
    wrong_younger_than_18 = as.numeric(wrong_younger_than_18),
    client_assessment_demographics.vi_f_spdat_children_num =
     wrong_younger_than_18 + client_assessment_demographics.vi_f_spdat_children_num,
    AssessmentID = "87",
    AssessmentName = "VI-F-SPDAT Prescreen for Families [V2]",
    InformationDate = format.Date(InformationDate, "%Y-%m-%d")
  )

# VI-SPDAT Prescreen for Single Adults [V2] (86)


# VI-Y-SPDAT Prescreen for Transition Age Youth (103)

