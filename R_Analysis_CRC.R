#1 Number of cases discontinued or declared admissible:
library(readxl)
library(dplyr)

df <- Final_Data_UN_CRC

outcome_counts <- df %>%
  count(case_outcome)

# Filter for admissible-related outcomes
relevant_outcomes <- outcome_counts %>%
  filter(case_outcome %in% c("admissible", "partly admissible", "discontinued", "inadmissible"))

print(relevant_outcomes)




#Number of individuals and states involved, as well as case count by country
df_main <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")
df_age <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "age_gender")

total_cases <- nrow(df_main)

cases <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")
total_cases <- nrow(df)

# Number of unique states (countries)
unique_states <- df %>%
  distinct(state_party) %>%
  nrow()
#23

#Cases by country
cases_by_country <- df %>%
  count(state_party, sort = TRUE)



#3) Success Rate of Cases (The Committee found a violation of the Convention in admissable or partly admissable cases)
library(readxl)
library(dplyr)

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

df_filtered <- df %>%
  filter(!Case_ID %in% excluded_cases)

reviewed_cases <- df_filtered %>%
  filter(case_outcome %in% c("admissible", "partly admissible"))

total_reviewed <- nrow(reviewed_cases)

num_violations <- reviewed_cases %>%
  filter(tolower(violation_outcome) == "violation") %>%
  nrow()

success_rate <- round(num_violations / total_reviewed, 2)
cat("Overall Success Rate:", success_rate * 100, "%\n")

df_admissible <- df_filtered %>%
  filter(case_outcome %in% c("admissible", "partly admissible")) %>%
  mutate(violation_outcome = tolower(violation_outcome))

success_by_country <- df_admissible %>%
  group_by(state_party) %>%
  summarise(
    total_cases = n(),
    successful_cases = sum(violation_outcome == "violation", na.rm = TRUE),
    success_rate = round(successful_cases / total_cases, 2)
  ) %>%
  arrange(desc(success_rate))

success_by_country

print(success_by_country)




#Most frequently invoked articles of the Optional Protocol
library(readxl)
library(dplyr)

df_op <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "OP_articles")

df_article_counts <- df_op %>%
  summarise(
    `Article_4(2)` = sum(`Article_4(2)` == 1, na.rm = TRUE),
    `Article_5(1)` = sum(`Article_5(1)` == 1, na.rm = TRUE),
    `Article_5(2)` = sum(`Article_5(2)` == 1, na.rm = TRUE),
    `Article_6`   = sum(`Article_6` == 1, na.rm = TRUE),
    `Article_7a`  = sum(`Article_7a` == 1, na.rm = TRUE),
    `Article_7b`  = sum(`Article_7b` == 1, na.rm = TRUE),
    `Article_7c`  = sum(`Article_7c` == 1, na.rm = TRUE),
    `Article_7d`  = sum(`Article_7d` == 1, na.rm = TRUE),
    `Article_7e`  = sum(`Article_7e` == 1, na.rm = TRUE),
    `Article_7f`  = sum(`Article_7f` == 1, na.rm = TRUE),
    `Article_7g`  = sum(`Article_7g` == 1, na.rm = TRUE),
    `Other`       = sum(`Other` == 1, na.rm = TRUE)
  )

article_counts <- as.data.frame(t(df_article_counts))
colnames(article_counts) <- "Count"
article_counts$Article <- rownames(article_counts)
article_counts <- article_counts %>% select(Article, Count) %>% arrange(desc(Count))

print(article_counts)


#Show which countries cite OP articles the most, and which articles are most cited per country.
library(dplyr)
library(tidyr)

op_long <- df_op %>%
  pivot_longer(
    cols = starts_with("Article_"),
    names_to = "Article",
    values_to = "Invoked"
  ) %>%
  filter(Invoked == 1)

# Count how many times each country invoked each article
article_by_country <- op_long %>%
  group_by(state_party, Article) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# View the result
print(article_by_country)



#Final: Average age for victims in admissable/partly admissable vs. inadmissable cases.
library(readxl)
library(dplyr)

df_main <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")
df_age <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "age_gender")

full_data <- left_join(df_main, df_age, by = "Case_ID")

full_data <- full_data %>%
  mutate(admissibility_group = case_when(
    case_outcome %in% c("admissible", "partly admissible") ~ "Admissible/Partly",
    case_outcome == "inadmissible" ~ "Inadmissible",
    TRUE ~ NA_character_
  ))

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

filtered_data_age <- full_data %>%
  filter(admissibility_group %in% c("Admissible/Partly", "Inadmissible")) %>% filter(!(Case_ID %in% excluded_cases))

filtered_data_age$age_of_victim <- as.numeric(filtered_data_age$Age)

average_age <- filtered_data_age %>%
  group_by(admissibility_group) %>%
  summarise(
    count = n(),
    average_age = mean(age_of_victim, na.rm = TRUE)
  )

print(average_age)




#Average Age Per Case
library(readxl)
library(dplyr)

df_main <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")
df_age <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "age_gender")

full_data <- left_join(df_main, df_age, by = "Case_ID")

full_data <- full_data %>%
  mutate(admissibility_group = case_when(
    case_outcome %in% c("admissible", "partly admissible") ~ "Admissible/Partly",
    case_outcome == "inadmissible" ~ "Inadmissible",
    TRUE ~ NA_character_
  ))

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

filtered_data_age_case <- full_data %>%
  filter(admissibility_group %in% c("Admissible/Partly", "Inadmissible")) %>%
  filter(!(Case_ID %in% excluded_cases)) %>%
  mutate(age_of_victim = as.numeric(Age))

case_level_ages <- filtered_data_age_case %>%
  group_by(Case_ID, admissibility_group) %>%
  summarise(case_avg_age = mean(age_of_victim, na.rm = TRUE), .groups = "drop")

average_case_age <- case_level_ages %>%
  group_by(admissibility_group) %>%
  summarise(
    number_of_cases = n(),
    average_age_per_case = mean(case_avg_age, na.rm = TRUE)
  )

print(average_case_age)




#Final: Gender differences between admissible/partly admissible vs. inadmissible cases.
library(readxl)
library(dplyr)

df_main <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")
df_gender <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "age_gender")

full_data <- left_join(df_main, df_gender, by = "Case_ID")

full_data <- full_data %>%
  mutate(admissibility_group = case_when(
    case_outcome %in% c("admissible", "partly admissible") ~ "Admissible/Partly",
    case_outcome == "inadmissible" ~ "Inadmissible",
    TRUE ~ NA_character_
  ))

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

filtered_data_gender <- full_data %>%
  filter(admissibility_group %in% c("Admissible/Partly", "Inadmissible")) %>%
  filter(!(Case_ID %in% excluded_cases))

filtered_data_gender_victim <- filtered_data_gender %>%
  mutate(Gender_clean = case_when(
    Gender %in% c("F", "f", "female", "Female") ~ "Female",
    Gender %in% c("M", "m", "male", "Male") ~ "Male",
    TRUE ~ "Missing"
  ))

gender_victim_counts <- filtered_data_gender_victim %>%
  group_by(admissibility_group, Gender_clean) %>%
  summarise(total_victims = n(), .groups = "drop")

print(gender_victim_counts)





#Final: Parents submitting the case between admissible/partly admissible vs. inadmissible cases.
library(readxl)
library(dplyr)

df_main <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")
df_gender <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "age_gender")

full_data <- left_join(df_main, df_gender, by = "Case_ID")

full_data <- full_data %>%
  mutate(admissibility_group = case_when(
    case_outcome %in% c("admissible", "partly admissible") ~ "Admissible/Partly",
    case_outcome == "inadmissible" ~ "Inadmissible",
    TRUE ~ NA_character_
  ))

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

filtered_data <- full_data %>%
  filter(admissibility_group %in% c("Admissible/Partly", "Inadmissible")) %>%
  filter(!(Case_ID %in% excluded_cases))

filtered_data <- filtered_data %>%
  mutate(
    age_of_victim = as.numeric(Age),
    Gender_clean = case_when(
      Gender %in% c("F", "f") ~ "Female",
      Gender %in% c("M", "m") ~ "Male",
      TRUE ~ "Missing"
    ),
    Parents_claim = case_when(
      claim_brought_by_parents %in% c("yes", "Yes") ~ "By Parents",
      claim_brought_by_parents %in% c("no", "No") ~ "Not by Parents",
      TRUE ~ "Unknown"
    )
  )

summary_table <- filtered_data %>%
  group_by(admissibility_group, Gender_clean, Parents_claim) %>%
  summarise(
    n = n(),
    avg_age = mean(age_of_victim, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(admissibility_group, Gender_clean, Parents_claim)

print(summary_table)




#Do parent-led cases overall lead to more violations? (includes admissable, partly admissable, and inadmissable)
filtered_data %>%
  filter(!Case_ID %in% c(
    "Argentina_CRC_C_88_D_104_2019.pdf",
    "France_CRC_C_88_D_106_2019.pdf",
    "Turkey_CRC_C_88_D_108_2019.pdf",
    "Brazil_CRC_C_88_D_105_2019.pdf",
    "Spain_CRC_C_91_D_116_2020",
    "Spain_CRC_C_91_D_117_2020",
    "Spain_CRC_C_91_D_118_2020"
  )) %>%
  group_by(Case_ID) %>%
  summarise(
    Parents_claim = case_when(
      any(claim_brought_by_parents %in% c("Yes")) ~ "By Parents",
      any(claim_brought_by_parents %in% c("No")) ~ "Not by Parents",
      TRUE ~ "Unknown"
    ),
    violation_outcome_case = ifelse(any(violation_outcome == "violation"), "violation", "no violation")
  ) %>%
  group_by(Parents_claim) %>%
  summarise(
    total_cases = n(),
    violations_found = sum(violation_outcome_case == "violation"),
    violation_rate = round(100 * violations_found / total_cases, 1)
  )



#Once a case gets admitted, is parental involvement associated with winning (i.e., finding a violation)?
library(dplyr)

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

filtered_data <- df_main %>%
  filter(case_outcome %in% c("admissible", "partly admissible", "inadmissible")) %>%
  filter(!Case_ID %in% excluded_cases) %>%
  mutate(violation_outcome = tolower(violation_outcome))

case_level_summary <- filtered_data %>%
  filter(case_outcome %in% c("admissible", "partly admissible")) %>%
  group_by(Case_ID) %>%
  summarise(
    Parents_claim = case_when(
      any(claim_brought_by_parents == "Yes") ~ "By Parents",
      any(claim_brought_by_parents == "No") ~ "Not by Parents",
      TRUE ~ "Unknown"
    ),
    violation_outcome_case = ifelse(any(violation_outcome == "violation"), "violation", "no violation"),
    .groups = "drop"
  )

case_level_summary %>%
  group_by(Parents_claim) %>%
  summarise(
    total_cases = n(),
    violations_found = sum(violation_outcome_case == "violation"),
    violation_rate = round(100 * violations_found / total_cases, 1),
    .groups = "drop"
  )





#Do parents help a case get past the admissibility filter in the first place?
library(dplyr)

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

filtered_data <- df_main %>%
  filter(case_outcome %in% c("admissible", "partly admissible", "inadmissible")) %>%
  filter(!Case_ID %in% excluded_cases)

case_level <- filtered_data %>%
  group_by(Case_ID) %>%
  summarise(
    Parents_claim = case_when(
      any(claim_brought_by_parents == "Yes") ~ "By Parents",
      any(claim_brought_by_parents == "No") ~ "Not by Parents",
      TRUE ~ "Unknown"
    ),
    case_outcome = unique(case_outcome),
    .groups = "drop"
  ) %>%
  mutate(
    admissibility = case_when(
      case_outcome %in% c("admissible", "partly admissible") ~ "admissible",
      case_outcome == "inadmissible" ~ "inadmissible",
      TRUE ~ "other"
    )
  )

case_level %>%
  group_by(Parents_claim) %>%
  summarise(
    total_cases = n(),
    admissible_cases = sum(admissibility == "admissible"),
    admissibility_rate = round(100 * admissible_cases / total_cases, 1),
    .groups = "drop"
  )





#Compare Rejection Reasons by Inadmissibility Category
library(readxl)
library(dplyr)

# Load your data
df <- Final_Data_UN_CRC

# Define list of duplicate Case_IDs to exclude
duplicates <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

# Filter out duplicates and only keep valid inadmissible cases
inadmissible <- df %>%
  filter(!(Case_ID %in% duplicates)) %>%
  filter(case_outcome == "inadmissible",
         inadmissibility_category != "Not applicable",
         inadmissibility_category != "N/A")

# Summarize by claim type and inadmissibility reason
summary_table <- inadmissible %>%
  group_by(claim_brought_by_parents, inadmissibility_category) %>%
  summarise(n_cases = n(), .groups = "drop") %>%
  group_by(claim_brought_by_parents) %>%
  mutate(percentage = round(100 * n_cases / sum(n_cases), 1))

# View the result
print(summary_table)



misframing_candidates <- df %>%
  filter(case_outcome == "inadmissible",
         !(Case_ID %in% duplicates),
         claim_brought_by_parents %in% c("Yes", "No")) %>%
  select(Case_ID, claim_brought_by_parents)




#Most frequent subjects:
library(dplyr)
library(tidyr)

# Load data
df <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")

# List of cases to exclude
excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

# Process subject matter categories with exclusion
df_long <- df %>%
  filter(!Case_ID %in% excluded_cases) %>%
  select(Case_ID, subject_matter_category) %>%
  filter(!is.na(subject_matter_category)) %>%
  mutate(subject_matter_category = tolower(subject_matter_category)) %>%
  separate_rows(subject_matter_category, sep = ";") %>%
  mutate(subject_matter_category = trimws(subject_matter_category)) %>%
  filter(subject_matter_category != "")

subject_counts <- df_long %>%
  group_by(subject_matter_category) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(subject_counts)




#Subject Matter by Country
library(readxl)
library(dplyr)
library(tidyr)

df <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")

excluded_cases <- c(
  "Argentina_CRC_C_88_D_104_2019.pdf",
  "France_CRC_C_88_D_106_2019.pdf",
  "Turkey_CRC_C_88_D_108_2019.pdf",
  "Brazil_CRC_C_88_D_105_2019.pdf",
  "Spain_CRC_C_91_D_116_2020",
  "Spain_CRC_C_91_D_117_2020",
  "Spain_CRC_C_91_D_118_2020"
)

df_long <- df %>%
  filter(!Case_ID %in% excluded_cases) %>%
  select(Case_ID, state_party, subject_matter_category) %>%
  filter(!is.na(subject_matter_category)) %>%
  mutate(subject_matter_category = tolower(subject_matter_category)) %>%
  separate_rows(subject_matter_category, sep = ";") %>%
  mutate(subject_matter_category = trimws(subject_matter_category)) %>%
  filter(subject_matter_category != "")

subject_by_country <- df_long %>%
  group_by(state_party, subject_matter_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

print(subject_by_country)




#Violation outcome and gender
library(readxl)
library(dplyr)
library(stringr)

df_main <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")
df_age <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "age_gender")

df_combined <- left_join(
  df_main %>% mutate(Case_ID = str_trim(Case_ID)),
  df_age %>% mutate(Case_ID = str_trim(Case_ID)),
  by = "Case_ID"
)

df_combined_filtered <- df_combined %>%
  mutate(
    Gender = str_trim(Gender),
    Gender = ifelse(is.na(Gender) | Gender == "" | Gender == "N/A", "N/A", Gender),
    violation_outcome = tolower(trimws(violation_outcome)),
    violation_outcome = ifelse(violation_outcome == "violation", "violation", "no_violation")
  )

df_clean <- df_combined_filtered %>%
  filter(case_outcome %in% c("admissible", "partly admissible"))

children_outcome_table <- df_clean %>%
  group_by(Gender, violation_outcome) %>%
  summarise(Victims = n(), .groups = "drop") %>%
  arrange(Gender, violation_outcome)

print(children_outcome_table)





#Nationality
library(readxl)
library(dplyr)
library(writexl)

df <- read_excel("Final_Data_UN_CRC.xlsx", sheet = "final_extraction_results")

df <- df %>%
  mutate(
    nationality_of_victim = tolower(nationality_of_victim),
    nationality_of_victim = str_trim(nationality_of_victim),
    nationality_of_victim = case_when(
      str_detect(nationality_of_victim, "moroc+") ~ "Moroccan",
      str_detect(nationality_of_victim, "spani+") ~ "Spanish",
      str_detect(nationality_of_victim, "syria+") ~ "Syrian",
      str_detect(nationality_of_victim, "afghan") ~ "Afghan",
      str_detect(nationality_of_victim, "iraq") ~ "Iraqi",
      TRUE ~ str_to_title(nationality_of_victim)
    )
  )

table <- df %>%
  count(nationality_of_victim, sort = TRUE)

write_xlsx(table, "nationality_summary.xlsx")


