


np_2021 <- readRDS("data/np_2021.rds")
np_2023 <- readRDS("data/np_2023.rds")
skills <- read.csv("data/tech_skills_keywords.csv", header = FALSE, stringsAsFactors = FALSE)[,1]

patterns <- paste0("\\b", skills, "\\b")


skill_counts_2021 <- tibble(
  skill = skills,
  pattern = patterns
) %>%
  rowwise() %>%
  mutate(
    count = sum(str_detect(np_2021$noun_phrase, regex(pattern, ignore_case = TRUE)))
  ) %>%
  ungroup() %>%
  filter(count > 0) %>%
  rename(
    word = skill,
    freq = count
  ) %>% 
  select(word, freq) %>% 
  arrange(desc(freq)) 

skill_counts_2023 <- tibble(
  skill = skills,
  pattern = patterns
) %>%
  rowwise() %>%
  mutate(
    count = sum(str_detect(np_2023$noun_phrase, regex(pattern, ignore_case = TRUE)))
  ) %>%
  ungroup() %>%
  filter(count > 0) %>%
  rename(
    word = skill,
    freq = count
  ) %>% 
  select(word, freq) %>% 
  arrange(desc(freq)) 
