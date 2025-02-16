
library(data.table)
library(tidyverse)
library(rvest)

studentdata = list(
  Fall2020 = read.delim("data/Fall2020.tsv"),
  Fall2019 = read.delim("data/Fall2019.tsv"),
  Fall2018 = read.delim("data/Fall2018.tsv"),
  Fall2017 = read.delim("data/Fall2017.tsv"),
  Fall2016 = read.delim("data/Fall2016.tsv"),
  Spring2021 = read.delim("data/Spring2021.tsv"),
  Spring2020 = read.delim("data/Spring2020.tsv"),
  Spring2019 = read.delim("data/Spring2019.tsv"),
  Spring2018 = read.delim("data/Spring2018.tsv"),
  Spring2017 = read.delim("data/Spring2017.tsv")
) %>%
  rbindlist()

ex = studentdata %>% group_by(InstructorName) %>% reframe(Good_count = length(which(FinalGrade >= "B-")), N = n(), Good_pct = Good_count/N) %>% arrange(-desc(Good_pct))

statistics = read_html("https://catalog.luc.edu/undergraduate/arts-sciences/mathematics-statistics/statistics-bs/#curriculumtext") %>%
  html_table() %>%
  bind_rows() %>%
  mutate_all(~ifelse(. == "", NA, .)) %>%
  mutate(Code = case_when(Title == "Introduction to Biostatistics" ~ "STAT 335",
                          .default = Code),
         Hours = as.numeric(Hours),
         Hours = case_when(Title == "Introduction to Biostatistics" ~ 3,
                           .default = Hours))

statistics2 = statistics %>%
  filter(!is.na(Code)) %>%
  mutate(Group = ifelse(grepl("Select .* of the following:", Code), Title, NA)) %>%
  fill(Group, Hours, .direction = "down") %>%
  mutate(Hours_adj = case_when(Group == "Select two of the following:" ~ 2,
                               Group == "Select three of the following:" ~ 3,
                               .default = Hours),
         Hours_final = ifelse(!is.na(Group), Hours/Hours_adj, Hours)) %>%
  filter(grepl("\\d{3}", Code)) %>%
  select(Code, Title, Hours_final, Group)
