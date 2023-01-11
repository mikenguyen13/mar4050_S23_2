library(tidyverse)
letter_grade = rio::import(file.path(getwd(), "only_mike","letter_grade", "letter_grade.csv")) %>%
  mutate(`Final Points` = as.numeric(`Final Points`)) %>% 
  mutate(`Final Points` = `Final Points` + 10) %>% 
  mutate(`Final Points` = plyr::round_any(`Final Points`,10, f = ceiling)) %>% 
  
  mutate(
    letter_grade = case_when(
      `Final Points` >= 970 ~ "A+",
      `Final Points` >= 940 ~ "A",
      `Final Points` >= 900 ~ "A-",
      `Final Points` >= 870 ~ "B+",
      `Final Points` >= 840 ~ "B",
      `Final Points` >= 800 ~ "B-",
      `Final Points` >= 770 ~ "C+",
      `Final Points` >= 740 ~ "C",
      `Final Points` >= 700 ~ "C-",
      `Final Points` >= 670 ~ "D+",
      `Final Points` >= 640 ~ "D",
      `Final Points` >= 610 ~ "D-",
      `Final Points` >= 0 ~ "F",
      TRUE ~ "NA"
    )
  ) %>%
  
  select(
    Student,
    ID,
    `SIS User ID`,
    `SIS Login ID`,
    Section,
    `Final Points`,
    `Current Grade`,
    `Unposted Current Grade`,
    `Final Grade`,
    `Unposted Final Grade`,
    letter_grade
  ) %>%
  
  view()

rio::export(letter_grade %>% select(
  -c(
    `Final Points`,
    `Current Grade`,
    `Unposted Current Grade`,
    `Final Grade`,
    `Unposted Final Grade`,
  )
), file.path(getwd(), "only_mike", "letter_grade","letter_grade_canvas.csv"))
