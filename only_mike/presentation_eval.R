library(tidyverse)


dt = rio::import(file.path(getwd(), "only_mike", "Presentation Evaluation.csv")) %>%
  
  # select questionnaire only
  select(starts_with("Q")) %>%
  
  # remove responses with missing "presentation group"
  filter(Q5 != "") %>%
  
  # remove the first two rows (Question detail, Import)
  slice(-(1:2)) %>%
  
  # remove responses with the same presentation and judge group
  filter(Q4 != Q5) %>%
  
  # change eval to number class
  mutate(
    style = as.numeric(Q6_1),
    content = as.numeric(Q6_2),
    visual_aid = as.numeric(Q6_3),
    creativity_interest = as.numeric(Q6_4)
  ) %>%
  
  mutate(total_eval = style + content + visual_aid + creativity_interest)  %>%
  
  # get only the highest eval from the judge team (i.e., remove additional lower ones)
  group_by(Q5, Q4) %>%
  arrange(desc(total_eval)) %>%
  slice(1) %>%
  ungroup()
  

# check if all groups submit their presentation evaluations 
dt$Q4 %>% table()

eval = dt %>%
  group_by(Q5)  %>%
  summarise(
    mean_style = mean(style),
    mean_content = mean(content),
    mean_visual_aid = mean(visual_aid),
    mean_creativity_interest = mean(creativity_interest)
  ) %>% 
  ungroup()



library(plyr)
comment = dt %>%
  ddply(.(Q5),
        summarize,
        pos = paste(paste(Q7, collapse = "; "), paste(Q8, collapse = "; ")),
        sug = paste(paste(Q9, collapse = "; "), paste(Q10, collapse = "; ")))

overall = eval %>%
  full_join(comment, by = "Q5") %>%
  dplyr::rowwise() %>%
  # dplyr::mutate(peer_eval = mean(c_across(starts_with("mean"))))
  
  dplyr::mutate(peer_eval = mean(c(
    mean_style,
    mean_content,
    mean_visual_aid,
    mean_creativity_interest
  )))
  
 

overall


library(googlesheets4)


instructor_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1EmvxRjSobmu4Wcl0rrez1PjnlX6ky3J7pRfFn-Hjw1w/edit#gid=0")
instructor_sheet_cleaned <- instructor_sheet %>% 
  mutate(instructor_pos = paste0(pos_1,"; ", pos_2)) %>% 
  mutate(instructor_sug = paste0(sug_1,"; ", sug_2)) %>% 
  select(-c(pos_1,pos_2,sug_1,sug_2))


final = overall %>%
  dplyr::rename(Group = Q5) %>% 
  
  inner_join(instructor_sheet_cleaned, by = "Group") %>% 
  
  select(Group, starts_with("mean"), peer_eval, pos, sug, instructor_eval, instructor_pos, instructor_sug) %>% 
  mutate(peer_eval_reweight = peer_eval * 2) %>% 
  mutate(total_eval = peer_eval_reweight + instructor_eval)


for (i in 1:nrow(final)) {
  subset = final[i,]
  rio::export(subset,file.path(getwd(),"only_mike","group_eval",paste0(final$Group[i],".xlsx")))
  rm(subset)
}


# grade for Presentation Assignment        

# presentation submission (10 points)
# Instructor eval (10 points) 
# Peers' eval (20 points)