# library
library(tidyverse)

# export your data to csv (use choice text)
# change name to "final_eval.csv"



# manually check if students input their members' names correctly

get_name_init = function(full.name) {
  if (nchar(full.name) > 2) {
    test1 = strsplit(toupper(full.name), " ")
    test2 = lapply(test1, function(name) {
      substr(name, start = 1, stop = 1)
    })
    test3 = lapply(test2, function(name) {
      paste0(name, collapse = "")
    })
    return(test3[[1]])
  } else {
    return(full.name)
  }
}

# import 
data = read.csv(file.path(getwd(), "only_mike", "final_eval.csv")) %>%
  
  slice(-c(1,2)) %>% # remove 2 and 3 rows (questions description) 
  
  # change members name 
  rename(member1 = Q5_1,
         member2 = Q5_2,
         member3 = Q5_3,
         member4 = Q5_4,
         member5 = Q5_5,
         member6 = Q5_6) %>% # 6 is rater's own evaluation
  
  # rename other variables
  rename(name = Q1, 
         id = Q2,
         group = Q3, 
         overall = Q4, 
         comment = Q10) %>% 
  
  # make sure percentage questions are in numeric format
  mutate_at(vars(starts_with("Q")), as.numeric) %>% 
  
  # take sum across columns
  mutate(score_1 = select(.,ends_with("_1")) %>% rowSums()) %>% 
  mutate(score_2 = select(.,ends_with("_2")) %>% rowSums()) %>% 
  mutate(score_3 = select(.,ends_with("_3")) %>% rowSums()) %>% 
  mutate(score_4 = select(.,ends_with("_4")) %>% rowSums()) %>% 
  mutate(score_5 = select(.,ends_with("_5")) %>% rowSums()) %>% 
  mutate(score_6 = select(.,ends_with("_6")) %>% rowSums()) 


data = data %>% 
  mutate_if(is.character, toupper) %>% 
  mutate_if(is.character, str_trim)

for (i in 1:nrow(data)){
  data$member1[i] = get_name_init(data$member1[i])
  data$member2[i] = get_name_init(data$member2[i])
  data$member3[i] = get_name_init(data$member3[i])
  data$member4[i] = get_name_init(data$member4[i])
  data$member5[i] = get_name_init(data$member5[i])
  data$member6[i] = get_name_init(data$member6[i])
}

# view(data)

name_abbre = data %>% 
  select(name, group) 


for (i in 1:nrow(data)) {
  name_abbre$abb_name[i] = get_name_init(name_abbre$name[i])
}
name_abbre = name_abbre %>% mutate(member_rated = paste0(abb_name,"_", group)) %>% select(name, member_rated)
view(name_abbre)

test = data %>% 
  
  # transform data 
  # select(starts_with("member")| starts_with("score"), name) %>%
  pivot_longer(
    cols = starts_with("score"),
    names_to = "order",
    values_to = "evaluation"
  ) %>% 
  pivot_longer(
    cols = starts_with("member"),
    names_to = "order1",
    values_to = "member_rated"
  ) %>% 
  
  mutate(order = str_sub(order, 7,8 )) %>% 
  mutate(order1 = str_sub(order1, 7,8 )) %>% 
  mutate(match = if_else(order == order1, 1, 0)) %>% 
  filter(match ==1 ) %>% 
  select(-c(order, order1, match)) %>% 
  
  filter(member_rated != "") %>%
  mutate(member_rated = paste0(member_rated,"_", group))  # mutate member_rated to be unique (first 2 initials = members name, second 2 initials = group name)



test1= test %>% 
  
  # get group size
  filter(evaluation !=0) %>% 
  group_by(group, name) %>% 
  summarise(group_size = n()) %>% 
  group_by(group) %>% 
  mutate(group_size = max(group_size)) %>% 
  mutate(supposed_contribution = 100/group_size) 


test2 = test %>% 
  full_join(test1, by = c("name","group")) %>% 

  # get how each rater rates their peers on multiple assignments
  mutate(within_rater_evaluation = evaluation/group_size) 


test3 = test2 %>% 
  # get average score across raters
  group_by(member_rated) %>% 
  summarise(final_score = mean(within_rater_evaluation))
  
test4 = test3 %>% 
  full_join(test2, by = ("member_rated")) %>% 
  mutate(weight = if_else(final_score >= supposed_contribution,1,final_score/supposed_contribution)) 
  
  
final_table = test4 %>% 
  select(member_rated,weight, group) %>% 
  full_join(name_abbre, by = "member_rated") %>% 
  unique() %>% 
  view()


mid_weight = rio::import(file.path(getwd(), "only_mike", "mid_weight.csv"))

report = final_table %>% 
  full_join(mid_weight, by= c("member_rated", "group")) %>% 
  
  dplyr::rename(final_weight = weight.x , mid_weight = weight.y) %>% 
  
  # round all the weights up
  
  dplyr::mutate(final_weight = round(final_weight,digits = 1), mid_weight = round(mid_weight,digits = 1)) %>% 
  # get final weight
  
  dplyr::mutate(report_weight = round(1/3*mid_weight + 2/3*final_weight, digits = 1)) %>% 
  
  # calculate group score
  dplyr::mutate(
    group_grade = case_when(
      group == "AD" ~ 60,
      group == "AG" ~ 50,
      group == "JB" ~ 46,
      group == "NC" ~ 65,
      group == "RL" ~ 45,
      group == "SB" ~ 60, 
      TRUE ~ 0
    )
  ) %>% 
  
  
  # calculate individual score
  dplyr::mutate(individual_grade = group_grade * report_weight) %>% 
  view()
  

rio::export(report, file.path(getwd(), "only_mike", "report_eval.csv"))

