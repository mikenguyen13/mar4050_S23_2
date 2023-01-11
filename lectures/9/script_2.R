# library
library(ggplot2)
library(tidyverse)

# set working directory
# getwd() # check current working directory
setwd("C:/Users/tn9k4/GitHub/courses_teach/mizzou/mar4050_F21/lectures/9")

descrip_1 = readxl::read_excel("Cross-tab worksheet.xlsx",sheet = "Visualization - Descriptive 1") 
descrip_1 = na.omit(descrip_1)

descrip_2 = readxl::read_excel("Cross-tab worksheet.xlsx",sheet = "Visualization - Descriptive 2")

diff_1 = readxl::read_excel("Cross-tab worksheet.xlsx",sheet = "Qualtrics Diff RQ practice 1")
diff_1 = na.omit(diff_1)

diff_2 = readxl::read_excel("Cross-tab worksheet.xlsx",sheet = "Qualtrics Diff RQ practice 2")
diff_2 = na.omit(diff_2)

# Descriptive 1
# percent
ggplot(
    descrip_1,
    aes(
        y = descrip_1$`Percent Preference Each of the Three Salad Dressing (n=270)`,
        x = descrip_1$`Salad Dressings`
    )
) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Salad Dressings") +
    ylab("Percent") +
    ggtitle("Percent Preference")


# count
ggplot(descrip_1,
       aes(y = descrip_1$Count,
           x = descrip_1$`Salad Dressings`)) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Salad Dressings") +
    ylab("Count") +
    ggtitle("Preference")


# Descriptive 2
# change data format
descrip_2_new = descrip_2 %>% 
    pivot_longer(!Side, names_to = "Salad Dressing", values_to = "Percent") 

ggplot(
    descrip_2_new,
    aes(
        fill = descrip_2_new$`Salad Dressing`,
        y = descrip_2_new$Percent,
        x = descrip_2_new$Side
    )
) + 
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Side") +
    ylab("Percent") +
    ggtitle("Preference") + 
    guides(fill = guide_legend(title = "Salad Dressing"))


# Diff RQ 1
# change data format
diff_1_new = diff_1 %>% 
    pivot_longer(!Gender, names_to = "Mode of Purchase", values_to = "Purchase Count") 

ggplot(
    diff_1_new,
    aes(
        fill = diff_1_new$Gender,
        y = diff_1_new$`Purchase Count`,
        x = diff_1_new$`Mode of Purchase`
    )
) + 
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Mode of Purchase") +
    ylab("Count") +
    ggtitle("Difference Chart") + 
    guides(fill = guide_legend(title = "Gender"))


# Diff RQ 2
ggplot(
    diff_2,
    aes(
        y = diff_2$Satisfaction_gift_shopping,
        x = diff_2$Gender
    )
) + 
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Mode of Purchase") +
    ylab("Satisfaction Level") +
    ggtitle("Satisfaction Gift Shopping") 

