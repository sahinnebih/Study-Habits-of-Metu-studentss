# Study Habits of Metu studentss
 The aim of our study is to examine the study habits of the students and whether they have an effect on their academic success or satisfaction. I dd it with Rstudio


library(likert)
library(readr)
library(dplyr)
library(ggplot2)
library(MASS)
library(tidyverse)
library(readxl)
library(Hmisc)
library(readxl)
sh <- read_excel("C:/Users/sahin/Downloads/sh2.xlsx", 
                 col_types = c("skip", "text", "text", 
                               "text", "text", "text", "numeric", 
                               "text", "text", "text", "text", "numeric", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "numeric"))
View(sh)


#
sh$`6) What is your CGPA?` =  as.numeric(sh$`6) What is your CGPA?`)


sh$text[sh$`Please rate the statements below. [32) I look at my cellphone or send text messages during class]` == "Never"] = "1"
sh$text[sh$`Please rate the statements below. [32) I look at my cellphone or send text messages during class]` == "Rarely"] = "2"
sh$text[sh$`Please rate the statements below. [32) I look at my cellphone or send text messages during class]` == "Sometimes"] = "3"
sh$text[sh$`Please rate the statements below. [32) I look at my cellphone or send text messages during class]` == "Often"] = "4"
sh$text[sh$`Please rate the statements below. [32) I look at my cellphone or send text messages during class]` == "Always"] = "5"

sh$text =  as.numeric(sh$text)

sh$`7) What is your relationship status?` = as.factor(sh$`7) What is your relationship status?`)


sh$`24)Are you satisfied with your grades?` = as.factor(sh$`24)Are you satisfied with your grades?`)






#
sh %>%
  filter(`7) What is your relationship status?` == "Single" |
           `7) What is your relationship status?` == "In a relationship") %>%
  ggplot(aes(`7) What is your relationship status?`, fill = `24)Are you satisfied with your grades?`))+
  geom_bar(position = "dodge",
           alpha = 0.6)+
  labs(title="",
       x="", y= "count")+
  #geom_text(aes(label = round(`24)Are you satisfied with your grades?`, 1)))+
  #geom_text(aes(label = signif(`24)Are you satisfied with your grades?`, digits = 3)), nudge_y = 4)+
  #scale_y_continuous(labels = scales::percent,
  #breaks = scales::pretty_breaks(7))+
  theme_bw()

summary(sh$`24)Are you satisfied with your grades?`)





#
describe(sh$`1) What gender do you identify as?`)




#
sh %>%
  filter(!(`1) What gender do you identify as?` %in% "Prefer not to say")) %>%
  filter(!(`7) What is your relationship status?` %in% 
             "Married")) %>%
  ggplot() +
  aes(
    x = `1) What gender do you identify as?`,
    fill = `7) What is your relationship status?`
  ) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c(`In a relationship` = "#A80BB7",
               Married = "#00C19F",
               Single = "#0AB05D")
  ) +
  labs(
    x = "Gender",
    y = "Count",
    fill = "Relationship Status"
  ) +
  theme_linedraw() +
  theme(
    axis.title.y = element_text(size = 18L,
                                face = "bold.italic"),
    axis.title.x = element_text(size = 20L,
                                face = "bold.italic")
  )






#
mean(sh$`34) Can you rate your satisfaction with your academic life from 1 to 5?`, na.rm=TRUE)




#
mean(sh$`6) What is your CGPA?`, na.rm=TRUE)

sh %>%
  filter(!(`1) What gender do you identify as?` %in% "Prefer not to say")) %>%
  ggplot() +
  aes(
    x = `6) What is your CGPA?`,
    y = `1) What gender do you identify as?`
  ) +
  geom_boxplot(fill = "#FF69B4") +
  geom_jitter() +
  labs(x = "CGPA", y = "Gender") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 20L,
                                face = "bold.italic"),
    axis.title.x = element_text(size = 20L,
                                face = "bold.italic")
  )



#
mean(sh$`6) What is your CGPA?`, na.rm=TRUE)

sh %>%
  filter(!(`1) What gender do you identify as?` %in% "Prefer not to say")) %>%
  ggplot() +
  aes(
    x = `6) What is your CGPA?`,
    y = `1) What gender do you identify as?`
  ) +
  geom_boxplot(fill = "#FF69B4") +
  geom_jitter() +
  labs(x = "CGPA", y = "Gender") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 20L,
                                face = "bold.italic"),
    axis.title.x = element_text(size = 20L,
                                face = "bold.italic")
  )



#
sh %>%
  filter(!(`1) What gender do you identify as?` %in% "Prefer not to say")) %>%
  ggplot() +
  aes(
    x = `24)Are you satisfied with your grades?`,
    fill = `9) Do you use tobacco products?`
  ) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c(No = "#B06617",
               Yes = "#1CBDCC")
  ) +
  labs(
    x = "Satisfaction with Grades",
    y = "Count",
    fill = "Usage of Tobacco Products"
  ) +
  theme_linedraw() +
  theme(
    axis.title.y = element_text(size = 20L,
                                face = "bold.italic"),
    axis.title.x = element_text(size = 18L,
                                face = "bold.italic")
  ) +
  facet_wrap(vars(`1) What gender do you identify as?`))





#
habit=sh
new3=data.frame(as.factor(habit$`22) Do you agree that being organized is important for studying?`))
names(new3)=c("Do you agree that being organized 
               is important for studying?")
new3_likert=likert(new3,grouping = habit$`18) Do you prefer to work alone or with a group of friends?` )
likert.bar.plot(new3_likert,text.size=4.5, legend.position = "bottom")



#


new1=data.frame(as.factor(habit$`Please rate the statements below. [28) I set aside a regular time each day to study.]`),
                as.factor(habit$`Please rate the statements below. [ 29) Before I read a chapter, I turn headings into questions so that I know what I’m going to learn.]`) ,
                as.factor(habit$`Please rate the statements below. [30) I take notes while studying or in class.]`),
                as.factor(habit$`Please rate the statements below. [31) I care to listen to the lecture in class]`)
)

new11=data.frame(as.factor(habit$`Please rate the statements below. [28) I set aside a regular time each day to study.]`),
                 as.factor(habit$`Please rate the statements below. [ 29) Before I read a chapter, I turn headings into questions so that I know what I’m going to learn.]`) ,
                 as.factor(habit$`Please rate the statements below. [30) I take notes while studying or in class.]`),
                 as.factor(habit$`Please rate the statements below. [31) I care to listen to the lecture in class]`)
)
names(new11)=c("I set aside a regular time each day to study.",
               "Before I read a chapter, I turn headings into questions so that I know what I’m going to learn.",
               "I take notes while studying or in class.",
               "I care to listen to the lecture in class")
names(new1)=c("a","b","c","d")

new1_likert=likert(new1)
Gender=habit$`1) What gender do you identify as?`
new11_likert=likert(new11, grouping = Gender)

likert.density.plot(new11_likert,text.size=4.5, legend.position = "bottom")

likert.bar.plot(new1_likert,text.size=4.5, legend.position = "bottom")    

likert.heat.plot(new1_likert)



#

new2=data.frame(as.factor(habit$`Please rate the statements below. [26) I schedule my study plan for the coming week at the beginning of each week.]`),
                as.factor(habit$`Please rate the statements below. [27) I study while watching TV or listening to music at]`) ,
                as.factor(habit$`Please rate the statements below. [32) I look at my cellphone or send text messages during class]`),
                as.factor(habit$`Please rate the statements below. [33) I take naps during class.]`)
)

new22=data.frame(as.factor(habit$`Please rate the statements below. [26) I schedule my study plan for the coming week at the beginning of each week.]`),
                 as.factor(habit$`Please rate the statements below. [27) I study while watching TV or listening to music at]`) ,
                 as.factor(habit$`Please rate the statements below. [32) I look at my cellphone or send text messages during class]`),
                 as.factor(habit$`Please rate the statements below. [33) I take naps during class.]`)
)
names(new2)= c("a","b","c","d")

names(new22)=c(" I schedule my study plan for the coming week at the beginning of each week.",
               "I study while watching TV or listening to music at",
               "I look at my cellphone or send text messages during class",
               "I take naps during class.")

new2_likert=likert(new2)

new22_likert=likert(new22,grouping = habit$`1) What gender do you identify as?`)

likert.density.plot(new22_likert,text.size=9.5, legend.position = "bottom")

likert.bar.plot(new2_likert,text.size=4.5, legend.position = "bottom")    

likert.heat.plot(new2_likert,text.size = 5.5 )








#
shapiro.test(sh$`6) What is your CGPA?`)




#
plot(density(sh$`6) What is your CGPA?`),"Density of cGPA")
