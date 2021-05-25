###=== DATA AMBASSADORS: SURVEY ANALYSIS ==========================

###--- PREP WORKSPACE ---------------------------------------------

## Set working directory and clear memory
setwd("/Users/shannoncarter/Documents/JanuaryAdvisors/CC-DataAmbassadors")
rm(list = ls(all = T))

library(tidyverse)
library(ggstance)
library(gridExtra)

mytheme <- theme_bw(base_size = 15, base_family = "Franklin Gothic Medium") +
  theme(panel.grid = element_blank())

cc_df <- read.csv("survey_data.csv")

###--- POST-SURVEY HISTOGRAMS -----------------------------------

post <- df %>% 
  filter(time == "post") %>% 
  filter(!is.na(confident_managing_data)) %>% 
  pivot_longer(cols = confident_managing_data:onboard, 
               names_to = "question", values_to = "response") %>% 
  mutate(response = factor(response, levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree")))

ggplot(post, aes(x = response)) +
  geom_histogram(stat = "count") +
  facet_wrap(~question) +
  mytheme +
  scale_x_discrete(drop = F) +
  labs(x = NULL,
       y = "number of responses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###--- DELTA PLOTS -----------------------------------------------

df2 <- df %>% 
  filter(!is.na(confident_managing_data)) %>% 
  pivot_longer(cols = confident_managing_data:onboard, 
               names_to = "question", values_to = "response") %>% 
  mutate(time = factor(time, 
                       levels = c("pre", "post"))) %>% 
  mutate(response = factor(response, 
                           levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree")))

pd <- ggstance::position_dodgev(0.3)
q1 <- df2 %>% 
  filter(question == "confident_managing_data") %>% 
  ggplot(aes(x = time, y = response, color = person, group = person)) +
  geom_point(position = pd, size = 2) + 
  geom_line(position = pd, size = 1.5) +
  mytheme +
  labs(x = NULL,
       y = NULL,
       title = "I feel confident managing my data") +
  theme(legend.position = 'none') +
  scale_y_discrete(drop = F)

q2 <- df2 %>% 
  filter(question == "quickly_find_data") %>% 
  ggplot(aes(x = time, y = response, color = person, group = person)) +
  geom_point(position = pd, size = 2) + 
  geom_line(position = pd, size = 1.5) +
  mytheme +
  labs(x = NULL,
       y = NULL,
       title = "I can quickly find my data when I need it") +
  theme(legend.position = 'none') +
  scale_y_discrete(drop = F)

q3 <- df2 %>% 
  filter(question == "comfortable_with_colleagues") %>% 
  ggplot(aes(x = time, y = response, color = person, group = person)) +
  geom_point(position = pd, size = 2) + 
  geom_line(position = pd, size = 1.5) +
  mytheme +
  labs(x = NULL,
       y = NULL,
       title = "I feel comfortable talking to my colleagues about data") +
  theme(legend.position = 'none') +
  scale_y_discrete(drop = F)

q4 <- df2 %>% 
  filter(question == "best_practices") %>% 
  ggplot(aes(x = time, y = response, color = person, group = person)) +
  geom_point(position = pd, size = 2) + 
  geom_line(position = pd, size = 1.5) +
  mytheme +
  labs(x = NULL,
       y = NULL,
       title = "I understand best practices on where to store data") +
  theme(legend.position = 'none') +
  scale_y_discrete(drop = F)

q5 <- df2 %>% 
  filter(question == "eto") %>% 
  ggplot(aes(x = time, y = response, color = person, group = person)) +
  geom_point(position = pd, size = 2) + 
  geom_line(position = pd, size = 1.5) +
  mytheme +
  labs(x = NULL,
       y = NULL,
       title = "I understand how to add new data to ETO") +
  theme(legend.position = 'none') +
  scale_y_discrete(drop = F)

q1
q2
q3
q4
q5
grid.arrange(q1, q2, q4, q5)
