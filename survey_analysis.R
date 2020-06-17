###=== DATA AMBASSADORS: SURVEY ANALYSIS ==========================

###--- PREP WORKSPACE ---------------------------------------------

## Set working directory and clear memory
setwd("/Users/shannoncarter/Documents/JanuaryAdvisors/CC-DataAmbassadors")
rm(list = ls(all = T))

library(tidyverse)
library(ggstance)

mytheme <- theme_bw(base_size = 15, base_family = "Franklin Gothic Medium") +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        text = element_text(size = 14),     
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10, family = "Franklin Gothic Medium"),
        panel.grid = element_blank())

df <- read.csv("survey_data.csv")

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
  labs(x = NULL,
       y = "number of responses")
###--- DELTA PLOTS -----------------------------------------------

df2 <- df %>% 
  filter(!is.na(confident_managing_data)) %>% 
  pivot_longer(cols = confident_managing_data:onboard, 
               names_to = "question", values_to = "response") %>% 
  mutate(time = factor(time, levels = c("pre", "post"))) %>% 
  mutate(response = factor(response, levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree")))

pd <- ggstance::position_dodgev(0.3)
plot1 <- df2 %>% 
  filter(question == "confident_managing_data") %>% 
  ggplot(aes(x = time, y = response, color = person, group = person)) +
  geom_point(position = pd) + 
  geom_line(position = pd) +
  mytheme +
  labs(x = NULL,
       y = NULL,
       title = "I feel confident managing my data") +
  theme(legend.position = 'none')
plot1
