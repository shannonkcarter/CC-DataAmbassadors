
library(tidyverse)
library(googlesheets4)

mytheme <- theme_bw(base_size = 15, base_family = "Franklin Gothic Medium") +
  theme(panel.grid = element_blank())

pre <- read_sheet("https://docs.google.com/spreadsheets/d/1HMe_zjXTHrhgKWQmx_r2s9WLx-36MpdSRXW7NCBBPUw/edit#gid=1402504997")
post <- read_sheet("https://docs.google.com/spreadsheets/d/1WeBHfjruh99MxddPPmgE2roPruvjGOuR7-4N4b35Pfc/edit?resourcekey#gid=1474246294")

df <- pre %>% 
  select(-c("Timestamp", "Email Address", `What are some of the top challenges you face on a day to day basis around data?`)) %>% 
  pivot_longer(-Name, names_to = "question", values_to = "response") %>% 
  mutate(question = str_remove_all(question, "]"),
         question = str_remove_all(question, "\\[")) %>% 
  mutate(response = factor(response, levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree"), ordered = T))

ggplot(df, aes(x = response)) +
  geom_histogram(stat = "count", fill = "#50ABAB") +
  facet_wrap(~question,  labeller = label_wrap_gen(width = 40)) +
  mytheme +
  scale_x_discrete(drop = F) +
  labs(x = NULL,
       y = "number of responses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("wesley_presurvey.jpg", width = 8, height = 8, dpi = 600)


###--- DELTA PLOTS -----------------------------------------------
pre_df <- pre %>% 
  select(Name, 4:7) %>% 
  rename(name = 1, 
         `I feel comfortable using data to measure program outcomes` = 2,
         `If a new employee started today it would be easy for me to train them on how to enter data into databases at Wesley` = 3,
         `I feel confident in my ability to clearly document data related processes at Wesley` = 4,
         `It's easy to keep track of clients who are referred from one program to another within Wesley` = 5) %>% 
  mutate(time = "pre")

post_df <- post %>% 
  select(2, 3, 5, 6, 8) %>%
  rename(name = 1, 
       `I feel comfortable using data to measure program outcomes` = 2,
       `If a new employee started today it would be easy for me to train them on how to enter data into databases at Wesley` = 3,
       `I feel confident in my ability to clearly document data related processes at Wesley` = 4,
       `It's easy to keep track of clients who are referred from one program to another within Wesley` = 5) %>% 
  mutate(time = "post") %>% 
  rbind(pre_df) %>% 
  select(name, time, everything())
  
df2 <- post_df %>% 
  pivot_longer(cols = 3:6, 
               names_to = "question", values_to = "response") %>% 
  mutate(time = factor(time, 
                       levels = c("pre", "post"))) %>% 
  mutate(response = factor(response, 
                           levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree")))

pd <- ggstance::position_dodgev(0.3)
qs <- df2 %>% 
  ggplot(aes(x = time, y = response, color = name, group = name)) +
  geom_point(position = pd, size = 2) +
  geom_line(position = pd, size = 1.5) +
  facet_wrap(~question, labeller = label_wrap_gen()) +
  mytheme +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none") +
  scale_y_discrete(drop = F)
qs
