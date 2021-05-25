library(tidyverse)
library(googlesheets4)

mytheme <- theme_bw(base_size = 15, base_family = "Franklin Gothic Medium") +
  theme(panel.grid = element_blank())

pre_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1HMe_zjXTHrhgKWQmx_r2s9WLx-36MpdSRXW7NCBBPUw/edit#gid=1402504997")
post_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1WeBHfjruh99MxddPPmgE2roPruvjGOuR7-4N4b35Pfc/edit?resourcekey#gid=1474246294")

###--- PRE SURVEY HISTOGRAM ----------------------------------------
pre <- pre_raw %>% 
  select(-c("Timestamp", "Email Address", `What are some of the top challenges you face on a day to day basis around data?`)) %>% 
  pivot_longer(-Name, names_to = "question", values_to = "response") %>% 
  mutate(question = str_remove_all(question, "]"),
         question = str_remove_all(question, "\\[")) %>%
  mutate(response = factor(response, levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree"), ordered = T))

ggplot(data = pre, aes(x = response)) +
  geom_histogram(stat = "count", fill = "#50ABAB") +
  facet_wrap(~question,  labeller = label_wrap_gen(width = 50)) +
  mytheme +
  scale_x_discrete(drop = F) +
  labs(x = NULL,
       y = "number of responses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(color="white", fill="white"),
        strip.text = element_text(size = 12, color = "black", hjust = 0)
        )
ggsave("wesley_presurvey.jpg", width = 8, height = 8, dpi = 600)

###--- POST SURVEY HISTOGRAM -------------------------------------
post <- post_raw %>% 
  select(2:9) %>% 
  pivot_longer(-Name, names_to = "question", values_to = "response") %>% 
  mutate(question = str_remove_all(question, "]"),
         question = str_remove_all(question, "\\["),
         question = str_remove_all(question, "Compared with how I felt at the beginning of this program... ")) %>% 
  mutate(response = factor(response, levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree"), ordered = T)) %>% 
  filter(question != "Overall, this program was valuable to me.")

ggplot(post, aes(x = response)) +
  geom_histogram(stat = "count", fill = "#50ABAB") +
  facet_wrap(~question,  labeller = label_wrap_gen(width = 45)) +
  mytheme +
  scale_x_discrete(drop = F) +
  labs(x = NULL,
       y = "number of responses",
       title = "Compared with how I felt at the beginning of this program...") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(color="white", fill="white"),
        strip.text = element_text(size = 12, color = "black", hjust = 0)
  )
ggsave("wesley_postsurvey.jpg", width = 11, height = 8, dpi = 600)

###--- DELTA PLOTS -----------------------------------------------
pre_df <- pre_raw %>% 
  select(Name, 4:7) %>% 
  rename(name = 1, 
         `I feel comfortable using data to measure program outcomes` = 2,
         `If a new employee started today it would be easy for me to train them on how to enter data into databases at Wesley` = 3,
         `Documenting procedures` = 4,
         `Tracking referrals` = 5) %>% 
  mutate(time = "pre")

post_df <- post_raw %>% 
  select(2, 3, 5, 6, 8) %>%
  rename(name = 1, 
       `I feel comfortable using data to measure program outcomes` = 2,
       `If a new employee started today it would be easy for me to train them on how to enter data into databases at Wesley` = 3,
       `Documenting procedures` = 4,
       `Tracking referrals` = 5) %>% 
  mutate(time = "post") %>% 
  rbind(pre_df) %>% 
  select(name, time, everything()) %>% 
  # some people reported their names differently at pre/post. this selects just first name
  mutate(name = sub(" .*", "", name)) %>% 
  # remove people who only took one of the 2 surveys
  filter(name != "David") 

pre_post <- post_df %>% 
  pivot_longer(cols = 3:6, 
               names_to = "question", values_to = "response") %>% 
  mutate(time = factor(time, 
                       levels = c("pre", "post"))) %>% 
  mutate(response = factor(response, 
                           levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree"))) %>% 
  filter(!question %in% c("I feel comfortable using data to measure program outcomes",
                          "If a new employee started today it would be easy for me to train them on how to enter data into databases at Wesley"))


pd <- ggstance::position_dodgev(0.3)
qs <- pre_post %>% 
  ggplot(aes(x = time, y = response, color = name, group = name)) +
  geom_point(position = pd, size = 2) +
  geom_line(position = pd, size = 1.5) +
  facet_wrap(~question, labeller = label_wrap_gen(width = 45)) +
  labs(x = NULL, y = NULL) +
  mytheme +
  theme(legend.position = "none",
    strip.background = element_rect(color="white", fill="white"),
    strip.text = element_text(size = 12, color = "black", hjust = 0),
    axis.text = element_text(size = 12)
  ) +
  scale_y_discrete(drop = F)
qs
ggsave("wesley_pre_post_survey_2qs.jpg", width = 9, height = 4, dpi = 600)
