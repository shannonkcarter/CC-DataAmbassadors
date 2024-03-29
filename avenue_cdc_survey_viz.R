library(tidyverse)
library(googlesheets4)

mytheme <- theme_bw(base_size = 15, base_family = "Franklin Gothic Medium") +
  theme(panel.grid = element_blank())

pre_raw <- read_sheet("https://docs.google.com/spreadsheets/d/18BxeR4tJWUvjLsJx_lhczdXEi6JZNJypCAXSXKpfzH4/edit")
post_raw <- read_sheet("https://docs.google.com/spreadsheets/d/18NY2veSPz4PuTROdeUINN9muHz1PToAG2N-lk1ROBfY/edit?resourcekey#gid=169876996")

###--- PRE SURVEY HISTOGRAM ----------------------------------------
pre <- pre_raw %>% 
  select(`Name`:`[I feel comfortable talking to my colleagues about data.]`) %>% 
  pivot_longer(-Name, names_to = "question", values_to = "response") %>% 
  mutate(question = str_remove_all(question, "]"),
         question = str_remove_all(question, "\\[")) %>%
  mutate(response = factor(response, levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree"), ordered = T))

ggplot(data = pre, aes(x = response)) +
  geom_histogram(stat = "count", fill = "#50ABAB") +
  facet_wrap(~question,  labeller = label_wrap_gen(width = 60)) +
  mytheme +
  scale_x_discrete(drop = F) +
  labs(x = NULL,
       y = "number of responses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(color="white", fill="white"),
        strip.text = element_text(size = 12, color = "black", hjust = 0)
  )
ggsave("avenue_cdc_presurvey.jpg", width = 9.5, height = 8, dpi = 600)

# ###--- POST SURVEY HISTOGRAM -------------------------------------
post <- post_raw %>%
  select(`Name`:`[I feel comfortable talking to my colleagues about data.]`) %>%
  pivot_longer(-Name, names_to = "question", values_to = "response") %>%
  mutate(question = str_remove_all(question, "]"),
         question = str_remove_all(question, "\\[")) %>%
  mutate(response = factor(response, levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree"), ordered = T))

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
  select(`Name`:`[I feel comfortable talking to my colleagues about data.]`) %>% 
  pivot_longer(-Name, names_to = "question", values_to = "response") %>%
  mutate(question = str_remove_all(question, "]"),
         question = str_remove_all(question, "\\[")) %>%
  pivot_wider(names_from = "question", values_from = "response") %>% 
  mutate(time = "pre")

post_df <- post_raw %>%
  select(`Name`:`[I feel comfortable talking to my colleagues about data.]`) %>% 
  pivot_longer(-Name, names_to = "question", values_to = "response") %>%
  mutate(question = str_remove_all(question, "]"),
         question = str_remove_all(question, "\\[")) %>%
  pivot_wider(names_from = "question", values_from = "response") %>% 
  mutate(time = "post") %>% 
  rbind(pre_df) %>%
  select(Name, time, everything()) %>%
  # some people reported their names differently at pre/post. this selects just first name
  mutate(Name = sub(" .*", "", Name)) %>%
  # remove people who only took one of the 2 surveys
  filter(Name != "Berenice")

pre_post <- post_df %>%
  pivot_longer(cols = 3:6,
               names_to = "question", values_to = "response") %>%
  mutate(time = factor(time,
                       levels = c("pre", "post"))) %>%
  mutate(response = factor(response,
                           levels = c("Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree", "Agree"))) #%>%
# filter(!question %in% c("I feel comfortable using data to measure program outcomes",
#                         "If a new employee started today it would be easy for me to train them on how to enter data into databases at Wesley"))

pre_hist <- pre_post %>%
  filter(time == "pre") %>%
  ggplot(aes(x = response)) +
  geom_histogram(stat = "count", fill = "#50ABAB") +
  facet_wrap(~question,  labeller = label_wrap_gen(width = 45)) +
  mytheme +
  scale_x_discrete(drop = F) +
  labs(x = NULL,
       y = "number of responses",
       title = "Prior to this program...") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(color="white", fill="white"),
        strip.text = element_text(size = 12, color = "black", hjust = 0)
  )
pre_hist
ggsave("wesley_presurvey.jpg", width = 8, height = 8, dpi = 600)


post_hist <- pre_post %>%
  filter(time == "post") %>%
  ggplot(aes(x = response)) +
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
post_hist
ggsave("wesley_postsurvey.jpg", width = 8, height = 8, dpi = 600)

pd <- ggstance::position_dodgev(0.3)
qs <- pre_post %>%
  ggplot(aes(x = time, y = response, color = Name, group = Name)) +
  geom_point(position = pd, size = 2) +
  geom_line(position = pd, size = 1.5) +
  facet_wrap(~question, labeller = label_wrap_gen(width = 40)) +
  labs(x = NULL, y = NULL) +
  mytheme +
  theme(legend.position = "none",
        strip.background = element_rect(color="white", fill="white"),
        strip.text = element_text(size = 12, color = "black", hjust = 0),
        axis.text = element_text(size = 12)
  ) +
  scale_y_discrete(drop = F)
qs
ggsave("avenuecdc_pre_post_survey.jpg", width = 9, height = 6, dpi = 600)
