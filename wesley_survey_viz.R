
library(tidyverse)
library(googlesheets4)

mytheme <- theme_bw(base_size = 15, base_family = "Franklin Gothic Medium") +
  theme(panel.grid = element_blank())

raw_df <- read_sheet("https://docs.google.com/spreadsheets/d/1HMe_zjXTHrhgKWQmx_r2s9WLx-36MpdSRXW7NCBBPUw/edit#gid=1402504997")

df <- raw_df %>% 
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

