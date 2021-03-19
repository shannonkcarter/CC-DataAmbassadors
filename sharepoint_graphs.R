###=== DATA AMBASSADORS: SHAREPOINT PROGRESS ======================

###--- PREP WORKSPACE ---------------------------------------------

## Set working directory and clear memory
setwd("/Users/shannoncarter/Documents/JanuaryAdvisors/ccda")
rm(list = ls(all = T))

library(tidyverse)

mytheme <- theme_bw(base_size = 15, base_family = "Franklin Gothic Medium") +
  theme(panel.grid = element_blank())

###--- BY PROGRAM ------------------------------------------------

df <- read.csv("sharepoint_progress.csv", header = T) %>% 
  filter(program != "Total")

# Glossary
ggplot(df, aes(x = program, y = glossary, fill = time)) + 
  geom_bar(stat = "identity", position = "stack") +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("tomato", "gray22")) +
  labs(x = NULL,
       fill = NULL,
       y = "number of glossary terms",
       title = "Number of glossary terms added by each program",
       subtitle = "Items in red were added during lab time on May 26")

# Procedure
ggplot(df, aes(x = program, y = procedure, fill = time)) + 
  geom_bar(stat = "identity", position = "stack") +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("tomato", "gray22")) +
  labs(x = NULL,
       fill = NULL,
       y = "number of procedures",
       title = "Number of procedures added by each program",
       subtitle = "Items in red were added during lab time on May 26")

# Index
ggplot(df, aes(x = program, y = index, fill = time)) + 
  geom_bar(stat = "identity", position = "stack") +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("tomato", "gray22")) +
  labs(x = NULL,
       fill = NULL,
       y = "number of index items",
       title = "Number of index items added by each program",
       subtitle = "Items in red were added during lab time on May 26")

###--- CATHOLIC CHARITIES TOTALS  --------------------------------------
df1 <- read.csv("sharepoint_progress2.csv", header = T) %>% 
  filter(program == "Total") %>% 
  select(-program) %>% 
  rename("data index" = data.index) %>% 
  pivot_longer(c(glossary, procedure, "data index")) %>% 
  mutate(name = factor(name, levels = c("glossary", "procedure", 'data index')))
  

ggplot(df1, aes(x = name, y = value, fill = name)) + 
  mytheme +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 14),
        legend.position = "none") +
  #scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) +
  labs(x = NULL,
       fill = NULL,
       y = "number of items in Sharepoint",
       title = "Number of items added to Sharepoint",
       subtitle = "All Catholic Charities programs")


###--- BY PROGRAM ------------------------------------------------

df2 <- read.csv("sharepoint_progress2.csv", header = T) %>% 
  filter(program != "Total") %>% 
  rename("data index" = data.index) %>% 
  pivot_longer(c(glossary, procedure, "data index"))%>% 
  mutate(name = factor(name, levels = c("glossary", "procedure", 'data index')))

# Glossary
ggplot(df2, aes(x = program, y = value, fill = name)) + 
  mytheme +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  #scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) +
  labs(x = NULL,
       fill = NULL,
       y = "number of items",
       title = "Number of items added to Sharepoint by each program") + 
  facet_grid(name~.)
