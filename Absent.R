library(tidyverse)


absent <- read.csv(file = "/Users/yongyetan/Desktop/Area51/AbsenteeismAtWork/Absenteeism_at_work.csv", sep = ";")

absent %>% 
  group_by(Age) %>% 
  summarise(average = mean(Work.load.Average.day)) %>% 
  ggplot(aes(x = Age, y =  average)) +
  geom_point(aes(color = Age)) +
  geom_smooth(se = FALSE, color = "green", method = 'loess', formula = 'y ~ x') +
  scale_x_continuous(breaks = seq(27, 57, by = 5)) +
  labs(x = "age of worker",
       y = "average amount of workload in a day",
       title = "How much workload a worker receives as he/she ages",
       subtitle = "the older, the fewer work?") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 42, color = "red", linetype = "dotted", size = 0.6)
