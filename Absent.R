library(tidyverse)

# read the file using absolute path
absent <- read.csv(file = "/Users/yongyetan/Desktop/Area51/AbsenteeismAtWork/Absenteeism_at_work.csv", sep = ";")

# checking whether there are missing / NA value in the dataset for each col
for (i in 1:ncol(absent)) {
    print(is.null(absent[, i]))
}
# double make sure that there is no NA value
summary(absent)



# exploring the relationship between the age and amount of the work in a day
absent %>% 
  # group the age group
  group_by(Age) %>% 
  # calculate the the average amount of work of the age group
  summarise(average = mean(Work.load.Average.day)) %>% 
  # display the graph with age as x-axis, and average as y-axis
  ggplot(aes(x = Age, y =  average)) +
  # assign points with colors
  geom_point(aes(color = Age)) +
  # making line-of-best-fit with green color
  geom_smooth(se = FALSE, color = "green", method = 'loess', formula = 'y ~ x') +
  # customize the x tick starting from 27 to 57 with an interval of 5
  scale_x_continuous(breaks = seq(27, 57, by = 5)) +
  # adding labels to x,y axis, title, and subtitle
  labs(x = "age of worker",
       y = "average amount of workload in a day",
       title = "How much workload a worker receives as he/she ages",
       subtitle = "the older, the fewer work?") +
  # center the title 
  theme(plot.title = element_text(hjust = 0.5)) + 
  # center the subtitle
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  # add a verticle line at age 42 with customized features 
  geom_vline(xintercept = 42, color = "red", linetype = "dotted", size = 0.6)
