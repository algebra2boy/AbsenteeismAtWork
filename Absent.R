# import libraries
library(tidyverse)
library(plotrix)

# read the file using absolute path
absent <- read.csv(file = "/Users/yongyetan/Desktop/Area51/AbsenteeismAtWork/Absenteeism_at_work.csv", sep = ";")

# checking whether there are missing / NA value in the dataset for each col
for (i in 1:ncol(absent)) {
    print(is.null(absent[, i]))
}
# double make sure that there is no NA value
summary(absent)


# Exploratory analysis on Absenteeism.time.in.hours and education
summary(absent$Absenteeism.time.in.hours)
# Min.    1st Qu.  Median    Mean   3rd Qu.    Max. 
# 0.000   2.000   3.000      6.924   8.000    120.000 

# tell us the count of people with various degrees
Education <- absent$Education
aggregate(data.frame(count = Education), list(value = Education), length)
#      value count
#  1     1   611 # high school 
#  2     2    46 # graduate
#  3     3    79 # post graduate
#  4     4     4 # master and doctor


# Figure 1
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
ggsave("age_workload.pdf")


# Figure 2
# exploring the relationship between the age and absent time
absent %>% 
  group_by(Age) %>% 
  summarise(average_absent = mean(Absenteeism.time.in.hours)) %>% 
  ggplot(aes(x = Age,  y = average_absent)) + 
  geom_point(aes(color = Age)) + 
  geom_smooth(se = FALSE, color = "green", method = 'loess', formula = 'y ~ x') + 
  # customize the x-tick from 27 to 57 with an interval of 3
  scale_x_continuous(breaks = seq(27, 57, by = 3)) + 
  # customize the y-tick from 0 to 33 with an interval of 5
  scale_y_continuous(breaks = seq(0, 33, by = 5)) + 
  labs(x = "age of worker",
       y = "average absent time per week in hours",
       title = "Average absent time vs age of worker") + 
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("absentTime_age.pdf")
  
  
# Figure 3
# making a graph about the number average absence across different workdays
absent %>% group_by(Day.of.the.week) %>% 
  summarise(total_absence = mean(Reason.for.absence)) %>% 
  # convert it to charater array because recode needs an character vector
  mutate(Day.of.the.week = as.character(Day.of.the.week)) %>% 
  # make the label more readable by converting numbers to text
  mutate(Day.of.the.week = fct_recode(
    Day.of.the.week, "Monday" = '2',
    "Tuesday" = '3', "Wednesday" = '4', 
    "Thursday" = '5', "Friday" = '6')) %>% 
  ggplot(aes(x = Day.of.the.week, y = total_absence)) + 
  geom_bar(stat = "identity") + 
  # adding labels 
  labs(x = "Day of a week",
       y = "the number of times of absence in average",
       title = "Average absence vs weekday",
       subtitle = "More people is absent on Friday") + 
  # center the title anmd subtitle
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  # flip the axis 
  coord_flip()
ggsave("absent_workday.pdf")


# Figure 4
# making a graph about the number average absence across different months
absent %>% group_by(Month.of.absence) %>% 
  summarise(Reason.for.absence = mean(Reason.for.absence)) %>% 
  # get rid of the first row because 0 month is meaningless
  slice(2:13) %>% 
  # convert it to charater array because recode needs an character vector
  mutate(Month.of.absence = as.character(Month.of.absence)) %>% 
  # refactor the months
  mutate(Month.of.absence = fct_recode(
    Month.of.absence, "January" = '1', "February" = '2',
    "March" = '3', "April" = '4',
    "May" = '5', "June" = '6',
    "July" = '7', 'August' = '8',
    "September" = '9', "October" = "10",
    "November" = "11", "December" = "12"
  )) %>% 
  # sort the absence time 
  ggplot(mapping = aes(Reason.for.absence, fct_reorder(Month.of.absence, Reason.for.absence, .fun = mean))) + 
  geom_point(colour = "red") + 
  labs(y = "Month of a year",
       x = "the number of times of absence in average",
       title = "Average absence vs month")  +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave("absent_month.pdf")


# grouping factor must have exactly 2 levels 
# this is where I perform my t-test to answer 
# Does bad habits lead to not showing up to work?
t.test(Absenteeism.time.in.hours~Social.smoker, data = absent)
t.test(Absenteeism.time.in.hours~Social.drinker, data = absent)
t.test(Absenteeism.time.in.hours~Disciplinary.failure, data = absent)

# label the distance with more meaningful labels
# explore the relationship between distance and average absent time
# Figure 5
refactor_absent <- absent %>% 
  mutate(Distance.from.Residence.to.Work =
           case_when( (Distance.from.Residence.to.Work>0) & (Distance.from.Residence.to.Work <= 10) ~ "0~10", 
                      (Distance.from.Residence.to.Work>10) & (Distance.from.Residence.to.Work <= 20) ~ "10~20", 
                     (Distance.from.Residence.to.Work>20) & (Distance.from.Residence.to.Work <= 30) ~ "20~30",
                    (Distance.from.Residence.to.Work>30) & (Distance.from.Residence.to.Work <= 40) ~ "30~40",
                    (Distance.from.Residence.to.Work>40 & Distance.from.Residence.to.Work<=50)  ~ "40~50",
                    Distance.from.Residence.to.Work> 50 ~ "50+")) %>% 
  group_by(Distance.from.Residence.to.Work) %>% 
  summarise(average_absent = mean(Absenteeism.time.in.hours))

Distance_label <- refactor_absent$Distance.from.Residence.to.Work
Distance_average <- refactor_absent$average_absent

# find the percentage of the average distance
percentage = round(Distance_average/sum(Distance_average) * 100)
new_labels = paste(Distance_label, "-", percentage, "%", sep="")

# creating the pie chart 
pie(Distance_average, labels = new_labels, 
    # the title of the graph
    main = "distance from residence to work in kilometer", 
    # adding 6 rainbow colors 
    col = rainbow(6),
    angle = 30, 
    # the size of circle
    radius = 0.9,
    lwd = 2)

# 3D pie-chart, not used in this paper 
pie3D(Distance_average, labels = new_labels, main = "Distance from residence to work in kilometer", col = rainbow(6), explode = 0.1)

