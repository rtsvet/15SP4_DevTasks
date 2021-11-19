

##############

SLE15SP5DevTasks <- 
  read.csv("/opt/Documents_Personal/Data_Analyse/15SP4_DevTasks/SLE15SP5DevTasks.csv",
           sep=";", stringsAsFactors=TRUE)

library(dplyr)

library(tidyr)

subset(SLE15SP5DevTasks,SLE15SP5DevTasks$Custom.field..Marketing.Need.!="NA")

names(SLE15SP5DevTasks)


Names <- c("Issue.key", "Issue.id", "Project.Manager.","Assignee","Status","Priority"
           ,"Marketing.Need","Team.Leader" ,"Worker"
           ,"Business.Impact",   "Impact",  "Summary"
           ,"Customer.Interest")
names(SLE15SP5DevTasks) <- Names

####
# Concatenating a Responsible column as sometimes the Team Leader is not filled in

levels(SLE15SP5DevTasks$Team.Leader)
levels(SLE15SP5DevTasks$Assignee)



SLE15SP5DevTasks$Responsible <- if_else( as.character(SLE15SP5DevTasks$Team.Leader) != "", 
                                         as.character(SLE15SP5DevTasks$Team.Leader),
                                         as.character(SLE15SP5DevTasks$Assignee)) 

View(SLE15SP5DevTasks[c("Assignee", "Team.Leader", "Responsible")])

SLE15SP5DevTasks$Responsible <- as.factor(SLE15SP5DevTasks$Responsible)



#####
# Making a barplot

Tasks <- table(SLE15SP5DevTasks$Responsible)



library(ggplot2)
library(forcats)

TaskNumber <- as.data.frame(Tasks)
names(TaskNumber) <- c("Team.Lead", "Tasks")
View(TaskNumber)

TaskNumber %>%
  mutate(Team.Lead = fct_reorder(Team.Lead, desc(Tasks))) %>%
  ggplot( aes(x= Team.Lead, y= Tasks)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("number of tasks") +
  theme_bw()



