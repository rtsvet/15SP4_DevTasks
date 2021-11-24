#
# 
##############

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

#SLE15SP5DevTasks <- 
#  read.csv("/opt/Documents_Personal/Data_Analyse/15SP4_DevTasks/SLE15SP5DevTasks.csv",
#           sep=";", stringsAsFactors=TRUE)

SLE15SP4DevTasks <- read.csv("/opt/Documents_Personal/Data_Analyse/15SP4_DevTasks/15SP4_CompleteInfo.csv",
                                 na.strings="#N/A",
                                 stringsAsFactors=TRUE)
View(SLE15SP4DevTasks)






#names(SLE15SP5DevTasks)
#Names <- c("Issue.key", "Issue.id", "Project.Manager.","Assignee","Status","Priority"
#           ,"Marketing.Need","Team.Leader" ,"Worker"
#           ,"Business.Impact",   "Impact",  "Summary"
#           ,"Customer.Interest")
#names(SLE15SP5DevTasks) <- Names

####
# Concatenating a Responsible column as sometimes the Team Leader is not filled in

levels(SLE15SP4DevTasks$Team.Leader)
levels(SLE15SP4DevTasks$Assignee)

SLE15SP4DevTasks$Responsible <- if_else( as.character(SLE15SP4DevTasks$Team.Leader) != "", 
                                         as.character(SLE15SP4DevTasks$Team.Leader),
                                         as.character(SLE15SP4DevTasks$Assignee)) 
SLE15SP4DevTasks$Responsible <- as.factor(SLE15SP4DevTasks$Responsible)
View(SLE15SP4DevTasks[c("Assignee", "Team.Leader", "Responsible")])



#####
# Making a barplot

Tasks <- table(SLE15SP4DevTasks$Responsible)

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

###
# Calculating the importance
####

SLE15SP4DevTasks$Business.Impact <- factor(SLE15SP4DevTasks$Business.Impact, 
                                           ordered = TRUE, 
                                           levels = c( "Low", "Medium", "High") )

SLE15SP4DevTasks$Customer.Interest <- factor(SLE15SP4DevTasks$Customer.Interest, 
                                             ordered = TRUE, 
                                             levels = c( "Low", "Medium", "High") )
### Small Test
SLE15SP4DevTasks$Customer.Interest[14]
as.numeric(SLE15SP4DevTasks$Customer.Interest[14])

SLE15SP4DevTasks$Marketing.Need <- factor(SLE15SP4DevTasks$Marketing.Need, 
                                             ordered = TRUE, 
                                             levels = c( "Low", "Medium", "High") )

levels(SLE15SP4DevTasks$Priority)

SLE15SP4DevTasks$Priority <- factor(SLE15SP4DevTasks$Priority, 
                                          ordered = TRUE, 
                                          levels = c( "Wish to Have","Could Have","Should Have","Must Have") )
### Small Test
SLE15SP4DevTasks$Priority
as.numeric(SLE15SP4DevTasks$Priority[1])

SLE15SP4DevTasks$Value <- 
  as.numeric(SLE15SP4DevTasks$Priority) +
  as.numeric(SLE15SP4DevTasks$Business.Impact) +
  as.numeric(SLE15SP4DevTasks$Customer.Interest) +
  as.numeric(SLE15SP4DevTasks$Marketing.Need)

SLE15SP4DevTasks[, SLE15SP4DevTasks$Responsible == "ptesarik"]

SLE15SP4DevTasks %>% filter((SLE15SP4DevTasks$Responsible == "ptesarik") & (!is.na(SLE15SP4DevTasks$Value)) )

SLE15SP4DevTasks %>% filter((SLE15SP4DevTasks$Responsible == "kstreitova") & (!is.na(SLE15SP4DevTasks$Value)) )

#
# subset only important columns

dt_kstreitova <- SLE15SP4DevTasks %>% 
  filter((SLE15SP4DevTasks$Responsible == "kstreitova") & (!is.na(SLE15SP4DevTasks$Value)) ) %>% 
  select(Issue.key, Value )
View(dt_kstreitova)

# Pareto chart

library(qicharts2)

kr_ordered <- dt_kstreitova[order(dt_kstreitova$Value),]
kr_ordered$Issue <- factor(kr_ordered$Issue.key)
View(kr_ordered)
kr_ordered$Issue.key <- NULL
names(kr_ordered)

x <- c(kr_ordered$Value)
names(x) <- c(kr_ordered$Issue)
paretochart(x)

View(x)

dt_kstreitova %>%
  mutate(Issue.key = fct_reorder(Issue.key, desc(Value))) 

k_team <- as.data.frame(table(dt_kstreitova$Value))

names(k_team) <- c("Importance", "Freq")

View(k_team)

k_team$Load <- k_team$Importance*k_team$Freq

xy <- k_team %>% select(Importance)

paretochart(k_team)
View(dt_kstreitova)

example(paretochart)

