###
# Late features 15 SP4
############

LateFeatures <- read.csv("./SLE15SP4 Late accepted features.csv", stringsAsFactors=TRUE)
View(SLE15SP4.Late.accepted.features)



library(ggplot2)
library(ggQC)

LF_PM <- as.data.frame(table(LateFeatures$Product.Manager))
names(LF_PM) <- c("PM", "Features")
View(LF_PM)

ggplot(LF_PM, aes(x = PM, y = Features)) +
  geom_bar(stat="identity", alpha=0) +
  stat_pareto(point.color = "red",
              point.size = 1,
              line.color = "black", 
              ) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) 
