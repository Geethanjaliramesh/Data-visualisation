library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
install.packages("gridExtra")
library(gridExtra)

install.packages("gridGraphics")
library(gridGraphics)
install.packages("ggplotify")
library(ggplotify)
install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization
library(ggplot2)


mydata <- read.csv("C:/Users/HP/Documents/Geethu/project data.csv")

mydata$M.F <- ifelse(mydata$M.F == "M",1,0)
mydata <- mydata[mydata$Group != "Converted",]
mydata <- na.omit(mydata)
str(mydata)
mydata$Group <- as.factor(mydata$Group)

summary_table <- summary(mydata)
head(mydata)
#write.table(summary_table, "summary_table.txt", sep="\t", col.names=FALSE)
#pairs(mydata)
is.na(mydata)

graph1<-boxplot(mydata$Age ~ mydata$Group, xlab = "Group", ylab = "Age")
counts <- table(mydata$Group)

graph2<-barplot(counts, main = "Demented vs Non-Demented", xlab = "Dementia Status", ylab = "Count", col = c("red", "blue"), legend = TRUE)
graph3<-plot(mydata$MMSE,mydata$Age)

g1 <- grob(graph1)
g2 <- grob(graph2)
g3 <- grob(graph3)

grid.arrange(g1, g2, g3, nrow = 1)

ggplot(mydata, aes(x = mydata$Age, fill = mydata$Group)) +
  geom_density(alpha = 0.7) +
  labs(title = "Age Distribution: Demented vs Non-Demented", x = "Age", y = "Density") +
  scale_fill_manual(values = c("red", "blue"))


#for creating distance matrix for mmse and cdr

distance_data1 <- mydata[,6:7]
distance <- get_dist(distance_data1)

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
is.na(mydata[,10])
data1 <- scale(mydata)
data_cluster <- mydata[,6:10]
data2<- scale(data_cluster)
set.seed(123)

optimal_clusters <- fviz_nbclust(data2, kmeans, method = "wss", k.max = 10)
optimal_clusters + geom_vline(xintercept = 3, linetype = 2)

summary(optimal_clusters)
k1 <- kmeans(data2, centers = 3, nstart = 25)
fviz_cluster(k1, data = data2)
centers <- k1$centers
str(k1)
k1$withinss
k1$totss
k1$tot.withinss

logit_model <- glm(mydata$Group ~ mydata$Age + mydata$M.F + mydata$MMSE + mydata$CDR, data = mydata, family = "binomial")
summary(logit_model)
contrasts(mydata$Group)



library(MASS) 
feature_backwards_model <- stepAIC(logit_model, direction = "backward")
summary(feature_backwards_model)
