library(readr)
Data_spreadsheet_Rformat_3_ <- read_csv("Data spreadsheet - Rformat (3).csv")
capdata <- Data_spreadsheet_Rformat_3_
head(capdata)
# importing and renaming the dataset

model1 <- glm(time ~ condition, data=capdata, family=gaussian(link='identity'))
summary(model1)

plot0 <- ggplot(model1, aes(x=condition, y=time)) +geom_boxplot()+coord_cartesian(ylim=c(0,15)) + ggtitle("Complete data GLM")
plot0
# this model compares time to condition without accounting the L# of participants

library(car)
model1.2 <- aov(time ~ condition *Lnum, data=capdata)
summary(model1.2)

library(ggplot2)
plotall <- ggplot(model1.2, aes(x=condition, y=time, fill=Lnum)) +geom_boxplot()+ coord_cartesian(ylim=c(0,15)) +ggtitle("Complete data AOV") +labs(fill= "Participant group")
plotall
# this is a two way anova model to account for time by condition by L2/L3

#when interpreting data think about discourse and how larger discourse chunks and meaning is built up through time, smaller chunks may mean being closer to the language 

# stat signifciant > or= 0.05, Pr is p value, which rejects null hypothessi (the opposite of what i expected) and support my REAL hypthesis 

library(readr)
rformat_L2 <- read_csv("rformat L2.csv")
L2data <- rformat_L2
head(L2data)
# import L2 dataset and rename

model2 <- glm(time ~ condition, data=L2data, family=gaussian(link='identity'))
summary(model2)

plotL2 <- ggplot(model2, aes(x=condition, y=time)) +geom_boxplot() + coord_cartesian(ylim=c(0,8)) + ggtitle("L2 time by condition GLM")
plotL2
# time by condition on a smaller graph

model2.2 <-glm(time ~ language, data=L2data, family=gaussian(link='identity'))
summary(model2.2)

plotL2.2 <- ggplot(model2.2, aes(x=language, y=time)) +geom_boxplot() + coord_cartesian(ylim=c(0,8))+ ggtitle("L2 time by langauge GLM")
plotL2.2
#glm time by language plotted

model2.3 <- aov(time ~ language *condition, data=L2data)
summary(model2.3)

plotL2.3 <- ggplot(model2.3, aes(x=language, y=time, fill=condition)) +geom_boxplot() + coord_cartesian(ylim=c(0,15)) +ggtitle("L2 data AOV")
plotL2.3
#L2 one-way anova and the boxplot

library(readr)
rformat_3 <- read_csv("rformat 3.csv")
L3data <- rformat_3
head(L3data)
#import L3 dataset and rename

model3 <- glm(time ~ condition, data=L3data, family=gaussian(link='identity'))
summary(model3)

plotL3 <- ggplot(model3, aes(x=condition, y=time)) +geom_boxplot()+ coord_cartesian(ylim=c(0,8)) + ggtitle("L3 time by condition GLM")
plotL3
# time by condition on a smaller graph

model3.2 <-glm(time ~ language, data=L3data, family=gaussian(link='identity'))
summary(model3.2)

plotL3.2 <- ggplot(model3.2, aes(x=language, y=time)) +geom_boxplot() + coord_cartesian(ylim=c(0,8)) +ggtitle("L3 time by language GLM")
plotL3.2
#glm time by language plotted

model3.3 <- aov(time ~ language *condition, data=L3data)
summary(model3.3)

plotL3.3 <- ggplot(model3.3, aes(x=language, y=time, fill=condition))+ geom_boxplot() + coord_cartesian(ylim=c(0,15)) + ggtitle("L3 data AOV")
plotL3.3

#L3 one-way anova and the boxplot

