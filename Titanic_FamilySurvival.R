# Did being in a family affect passenger survival?

titan <- read.csv("titanic.csv")

titan$Survived <- as.factor(titan$Survived)
levels(titan$Survived) <- c("Dead","Alive")

#levels(titan$Embarked)
#levels(titan$Embarked) <- c("Unknown","Cherbourg","Queenstown","Southhampton")

titanMen <- titan[titan$Sex=="male",]
titanWomen <- titan[titan$Sex=="female",]

singlemen <- titanMen[titanMen$SibSp==0 & titanMen$Parch==0,]
familymen <- titanMen[titanMen$SibSp!=0 | titanMen$Parch!=0,]

singlewomen <- titanWomen[titanWomen$SibSp==0 & titanWomen$Parch==0,]
familywomen <- titanWomen[titanWomen$SibSp!=0 | titanWomen$Parch!=0,]

par(mfrow=c(2,2))
pie(table(familymen$Survived), labels=c("Dead","Survived"), main ="Family Men Passenger Survival")
pie(table(singlemen$Survived), labels=c("Dead","Survived"), main ="Single Men Passenger Survival")
pie(table(familywomen$Survived), labels=c("Dead","Survived"), main ="Family Women Passenger Survival")
pie(table(singlewomen$Survived), labels=c("Dead","Survived"), main ="Single Women Passenger Survival")
