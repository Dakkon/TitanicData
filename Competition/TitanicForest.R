
#install packages
install.packages('randomForest')
install.packages('tm')
install.packages('party')
install.packages('stringr')
install.packages('dplyr')
install.packages('mice')
install.packages('ggthemes')
#Load libraries of associated packages
library(randomForest) 
library(tm) 
library(party) 
library(stringr) 
library(rpart)
library('dplyr') # data manipulation
library('mice') # imputation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization

#Read in Titanic Datasets
train.titanic <- read.csv("./Competition/train.csv")
test.titanic <- read.csv("./Competition/test.csv")

#Add the Survived Column to the test data
test.titanic$Survived <- NA

#Add Survived Column to prediction frame
prediction.titanic <- data.frame(PassengerId = test.titanic$PassengerId, Survived = rep(0, 418))

#create full dataset in order to use for alterations/analytics
full <- rbind(train.titanic, test.titanic)

#Make Passenger Class a Categorical Value
full$Pclass <- factor(full$Pclass)


#Fill in missing values:
##Missing Embarkment
full$Embarked[which(full$Embarked == '' || full$Embarked == ' ')]
## passengers 62 and 830 are missing   
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[2]][1], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[2]][1], '</b>. So from where did they embark?'))
# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()  

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

#Show missing fairs
full[which(full$Fare == 0),c(1,3,9:12)]

# Replace missing fare value with median fare for class/embarkment
full$Fare[which(full$Fare == 0 & full$Pclass =='1' & full$Embarked == 'S')] <- median(full[full$Pclass == '1' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full$Fare[which(full$Fare == 0 & full$Pclass =='2' & full$Embarked == 'S')] <- median(full[full$Pclass == '2' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full$Fare[which(full$Fare == 0 & full$Pclass =='3' & full$Embarked == 'S')] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

#Extract FamilySize Feature
full$FamilySize <- full$Parch + full$SibSp + 1

#Extract Cabin Prefix
full$Cabin <- as.character(full$Cabin) 
full$CabinType <- sapply(full$Cabin, FUN = function(x) {substring(x, 1, 1)}) 
full$CabinType[full$CabinType==""] <- "NA" 
full$CabinType <- factor(full$CabinType)

#Extract Whether Passenger has Cabin Feature
full$HasCabin[full$CabinType=="NA"] <- 0L 
full$HasCabin[full$CabinType!="NA"] <- 1L 
#full$HasCabin <- factor(full$HasCabin)  
full$HasCabin <- as.logical(full$HasCabin)


#Establish Individual Fare Feature
full$FaresOnTicket <- sapply(1:length(full$Ticket), function(x)sum(full$Ticket[1:length(full$Ticket)] == full$Ticket[x])) 
full$IndividualFare <- full$Fare / full$FaresOnTicket
full$FaresOnTicket <- factor(full$FaresOnTicket)

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)
full$NameChar <- as.character(full$Name)

# Finally, grab surname from passenger name

#Add Features for Family Surname and Size (Seeks to calculate family associations)
#regex matching for everything that happens before the 1st ,
full$Surname <- sapply(full$NameChar, FUN=function(x) {strsplit(x, split = '[,.]')[[1]][1]})
#regex split on ( and ) then pulling the 2nd set of data (if maidenname then should get)
full$MaidenName <- sapply(full$NameChar, FUN=function(x) {strsplit(x, split = '[()]')[[1]][2]}) 

#set all people w/ a non-na maidenname to married
full$IsMarried[!is.na(full$MaidenName)] <- 1 
#set all men to single
full$IsMarried[full$Sex == "male"] <- 0 
#set all men over 18 with a sibling or spouse to be considered married
full$IsMarried[full$Sex == "male" && full$Age >= 18 && full$SibSp >= 1] <- 1 
#set all people who's familysize (sibsp + parch + 1) is 1 to not be married
full$IsMarried[full$FamilySize == 1] <- 0

cat(paste('We have', nlevels(factor(full$Surname)), 'unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))


# Do i care if someone has a ticket prifix?
# y <- as.data.frame(str_split_fixed(as.character(full$Ticket), " ", 2))
# full$TicketPrefix <- y[,1]
# full$TicketPrefix <- as.character(full$TicketPrefix)
# full$TicketPrefix[which(y[2] == "")] = "-"
# full$TicketPrefix <- factor(full$TicketPrefix)

#full$TicketPrefix <- unique(na.omit(unlist(strsplit(unlist(as.character(full$Ticket)), "[^a-zA-Z]+"))))

# Create a family variable 
full$Family <- paste(full$Surname, full$FamilySize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
# ggplot(full[1:891,], aes(x = FamilySize, fill = factor(Survived))) +
#   geom_bar(stat='count', position='dodge') +
#   scale_x_continuous(breaks=c(1:11)) +
#   labs(x = 'Family Size') +
#   theme_few()

# Discretize family size
full$FsizeD[full$FamilySize == 1] <- 'singleton'
full$FsizeD[full$FamilySize < 5 & full$FamilySize > 1] <- 'small'
full$FsizeD[full$FamilySize > 4] <- 'large'


# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
  full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
  
  
  
  titanic.rf.model <- randomForest(Survived ~ ., data=train.titanic);

#Predict Age based upon the other factors
#Don't use this until you understand it...
##Dak##
dectree.ages <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare + IsMarried, data = full[!is.na(full$Age), ], method = "anova")
dectree.ages <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = full[!is.na(full$Age), ], method = "anova")
set.seed(550)

full.imputed <- rfImpute(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare, train.titanic)
full.imputed <- rfImpute(Age ~ ., train.titanic)

full.ageimputed <- na.omit(full[which(colnames(full) == 'Age')])
full.ageimputed <- full[!is.na(full$Age), ]

rf.fit.age <- randomForest(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare, data = full.ageimputed, importance=TRUE, ntree = 3000)
prediction.age <- predict(rf.fit.age, full)
prediction.age[!is.na(full$Age)] <- full$Age[!is.na(full$Age)]

full$Age <- prediction.age
full$Age[is.na(full$Age)] <- prediction.age[is.na(full$Age), ]

submission.titanic <- data.frame(PassengerId = test.titanic$PassengerId, Survived = prediction)

#Copy predicted ages back into full dataset
full$Age[is.na(full$Age)] <- predict(dectree.ages, full[is.na(full$Age), ])
train.titanic <- full[1:891,] test.titanic <- full[892:1309,]

#Set Control Variables
dectree.control = rpart.control(minsplit = 100, minbucket = 1, cp = 0, maxdepth = 30)

#Measure effectiveness of Model Using Tree Approach on Training Model
dectree.titanic <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare, data = train.titanic, method = "class", control = dectree.control)
dectree.titanic <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = train.titanic, method = "class", control = dectree.control)

#Create Prediction Submission based on model and test dataset
submission.titanic <- data.frame(PassengerId = test.titanic$PassengerId, Survived = predict(dectree.titanic, newdata = test.titanic, type = "class", dectree.control))
Survived = predict(dectree.titanic, newdata = test.titanic, type = "class", control = dectree.control)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare, data = train.titanic, importance=TRUE, ntree = 3000)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare + FamilyID + TicketPrefix + IsMarried, data = train.titanic, controls = cforest_unbiased(ntree=25000, mtry=4)) prediction<- predict(fit, test.titanic, OOB=TRUE, type = "response")

prediction<- predict(fit, test.titanic)
submission.titanic <- data.frame(PassengerId = test.titanic$PassengerId, Survived = prediction)

PassengerId = test.titanic$PassengerId
submission.titanic <- data.frame(PassengerId, Survived)
write.csv(submission.titanic, file = "../Competition/Submission1.csv", row.names = FALSE)
