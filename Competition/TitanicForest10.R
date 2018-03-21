#install packages
install.packages('randomForest')
install.packages('tm')
install.packages('party')
install.packages('stringr')
install.packages('dplyr')
install.packages('mice')
install.packages('ggthemes')

#Attempt ##????
attempt <- 11L
# On attempt 2 I know that the 0 price passangers all but one die -> First leaving their price at 0
# improved .005 accuracy 
# on attempt 3 I realized via histogram that age could be more usefully bucketed to <10/10-60/>60
# on attempt 4 i changed the age back to leave it as age and not bucket it (is this a me problem?)
# on attempt 4 i cut the mother column 
# on attempt 5 I cut ageType
# on attempt 6 i put the ticket price back to the full prices 
# on attempt 7 i'm going to make a "Save the women and children" model -> 
##replacing sex (because I don't think ) sex matters if you're a kid so it averages more out than i want.
# on attempt 8 i got rid of sex and age ( i think this will be worse)
# WaC is good, but not additive yet. Still need to keep age (probably because it's good at people over 16)
# on attempt 9 I added age back. got rid of embarked. 
# on attempt 10 I added embarked and a few other smaller things back in (mother, embarked)
# on attempt 11 removed mother (again), and turned on importance

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
#full[which(full$Fare == 0 | is.na(full$Fare)),c(1,3,9:12)]

# Replace missing fare value with median fare for class/embarkment
# On attempt 2 I change this to just have na's
# full$Fare[which((full$Fare == 0 | is.na(full$Fare)) & full$Pclass =='1' & full$Embarked == 'S')] <- median(full[full$Pclass == '1' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare)) & full$Pclass =='2' & full$Embarked == 'S')] <- median(full[full$Pclass == '2' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare)) & full$Pclass =='3' & full$Embarked == 'S')] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare)) & full$Pclass =='1' & full$Embarked == 'Q')] <- median(full[full$Pclass == '1' & full$Embarked == 'Q', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare)) & full$Pclass =='2' & full$Embarked == 'Q')] <- median(full[full$Pclass == '2' & full$Embarked == 'Q', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare))& full$Pclass =='3' & full$Embarked == 'Q')] <- median(full[full$Pclass == '3' & full$Embarked == 'Q', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare)) & full$Pclass =='1' & full$Embarked == 'C')] <- median(full[full$Pclass == '1' & full$Embarked == 'C', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare)) & full$Pclass =='2' & full$Embarked == 'C')] <- median(full[full$Pclass == '2' & full$Embarked == 'C', ]$Fare, na.rm = TRUE)
# full$Fare[which((full$Fare == 0 | is.na(full$Fare))& full$Pclass =='3' & full$Embarked == 'C')] <- median(full[full$Pclass == '3' & full$Embarked == 'C', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='1' & full$Embarked == 'S')] <- median(full[full$Pclass == '1' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='2' & full$Embarked == 'S')] <- median(full[full$Pclass == '2' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='3' & full$Embarked == 'S')] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='1' & full$Embarked == 'Q')] <- median(full[full$Pclass == '1' & full$Embarked == 'Q', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='2' & full$Embarked == 'Q')] <- median(full[full$Pclass == '2' & full$Embarked == 'Q', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='3' & full$Embarked == 'Q')] <- median(full[full$Pclass == '3' & full$Embarked == 'Q', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='1' & full$Embarked == 'C')] <- median(full[full$Pclass == '1' & full$Embarked == 'C', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='2' & full$Embarked == 'C')] <- median(full[full$Pclass == '2' & full$Embarked == 'C', ]$Fare, na.rm = TRUE)
full$Fare[which((is.na(full$Fare)) & full$Pclass =='3' & full$Embarked == 'C')] <- median(full[full$Pclass == '3' & full$Embarked == 'C', ]$Fare, na.rm = TRUE)

#Extract FamilySize Feature
full$FamilySize <- full$Parch + full$SibSp + 1

#Extract Cabin Prefix
full$Cabin <- as.character(full$Cabin) 
full$CabinType <- sapply(full$Cabin, FUN = function(x) {substring(x, 1, 1)}) 
full$CabinType[full$CabinType==""] <- "NA" 
full$CabinType <- factor(full$CabinType)

#Extract Whether Passenger has Cabin Feature -> should we figure out how many cabins? it matters
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
mosaicplot(table(full$ageType, full$Survived), main='Age Group by Survival', shade=TRUE)

# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
  full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
  
  
#Predict Age based upon the other factors
# Commented out Rpart age to use 
# ##Dak##
# dectree.ages <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare + IsMarried, data = full[!is.na(full$Age), ], method = "anova")
# dectree.ages <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = full[!is.na(full$Age), ], method = "anova")
# set.seed(550)
# 
# full.imputed <- rfImpute(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare, train.titanic)
# full.imputed <- rfImpute(Age ~ ., train.titanic)
# 
# full.ageimputed <- na.omit(full[which(colnames(full) == 'Age')])
# full.ageimputed <- full[!is.na(full$Age), ]
# 
# rf.fit.age <- randomForest(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + HasCabin + CabinType + FaresOnTicket + IndividualFare, data = full.ageimputed, importance=TRUE, ntree = 3000)
# prediction.age <- predict(rf.fit.age, full)
# prediction.age[!is.na(full$Age)] <- full$Age[!is.na(full$Age)]
# 
# full$Age <- prediction.age
# full$Age[is.na(full$Age)] <- prediction.age[is.na(full$Age), ]
# 
# submission.titanic <- data.frame(PassengerId = test.titanic$PassengerId, Survived = prediction)
# 
# #Copy predicted ages back into full dataset
# full$Age[is.na(full$Age)] <- predict(dectree.ages, full[is.na(full$Age), ])
# train.titanic <- full[1:891,] test.titanic <- full[892:1309,]

# using MICE
  # Make variables factors into factors
  factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                   'Title','Surname','Family','FsizeD','Cabin','NameChar','MaidenName')
  
  full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
  
  # Set a random seed
  set.seed(324)
  
  # Perform mice imputation, excluding certain less-than-useful variables:
  mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived','NameChar','MaidenName')], method='rf') 
  
  # Save the complete output 
  mice_output <- complete(mice_mod)
  
  # Plot age distributions
  par(mfrow=c(1,1))
  hist(full$Age, freq=F, main='Age: Original Data', 
       col='darkgreen', ylim=c(0,0.04))
  hist(mice_output$Age, freq=F, main='Age: MICE Output', 
       col='lightgreen', ylim=c(0,0.04))
  #Show the diffs
  ages <- cbind(full$Age, mice_output$Age)
  ages
  # Replace Age variable from the mice model.
  full$Age <- mice_output$Age
  
  #Now that we have age on everyone... what can we do with it?
  # First we'll look at the relationship between age & survival
  ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
    geom_histogram(bins=8) + 
    # I include Sex since we know (a priori) it's a significant predictor
    #facet_grid(.~Sex) + 
    theme_few()
  
  age = full$Age[1:891]
  survived = full$Survived[1:891]
  df <- data.frame(age,survived)
  ggplot(df, aes(age,fill = factor(survived))) + geom_histogram(bins=10)
  # create bar chart to show relationship between survival rate and age intervals
  cuts <- cut(full$Age[1:891],hist(full$Age[1:891],10,plot = F)$breaks)
  rate <- tapply(full$Survived[1:891],cuts,mean)
  d2 <- data.frame(age = names(rate),rate)
  barplot(d2$rate, xlab = "age",ylab = "survival rate")
  # I see that it's more likely to survive if <10 or >60
  
  
  # Create the column child, and indicate whether child or adult
  full$ageType[full$Age < 16] <- 0
  full$ageType[full$Age >= 16 & full$Age < 32] <- 1
  full$ageType[full$Age >= 32 & full$Age < 48] <- 2
  full$ageType[full$Age >= 48 & full$Age < 64] <- 3
  full$ageType[full$Age >= 64] <- 4
  
  # Show counts
  table(full$ageType, full$Survived)
  
  # creating a 3 variable sex feature
  # WaC is women and children
  full$WaC[full$Sex == "female" & full$Age >16] <- "F"
  full$WaC[full$Sex == "male" & full$Age >16] <- "M"
  full$WaC[full$Age <= 16] <- "C"
  
  # Adding Mother variable
  full$Mother <- 'Not Mother'
  full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
  
  # Show counts
  table(full$Mother[which(full$Sex=='female')], full$Survived[which(full$Sex=='female')])
  
  # Finish by factorizing our two new factor variables
  full$ageType  <- factor(full$ageType)
  full$Mother <- factor(full$Mother)
  full$WaC <- factor(full$WaC)
  
  #Ready to predict
  # Split the data back into a train set and a test set
  train.titanic <- full[1:891,]
  test.titanic <- full[892:1309,]
  
  # Set a random seed
  set.seed(298)
  
  # Build the model (note: not all possible variables are used)
  # removed individual family metrics
  # changed to ageType
  rf_model <- randomForest(factor(Survived) ~ Pclass + 
                                              Sex + 
                                              Age + 
                                              #ageType +
                                              WaC +
                                              FaresOnTicket +
                                              IndividualFare + 
                                              Embarked + 
                                              Title + 
                                              FsizeD + 
                                             # Mother + 
                                              HasCabin,
                                              data = train.titanic, ntree=1501,mtry=2, importance=T) 
  # Show model error
  plot(rf_model, ylim=c(0,0.36))
  legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
#titanic.rf.model <- randomForest(Survived ~ ., data=train.titanic);
#dectree.control = rpart.control(minsplit = 100, minbucket = 1, cp = 0, maxdepth = 30)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test.titanic)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
submission.titanic <- data.frame(PassengerId = test.titanic$PassengerId, Survived = prediction)

#Create new folder to store outputs
dir.create(paste0("./Competition/",attempt))
#Write the updated train dataset to file
write.csv(full,paste0("./Competition/",attempt,"/full",attempt,".cvs"), row.names = FALSE)
write.csv(train.titanic,paste0("./Competition/",attempt,"/train",attempt,".cvs"), row.names = FALSE)
write.csv(test.titanic,paste0("./Competition/",attempt,"/test",attempt,".cvs"), row.names = FALSE)

# Write the solution to file
write.csv(submission.titanic,paste0("./Competition/",attempt,"/Submission",attempt,".csv"), row.names = FALSE)