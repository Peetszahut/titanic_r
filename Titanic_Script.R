library(dplyr)
library(ggplot2)
library(mice)

originalDataImport.train <- read.csv("train.csv", header = TRUE, sep = ",")
originalDataImport.test <- read.csv("test.csv", header = TRUE, sep = ",")
originalDataImport.train <- tbl_df(originalDataImport.train)
originalDataImport.test <- tbl_df(originalDataImport.test)
originalData <- bind_rows(originalDataImport.train, originalDataImport.test)



originalData$Titles <- gsub("(.*, )|(\\..*)", "", originalData$Name)

originalData$Titles[originalData$Titles == "Mme"]  <- "Miss"
originalData$Titles[originalData$Titles == "Mlle"] <- "Miss"
originalData$Titles[originalData$Titles == "Ms"]   <- "Miss"

rareTitles <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major",
                "Rev", "Sir", "the Countess", "Dona")
originalData$Titles[originalData$Titles %in% rareTitles] <- "Rare Titles"

## Seperates last name from the $Name column.  The string split is splitting at any point with a ',' or '.'
## The [[1]][1] is for list notation.  It stands for the first 'array' from the list, then the first object
## in the array.  
originalData$lastName <- sapply(originalData$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


## n_distinct() can be used to see how many distinct names there are 
## cat(paste()) is when you want to print with variables inside
cat(paste("There are",n_distinct(originalData$lastName), "families out of ", length(originalData$lastName),
          "people"))

table(originalData$Sex, originalData$Titles) ## Checks how many titles 

## Create a new variable with family size per person and add the person onto it.  Will graph to see how many people
## survived based on the family size

originalData$familySize <- originalData$SibSp + originalData$Parch + 1

## For stat = "count" you only need the x component.  Fill has to be a factor or else the bar graph doesn't know
## what to do colorwise and just makes it all one color
## seq(1,11,1) == c(1:11)
ggplot(originalData[1:891,], aes(x = familySize, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = "dodge") +
  scale_x_continuous(breaks = seq(1,4,1))

## Change family sizes to 'small' 'medium' 'large' based off observation that single people had horrible 
## time surviving and so did families >= 5.  First adds a new column, then uses logic tests to give the correct
## size.  Then, the 3 sizes are converted to a factor and plotted

originalData <- mutate(originalData, familySizeD = 'single')
originalData$familySizeD[originalData$familySize == 1]                               <- "single"
originalData$familySizeD[originalData$familySize > 1 & originalData$familySize < 5 ] <- 'small'
originalData$familySizeD[originalData$familySize > 4]                                <- 'large'
originalData$familySizeD <- factor(originalData$familySizeD, levels = c("single", "small", "large"))


ggplot(originalData[1:891,], aes(x = familySizeD, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = "dodge")


originalData <- mutate(originalData, deck = '')
originalData$deck <- factor(sapply(originalData$Cabin, 
                      function (x) strsplit(x, NULL)[[1]][1]))

## Counts the empty and NA values in the Embarked column
count(originalData, Embarked == '' | is.na(Embarked)) ## Counts the number of empty / NA
filter(originalData, Embarked == '' | is.na(Embarked)) ## Shows which rows they are

originalData$Embarked[originalData$Embarked == '' | is.na(originalData$Embarked)]

## Function to check all columns for NA or missing values
## Good function to use implemtn in all your code
CheckNA <- function(tempFrame){
  rList <- sapply(tempFrame,
             function(x) sum(x == '' | is.na(x)))
  return(rList)
}


#### Boxplot is used to check the median values of each class at each port.  It shows that the median price ticket
#### at C was around $80, while S is not as likely due to only 25% of the people paying over 80
temp <- originalData %>%
  filter(Fare < 500) 

ggplot(temp, aes(x = factor(Embarked), y = Fare, fill = factor(Pclass))) + geom_boxplot() +
  geom_hline(aes(yintercept = 80), colour = "Blue")


cond <- (originalData$PassengerId == 62 | originalData$PassengerId == 830)
originalData$Embarked[cond] <- "C"

#### Missing Fare value  = PassengerId = 1044
View(filter(originalData, Fare == '' | is.na(Fare)))

## Embarked = S | PClass = 3 | familySize = 1 | Ticket = 3701
fareFind <- filter(originalData, Embarked == "S" & Pclass == 3 & familySize == 1)
originalData$Fare[originalData$PassengerId == 1044] <- median(fareFind$Fare, na.rm = TRUE)

#### Save Point
orgSave <- originalData

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','familySize','familySizeD')

originalData[factor_vars] <- lapply(originalData[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

#### Perform mice imputation, excluding certain less-than-useful variables:
##   method = 'rf' stands for random forest method
mice_mod <- mice(originalData[, !names(originalData) %in% c('PassengerId','Name','Ticket','Cabin',
                                            'Family','Surname','Survived')], method='rf') 
miceOut <- complete(mice_mod)

#### par() is used to create multiple plots in the same window.  mfrow=c(# of rows, # of columns)
##   freq = FALSE makes it a density plot
##   The histograms is to sanity check the distribution of the imputed data versus the original
##   If the distribution looks significantly different,then you know something went wrong
par(mfrow=c(1,2))
hist(originalData$Age, freq = FALSE, col = 'blue', main = 'originalData$Age', ylim = c(0,0.04))
hist(miceOut$Age, freq = FALSE, col = 'green', main = 'miceData$Age', ylim = c(0,0.04))

originalData$Age <- miceOut$Age

#### Need more work done on imputations and different methods of imputations

#### Check male/female survive by age 1-891
##   geom_histogram() is used instead of geom_bar() because bar will draw a line for EVERY age.  This 
##   makes it very hard to read.  Histogram is placing them into seperate bins
ggplot(originalData[1:891,], aes(x = Age, fill = factor(Survived))) + 
  geom_histogram() +
  facet_grid(.~Sex)

originalData$Titles <- factor(originalData$Titles)
originalData$Embarked <- factor(originalData$Embarked)
#### RandomForest model
##   Split the data back into train/test sets
train <- originalData[1:891,]
test  <- originalData[892:1309,]

###################################### RANDOM FOREST ##############################################
## Set seed for random forest
set.seed(754)


rfModel <- randomForest(factor(Survived) ~ Pclass + Sex + Age +
                                           Fare + Titles ,
                                           data = train)
## par() will partition your graph UNTIL you change it
par(mfrow=c(1,1))
plot(rfModel, ylim=c(0,0.36))
legend('topright', colnames(rfModel$err.rate), col=1:3, fill=1:3)

#### Make a dataframe of the values
testImp <- importance(rfModel)
nameVec <- rownames(testImp)

df <- tbl_df(data.frame(nameVec, testImp))
df <- mutate(df, percentTotal = (MeanDecreaseGini / (sum(MeanDecreaseGini))) * 100)

ggplot(df, aes(reorder(nameVec, MeanDecreaseGini), percentTotal)) + 
  geom_bar(stat = "identity") + coord_flip() + theme_classic()


prediction <- predict(rfModel, test)

solution <- data.frame(PassengerId = test$PassengerId, Survived = prediction)

write.csv(solution, file = 'rf_predicted2.csv', row.names = F)

###################################### RANDOM FOREST ##############################################







