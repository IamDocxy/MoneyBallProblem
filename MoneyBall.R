library(dplyr)
library(ggplot2)

#Loading the batting.csv into a dataframe
batting <- read.csv("MoneyBall Project/MoneyBallProblem/Batting.csv")
head(batting)

#Creating column Batting Average BA.
#Batting Average measures the performance of batters in baseball.
#BA is equal to H (Hits) divided by AB (At Bats).
batting$BA <- batting$H/batting$AB

#Creating column On Base Percentage OBP.
#On Base Percentage measures how frequently a batter reaches base.
#OBP is equal to (H + BB + HBP)/(AB + BB + HBP + SF)
batting$OBP <- (batting$H + batting$BB + batting$HBP)/
                  (batting$AB + batting$BB + batting$HBP + batting$SF)

#Creating column Slugging Percentage SLG.
#Slugging Percentage measures the batting productivity of a hitter.
#SLG is equal to ((1 * X1B) + (2 * X2B) + (3 * X3B) + (4 * HR) ) / AB
#X1B = H - X2B - X3B - HR
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
batting$SLG <- ((1*batting$X1B)+(2*batting$X2B)+(3*batting$X3B)+(4*batting$HR))/
                  batting$AB

str(batting)

#Loading the salary.csv into a dataframe
sal <- read.csv(("MoneyBall Project/MoneyBallProblem/Salaries.csv"))
head(sal)

summary(batting) #Year starts from 1871 in batting.csv
summary(sal) # and 1985 in salary.csv

batting85 <- subset(batting,batting$yearID>=1985)
summary(batting85)

#Merging the batting85 and sal dataframes on playerID and yearID
#since the combination of playerID and yearID
#is unique in both the dataframes
fullData <- merge(batting85,sal,c('playerID','yearID'))
summary(fullData)

#Retrieving the stats of the lost players for 2001
#Jason Giambi (giambja01)
#Johnny Damon (damonjo01)
#Rainer Gustavo "Ray" Olmedo (saenzol01).

lostPlayers <- subset(subset(fullData,yearID == 2001),
                      playerID %in% c('giambja01','damonjo01','saenzol01'))

#Retrieving the H,X1B,X2B,X3B,HR,OBP,SLG,BA,AB stats of the lost players
lostPlayers <- lostPlayers[,c('playerID','H','X2B','X3B'
                              ,'HR','OBP',"SLG","BA","AB")]
head(lostPlayers)

#Finding replacement players from 2001
replacementPlayers <- subset(fullData,fullData$yearID == 2001)

ggplot(replacementPlayers,aes(x = OBP, y = salary)) + geom_point()

#Removing players with 0 OBP
replacementPlayers <- subset(replacementPlayers,OBP > 0)

#Selecting players with salary < 8,000,000
replacementPlayers <- filter(replacementPlayers,salary < 8000000)

#Total AB of the lost players is 1469.
#AB of each new player => 1469/3 = 490
replacementPlayers <- filter(replacementPlayers, AB >= 490)

#Sorting the available players by OBP
replacementPlayers <- arrange(replacementPlayers,desc(OBP))

#Removing Jason Giambi (giambja01)
#Johnny Damon (damonjo01) and
#Rainer Gustavo "Ray" Olmedo (saenzol01) 
#from the list of available players.
replacementPlayers <- filter(replacementPlayers,!playerID %in% 
                               c('giambja01','damonjo01','saenzol01'))

finalisedPlayers <- replacementPlayers[,c('playerID','AB','BA','OBP','SLG','salary')]
finalisedPlayers <- arrange(finalisedPlayers,-AB)
finalisedPlayers
