

games = gamesID[,2:length(gamesID)]
samp = sample(1:nrow(games), .3*nrow(games))
TESTX = games[samp,2:length(games)]
TESTY = games[samp,1]
TRAINX = games[-samp,2:length(games)]
TRAINY = games[-samp,1]


###

games = data.frame(nonbin[,2],aa4)
IDtoChamp = read.csv("/Users/chrismurphy/IDtoChamp.csv")
IDtoChamp$NAME2 = as.character(IDtoChamp$NAME2)
names(games)[2:130] = IDtoChamp$NAME2
names(games)[131:259]=IDtoChamp$NAME2

IDtoChamp$TAGS = as.character(IDtoChamp$TAGS)
unique(IDtoChamp$TAGS)
Fighters = IDtoChamp[IDtoChamp$TAGS == "Fighter",]$NAME2
Mage = IDtoChamp[IDtoChamp$TAGS == "Mage",]$NAME2
Tank = IDtoChamp[IDtoChamp$TAGS == "Tank",]$NAME2
Marksman = IDtoChamp[IDtoChamp$TAGS == "Marksman",]$NAME2
Assassin = IDtoChamp[IDtoChamp$TAGS == "Assassin",]$NAME2
Support = IDtoChamp[IDtoChamp$TAGS == "Support",]$NAME2
#### New Features
games$Fighters = 0
games$Mage = 0
games$Tank = 0
games$Marksman = 0
games$Assassin = 0
games$Support = 0

games$Fighters2 = 0
games$Mage2 = 0
games$Tank2 = 0
games$Marksman2 = 0
games$Assassin2 = 0
games$Support2 = 0



TypeCount = function(games = games, class, colly, team = 1)
{
  if (team == 1)
  {
    team1 = games[,2:130]
    newdf = team1[,names(team1) %in% class]
    
    games[,colly] = rowSums(newdf)
  }
  else
  {
    team2 = games[,131:259]
    newdf = team2[,names(team2) %in% class]
    games[,colly] = rowSums(newdf)
  }
  return(games)
}
games = TypeCount(games,Fighters,260,team=1)
games = TypeCount(games,Mage,261,team=1)
games = TypeCount(games,Tank,262,team=1)
games = TypeCount(games,Marksman,263,team=1)
games = TypeCount(games,Assassin,264,team=1)
games = TypeCount(games,Support,265,team=1)

games = TypeCount(games,Fighters,266,team=2)
games = TypeCount(games,Mage,267,team=2)
games = TypeCount(games,Tank,268,team=2)
games = TypeCount(games,Marksman,269,team=2)
games = TypeCount(games,Assassin,270,team=2)
games = TypeCount(games,Support,271,team=2)
classcheck = games[,260:271]
#AFTER TYPE COLUMN
IDtoChamp$NAME2 = as.character(IDtoChamp$NAME2)
IDtoChamp$NAME2 = gsub(" ", "", IDtoChamp$NAME2)
names(games)[2:130] = IDtoChamp$NAME2
names(games)[131:259]=paste(IDtoChamp$NAME2, "2", sep = "")
names(games)
