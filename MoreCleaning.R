nonbin = read.csv("/Users/chrismurphy/Downloads/gamesunique.csv", header=FALSE)
nonbin2 = nonbin[,-c(1,2)] #load original 12 col data frame (gameID, win/loss, 10 champs)
# want to convert to a binary format (1 if champ is in the game, 0 if not)

uniquesortchamp = factor(sort(unique(unlist(nonbin2))))#champID's
newframe = as.numeric(uniquesortchamp)#special way to convert the champ ID's
#vector into 1:129 (originally not linear ID's... i.e 50, 51, 53, 54... etc)
secondframe = as.numeric(as.character(uniquesortchamp))
#this is the numeric vector of the original ID's
#the goal is to convert each champ ID into a column (129 for team 1, 129 for team 2)

whatnum = function(x, column)
{
  newframe[as.numeric(nonbin2[,column])[x]==secondframe]
}
#using nested apply function to go across each of the 1:10 columns and over each row
yes = sapply(1:10, function(y) sapply(1:nrow(nonbin2), function(x) whatnum(x,y)))
yes2 = data.frame(yes)
# convert all champ ID's into 1 to 129 from their original
emptydf= data.frame(matrix(0, nrow=nrow(nonbin), ncol=129*2))
#new empty df with all columns

conversion = function(rownum, emptydf) #takes in a row and emptydf to make into a binary df
{
  row1 = as.numeric(yes2[rownum,])
  emptydf[rownum,row1[1:5]]=1
  emptydf[rownum,row1[6:10]+129]=1
  return(emptydf[rownum,])
  
}
aa = data.frame(sapply(1:nrow(yes2), function(x) conversion(x,emptydf)))
aa = t(aa)#comes out 202234 columns by 258 rows (switched)
aa2 = as.data.frame(aa)
aa3 = unlist(aa2) #due to some class problems, I'm getting a least in each observation
#so unlisting and making into a matrix then back to a df
aa4 = data.frame(matrix(aa3, nrow =202234, ncol=258))

games = aa4 #now we have our new data frame same number of rows with 129x2 columns