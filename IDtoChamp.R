STRINGLEAGUE <- read.csv("~/Documents/STRINGLEAGUE.txt", header=FALSE)
#from the api, I've got a string with
#the name, id, and roles of the champ 
#only taking primary role in this
test = t(STRINGLEAGUE)#convert league string into a single column separated by ,
#messy so we're going to have to clean this up
NAME = test[grepl("name", test),]
NAME2 = gsub("name:","", NAME)

TAGS = test[grepl("tags:", test),]
TAGS2 = gsub("tags:\\[|\\]}", "", TAGS)
TAGS2
length(TAGS2)
length(NAME2)


ID = test[grepl("id:", test),]
ID = gsub("data:\\{","", ID)
ID
IDSPLIT = strsplit(ID, ":")
IDSPLIT[[1]][3]
length(IDSPLIT)
IDonly = sapply(1:129, function(x) IDSPLIT[[x]][3])

total = data.frame(IDonly, NAME2, TAGS2)
total$IDonly = as.numeric(as.character(total$IDonly))

total2 = total[with(total, order(IDonly)),]
write.csv(total2, file = "IDtoChamp.csv")
#eventually we end up with champ ID, champ Name, and primary role
