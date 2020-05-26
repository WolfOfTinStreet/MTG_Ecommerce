install.packages("dplyr")
library(tidyverse)
setwd("/home/cujo253/Essential_Referential_CSVS/Set_Jsons/")

temp <- list.files(pattern="*.json")
Number_Of_Files <- length(temp)
fromJSON(temp[i])$cards
Card_Dictionary <- NULL
for (i in 1:Number_Of_Files){
  rdate <- fromJSON(temp[i])$releaseDate
  uuid <- fromJSON(temp[i])$cards$uuid
  if(is.null(uuid)==T){uuid = NA}
  scryfall_id <- fromJSON(temp[i])$cards$scryfallId
  if(is.null(scryfall_id)==T){scryfall_id = NA}
  mcmid <- fromJSON(temp[i])$cards$mcmId
  if(is.null(mcmid)==T){mcmid = NA}
  tcg_ID <- fromJSON(temp[i])$cards$tcgplayerProductId
  if(is.null(tcg_ID)==T){tcg_ID = NA}
  card <- fromJSON(temp[i])$cards$name
  if(is.null(card)==T){card = NA}
  set <- fromJSON(temp[i])$name
  if(is.null(set)==T){set = NA}
  abbr <- fromJSON(temp[i])$code
  if(is.null(abbr)==T){abbr = NA}
  rarity <- fromJSON(temp[i])$cards$rarity
  if(is.null(rarity)==T){rarity = NA}
  number <- fromJSON(temp[i])$cards$number
  if(is.null(number)==T){number = NA}
  types <- fromJSON(temp[i])$cards$types
  if(is.null(types)==T){types = NA}
  hasFoil <- fromJSON(temp[i])$cards$hasFoil
  if(is.null(hasFoil)==T){hasFoil = NA}
  hasNonFoil <- fromJSON(temp[i])$cards$hasNonFoil
  if(is.null(hasNonFoil)==T){hasNonFoil = NA}
  isAlternative <- fromJSON(temp[i])$cards$isAlternative
  if(is.null(isAlternative)==T){isAlternative = NA}
  # variations <- fromJSON(temp[i])$cards$variations
  # if(is.null(variations)==T){variations = NA}
  # standard <- fromJSON(temp[i])$cards$legalities.standard
  if(is.null(standard)==T){standard = NA}
  pioneer <- fromJSON(temp[i])$cards$legalities.pioneer
  if(is.null(pioneer)==T){pioneer = NA}
  modern <- fromJSON(temp[i])$cards$legalities.modern
  if(is.null(modern)==T){modern = NA}
  legacy <- fromJSON(temp[i])$cards$legalities.legacy
  if(is.null(legacy)==T){legacy = NA}
  commander <- fromJSON(temp[i])$cards$legalities.commander
  if(is.null(commander)==T){commander = NA}
  pauper <- fromJSON(temp[i])$cards$legalities.pauper
  if(is.null(pauper)==T){pauper = NA}
  info <- cbind(rdate,uuid,scryfall_id,mcmid,tcg_ID,card,set,abbr,rarity,number,types,hasFoil,hasNonFoil,isAlternative,standard,pioneer,modern,legacy,commander,pauper)
  Card_Dictionary <- rbind(Card_Dictionary,info)
}

Card_Dictionary_backup <- Card_Dictionary
#Card_Dictionary <- Card_Dictionary_backup
#Card_Dictionary <- unlist(Card_Dictionary[15])
Card_Dictionary <- as.data.frame(Card_Dictionary)
Card_Dictionary$rdate <- unlist(Card_Dictionary[1])
Card_Dictionary$uuid <- unlist(Card_Dictionary[2])
Card_Dictionary$scryfall_id <- unlist(Card_Dictionary[3])
Card_Dictionary$mcmid <- unlist(Card_Dictionary[4])
Card_Dictionary$tcg_ID<- unlist(Card_Dictionary[5])
Card_Dictionary$card <- unlist(Card_Dictionary[6])
Card_Dictionary$set <- unlist(Card_Dictionary[7])
Card_Dictionary$abbr <- unlist(Card_Dictionary[8])
Card_Dictionary$rarity <- unlist(Card_Dictionary[9])
Card_Dictionary$number <- unlist(Card_Dictionary[10])
Card_Dictionary$hasFoil <- unlist(Card_Dictionary[12])
Card_Dictionary$hasNonFoil <- unlist(Card_Dictionary[13])
Card_Dictionary$isAlternative <- unlist(Card_Dictionary[14])
#Card_Dictionary$variations <- unlist(Card_Dictionary[15])
Card_Dictionary$standard <- unlist(Card_Dictionary[15])
Card_Dictionary$pioneer <- unlist(Card_Dictionary[16])
Card_Dictionary$modern <- unlist(Card_Dictionary[17])
Card_Dictionary$legacy <- unlist(Card_Dictionary[18])
Card_Dictionary$commander <- unlist(Card_Dictionary[19])
Card_Dictionary$pauper <- unlist(Card_Dictionary[20])
Card_Dictionary <- Card_Dictionary[-11]
Card_Dictionary$rarity <- ifelse(Card_Dictionary$rarity == "mythic","M",
                                 ifelse(Card_Dictionary$rarity == "rare","R",
                                        ifelse(Card_Dictionary$rarity == "uncommon","U",
                                               ifelse(Card_Dictionary$rarity == "common","C", Card_Dictionary$rarity))))

Special_Card_Dictionary <- Card_Dictionary[grepl("\\★",Card_Dictionary$number),]
Nonfoil_Card_Dictionary <- Card_Dictionary[!grepl("\\★",Card_Dictionary$number),]
# setwd("/home/cujo253/Essential_Referential_CSVS/")
# 
# 
# csvFileName <- paste("Special_Card_Dictionary",".csv",sep="")
# write.csv(Special_Card_Dictionary, file=csvFileName, row.names = FALSE)
# 
# csvFileName <- paste("Nonfoil_Card_Dictionary",".csv",sep="")
# write.csv(Nonfoil_Card_Dictionary, file=csvFileName, row.names = FALSE)
# 

Nonfoil_Only <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == F),]
Nonfoil_Only$hasFoil <- ""
Nonfoil_Only$hasNonFoil <- ""
Foil_Only <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == F & Nonfoil_Card_Dictionary$hasFoil == T),]
Foil_Only$hasFoil <- " FOIL"
Foil_Only$hasNonFoil <- ""
Nonfoil_Halfs <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == T),]
Nonfoil_Halfs$hasFoil <- ""
Nonfoil_Halfs$hasNonFoil <- ""
Foil_Halfs <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == T),]
Foil_Halfs$hasFoil <- " FOIL"
Foil_Halfs$hasNonFoil <- ""

Entire_Dictionary <- rbind(Nonfoil_Only, Foil_Only)
Entire_Dictionary <- rbind(Entire_Dictionary,Nonfoil_Halfs)
Entire_Dictionary <- rbind(Entire_Dictionary,Foil_Halfs)
Entire_Dictionary$Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,Entire_Dictionary$number,sep="")
Entire_Dictionary$Working_Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,sep="")
