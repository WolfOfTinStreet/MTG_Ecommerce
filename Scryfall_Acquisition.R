library(jsonlite)
library(httr)


total = 50598 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) 
card_dictionary <- NULL
for(i in 1:nrow(Scryfall_IDS)){
  scryfall_link <- paste("https://api.scryfall.com/cards/",Scryfall_IDS$scryfallID[i],sep="")
  scryfall <- GET(scryfall_link)
  
  oracle_id <- (content(scryfall,"parsed")$oracle_id)
  tcgplayer_id <- (content(scryfall,"parsed")$tcgplayer_id)
  if(is.null(tcgplayer_id) == T){tcgplayer_id = NA}
  card <- (content(scryfall,"parsed")$name)
  if(is.null(card) == T){card = NA}
  release_date <- (content(scryfall,"parsed")$released_at)
  if(is.null(release_date) == T){release_date = NA}
  cmc <- (content(scryfall,"parsed")$cmc)
  if(is.null(cmc) == T){cmc = NA}
  type_line <- (content(scryfall,"parsed")$type_line)
  if(is.null(type_line) == T){type_line = NA}
  standard <- (content(scryfall,"parsed")$legalities$standard)
  if(is.null(standard) == T){standard = NA}
  pioneer <- (content(scryfall,"parsed")$legalities$pioneer)
  if(is.null(pioneer) == T){pioneer = NA}
  modern <- (content(scryfall,"parsed")$legalities$modern)
  if(is.null(modern) == T){modern = NA}
  legacy <- (content(scryfall,"parsed")$legalities$legacy)
  if(is.null(legacy) == T){legacy = NA}
  commander <- (content(scryfall,"parsed")$legalities$commander)
  if(is.null(commander) == T){commander = NA}
  foil <- (content(scryfall,"parsed")$foil)
  if(is.null(foil) == T){foil = NA}
  nonfoil <- (content(scryfall,"parsed")$nonfoil)
  if(is.null(nonfoil) == T){nonfoil = NA}
  promo <- (content(scryfall,"parsed")$promo)
  if(is.null(promo) == T){promo = NA}
  reprint <- (content(scryfall,"parsed")$reprint)
  if(is.null(reprint) == T){reprint = NA}
  set_abbr <- (content(scryfall,"parsed")$set)
  if(is.null(set_abbr) == T){set_abbr = NA}
  edition <- (content(scryfall,"parsed")$set_name)
  if(is.null(edition) == T){edition = NA}
  collector_number <- (content(scryfall,"parsed")$collector_number)
  if(is.null(collector_number) == T){collector_number = NA}
  rarity <- (content(scryfall,"parsed")$rarity)
  if(is.null(rarity) == T){rarity = NA}
  edhrec <- content(scryfall,"parsed")$edhrec_rank
  if(is.null(edhrec) == T){edhrec = NA}
  color_1 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[1]]}, error = function(e){color_1 = "NA"})
  color_2 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[2]]}, error = function(e){color_2 = "NA"})
  color_3 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[3]]}, error = function(e){color_3 = "NA"})
  color_4 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[4]]}, error = function(e){color_4 = "NA"})
  color_5 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[5]]}, error = function(e){color_5 = "NA"})
  color_6 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[6]]}, error = function(e){color_6 = "NA"})
  if(is.null(color_1) == T){color_1 = NA}
  if(is.null(color_2) == T){color_2 = NA}
  if(is.null(color_3) == T){color_3 = NA}
  if(is.null(color_4) == T){color_4 = NA}
  if(is.null(color_5) == T){color_5 = NA}
  if(is.null(color_6) == T){color_6 = NA}
  purchase_tcg <- (content(scryfall,"parsed")$purchase_uris$tcgplayer)
  
  row_info <- cbind(oracle_id,tcgplayer_id,card,release_date,cmc,type_line,
                    standard,pioneer,modern,legacy,commander,foil,nonfoil,
                    promo,reprint,set_abbr,edition,collector_number,rarity,
                    edhrec,purchase_tcg)
  card_dictionary <- rbind(card_dictionary, row_info)
  colnames(row_info)
  colnames(card_dictionary)
  setTxtProgressBar(pb,i)
  Sys.sleep(.12)
}

card_dictionary_backup <- card_dictionary

