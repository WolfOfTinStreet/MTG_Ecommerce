#Packages & Needed Functions####
install.packages("rvest")     # HTML Hacking & Web Scraping
install.packages("httr")  # JSON manipulation
# install.packages("tidyverse") # Data Manipulation
# install.packages("reshape2") 
# install.packages("dplyr")
# install.packages("knitr")     # Pretty HTML Tables
# install.packages("purrr")     # Allows for the replacement of loops and suite of apply functions
# install.packages("tibble")    # Breakdown further elements
# install.packages("dplyr", dependencies = TRUE, INSTALL_opts = '--no-lock')     # Data Manipulation
# install.packages("tidyr")     # The Janitor is this guy
# install.packages("tictoc")
# install.packages("taRifx")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("zoo")
# install.packages("plyr")
# install.packages("rlist")
# install.packages("fpp2")
# install.packages("forecast")
# install.packages("stats")
# install.packages("tseries")
# install.packages("data.table")
# install.packages("gtools")
# install.packages("XML")
# install.packages("RCurl")
# install.packages("xml12")
#If already Installed, Load Packages in from here####
library(easypackages)
libraries("rvest","jsonlite","plyr","readr","purrr","tidyr","tibble","tidyverse","reshape2","knitr","tictoc","devtools",
          "taRifx","ggplot2","zoo","dplyr","rlist","fpp2","forecast","stats","tseries","data.table","gtools","RSelenium","httr")
#library(svMisc)    # Progress bar on loops to make me feel good - Outsourced
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
  substr(text, 1, num_char)
} #Recreating the left function from Excel 
funk <- function(t){
  ifelse(nchar(t) <= 10, right(t,1),ifelse(nchar(t)<=190, right(t,2),ifelse(nchar(t)>=191, right((t),3),0)))
} #Character Count utilization of 'left'&'right' functions for quantity breakdown
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

#CK- Buylist####
setwd("/home/cujo253/")
tic()
All <-NULL #Create Null value to assign our scraped values to in following steps
total = 280 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
for(i in 1:280){
  url <- paste0("https://www.cardkingdom.com/purchasing/mtg_singles?filter%5Bipp%5D=125&filter%5Bsort%5D=edition&filter%5Bsearch%5D=mtg_advanced&page=",i)
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE) # Gets around company firewall restrictions
  html <- read_html("scrapedpage.html")  #html <- read_html(url) html <- read_html("scrapedpage.html")
  json <- html %>% html_nodes(".itemContentWrapper") %>% html_text()
  json <-data.frame(json)
  All <-rbind(All,json)
  #Sys.sleep(sample(3:7, 1))
  setTxtProgressBar(pb,i)
  
} #Cardkingdom BL scraper: URL Location - HTML Elements to 'borrow':Loop results to paste under each other page results to the prior null value: Report current status to loading bar to ensure scrape has not crashed.
toc()
#CK Buylist Formatting####

test<-data.frame(do.call('rbind', strsplit(as.character(All$json),'\n',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
test<-subset(test, select = -c(X8,X9,X10,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X27,X28,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46)) #Remove the NUMEROUS Extraneous columns, we could certainly try to refine the amount being scraped here moving forward.

#test2<-as.data.frame(sapply(test, function(x) x[x != " "])) - OLD, leave me alone, but, just n case, retain.
test2 <- test$X24 %>% dplyr::na_if("") #Remove NA values in a new column (ensures we have done it right without tampering with original output)
test$X24<-test2 #After we are positive, replace the og column with cleaned version
test[]<-t(apply(test,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])})) #Function to remove all NA's from the entire doc (aka I got clever and made it cleaner than above, but fun to see both ways)
test <- as.data.frame(test) #Ensure correct structure
test$X31 = funk(test$X31) #apply our quantity function to the appropriate columns
test$X32 = funk(test$X32) #due to uneven newline escapes, we must also run the quantity function across the following column as well, what i tend to name as the "aux" or "auxillary" column
test2<-data.frame(do.call('rbind', strsplit(as.character(test$X5),' (',fixed=TRUE))) 
test3<-data.frame(do.call('rbind', strsplit(as.character(test2$X2),')',fixed=TRUE))) #59&60, we are breaking apart the cardname and set formatting to recreate in our desired format
test$X4<-trimws(test2$X1)
test$X5<- test3$X1
test$X6 <- test3$X2 #having reviewed the outputs of our delimiting above, reconstruct the card names, sets, and rarity above.
test$X2 <- paste(test$X3,test$X4,test$X5,test$X6,sep = "") #Creating THE PRIMARY KEY for all subsequent scrape joins.
test$X1 <- test$X4#Create a secondary key for mtggoldfish as they serve as the baseline, in terms of all cards we could possibly track
test <- test[c(7,2,3,1,4,5,6,9,14,15)] #Restructure & Reorder the columns into how will fit into my 'honeypot' document
Price_Trim <- function(t){
  ifelse(nchar(t) <= 4, (right(t,3)),ifelse(nchar(t)<= 5, (right(t,4)),ifelse(nchar(t)<= 6, (right(t,5)),ifelse(nchar(t)<= 7, (right(t,6)),ifelse(nchar(t)<= 8, (right(t,7)),0)))))
} #The scrape returns the Price values with a '$', which prevents R from recognizing as a numeric value. Therefor, we must remove the specal character.
test$X24<-Price_Trim(test$X24) #apply the price trim function
test$X24 <- as.numeric(test$X24) #Now convertible to numeric format
test$X24 <- (test$X24 / 100)#CK scraper, for whatever reason, drops the decimal point, so we must adjust back to real dollar amounts (eg, '006' is 6 cents, not 6 dollars)
#LOAD IN "SETS" HERE OR YOUR OUTPUT WILL BE FUGLY
names(test) <- c("MTG_Gold_Key","CK_Key","Card Name", "CK_Modif_Set","Set","Rarity","NF/F","BL_Value","Qty_Des","Qty_Aux") #rename columns to be a tad easier to track
test$Gold_Merge <- "" #This is an utterly superfluous step but it's my code so... your move...
test <- as.data.frame(test) #Ensure proper formatting
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Sets <- Sets[c(1:7)]
names(Sets) <- c("CK_Modif_Set","Goldfish_Full","Goldfish_Abbrev","Goldfish_Abbrev_Foil","TCG_Key","Golf_NF_Scrape","Gold_F_Scrape") #rename columns to identify sets by their website of origin.
Sets$CK_Modif_Set <- trimws(Sets$CK_Modif_Set)
Sets$Goldfish_Full <- trimws(Sets$Goldfish_Full)
Sets$Goldfish_Abbrev <- trimws(Sets$Goldfish_Abbrev)
Sets$TCG_Key <- trimws(Sets$TCG_Key)
test$Gold_Merge <- Sets$Goldfish_Abbrev[match(test$CK_Modif_Set,Sets$CK_Modif_Set)] #Merge goldfish abbreviated sets against cardkingdom scrape values for sets
test$MTG_Gold_Key <- paste(test$`Card Name`,test$Gold_Merge, sep ="") #create the secondary key for goldfish
CK_BL_Output <- test
#Goldfish Market Scrape####
#Create a list of all the sites for MTG Goldfish to retrieve the market values from
sets = list("10E","1E","2ED","3ED","4ED","5DN","5ED","6ED","7E","8ED","9ED","A25","AER","AKH","ALA","ALL","AP","ARB","ARC","ARN","ATQ","AVR","BBD","BFZ","BNG","BOK","BOOSTER","BRB","BTD","C13","C14","C15","C16","C17","C18","C19","CHK","CHR","CM1","CM2","CMA","CMD","CN2","CNS","CON","CSP","DD2","DDC","DDD","DDE","DDF","DDG","DDH","DDI","DDJ","DDK","DDL","DDM","DDN","DDO","DDP","DDQ","DDR","DDS","DDT","DDU","DGM","DIS","DKA","DOM","DRB","DRK","DST","DTK","E01","E02","ELD","EMA","EMN","EVE","EVG","EX","EXP","FEM","FRF","FUT","GK1","GK2","GNT","GPT","GRN","GTC","H09","HML","HOU","ICE","IMA","ISD","JOU","JUD","KLD","KTK","LEA","LEB","LEG","LGN","LRW","M10","M11","M12","M13","M14","M15","M19","M20","MBS","MD1","ME2","ME3","ME4","MED","MED-GRN","MED-RNA","MED-WAR","MH1","MI","MM","MM2","MM3","MMA","MOR","MRD","MS2","MS3","NE","NPH","OD","OGW","ONS","ORI","PC1","PC2","PCA","PD2","PD3","PELD","PLC","PO2","POR","PR","PRM","PRM-ARN","PRM-BAB","PRM-CHP","PRM-FNM","PRM-GBP","PRM-GDP","PRM-GPP","PRM-GUR","PRM-GWP","PRM-HRO","PRM-JSS","PRM-JUD","PRM-LEP","PRM-LPC","PRM-MED","PRM-MPR","PRM-MSC","PRM-OHP","PRM-PRE","PRM-PTP","PRM-REL","PRM-SDCC13","PRM-SDCC14","PRM-SDCC15","PRM-SDCC16","PRM-SDCC17","PRM-SDCC18","PRM-SDCC19","PRM-SPO","PRM-SSP","PRM-UGF","PRM-UMA","PRM-WMCQ","PRM-WPN","PS","PTG","PTK","PZ1","PZ2","RAV","RIX","RNA","ROE","RTR","S00","S99","SCG","SLD","SHM","SOI","SOK","SOM","SS1","SS2","ST","TD0","TD2","TE","THS","TOR","TPR","TSB","TSP","UD","UGL","UL","UMA","UNH","UST","UZ","V09","V10","V11","V12","V13","V14","V15","V16","V17","VAN","VI","VMA","WAR","WL","WWK","XLN","ZEN","THB","MB1")
total = 240 #238 sets on the list above, take my word for it ;)
pb <- txtProgressBar(min=0, max = total, style = 3) # loading bar formatting
Gold_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
tic()
for(i in sets){
  
  url <- paste0("https://www.mtggoldfish.com/index/",print(i),"#paper")
  html <- read_html(url)
  tbls_ls <- html %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  tbls_ls <-data.frame(tbls_ls)
  Gold_Market <-rbind(Gold_Market,tbls_ls)
  setTxtProgressBar(pb,i)
}
toc()
Back_Up_Gold <- Gold_Market
#Gold_Market <- Back_Up_Gold
#

sets_Foil = list("7E_F",
                 "8ED_F",
                 "9ED_F",
                 "10E_F",
                 "M10_F",
                 "M11_F",
                 "M12_F",
                 "M13_F",
                 "M15_F",
                 "AER_F",
                 "AKH_F",
                 "AP_F",
                 "AVR_F",
                 "BFZ_F",
                 "BBD_F",
                 "BOK_F",
                 "BNG_F",
                 "CHK_F",
                 "CSP_F",
                 "CON_F",
                 "CN2_F",
                 "M19_F",
                 "M20_F",
                 "DIS_F",
                 "DOM_F",
                 "DGM_F",
                 "DTK_F",
                 "EVE_F",
                 "EX_F",
                 "FRF_F",
                 "5DN_F",
                 "FUT_F",
                 "GTC_F",
                 "GPT_F",
                 "GRN_F",
                 "IMA_F",
                 "ISD_F",
                 "XLN_F",
                 "JUD_F",
                 "KLD_F",
                 "KTK_F",
                 "LGN_F",
                 "LRW_F",
                 "ORI_F",
                 "A25_F",
                 "MM_F",
                 "MI_F",
                 "MRD_F",
                 "MBS_F",
                 "MH1_F",
                 "MMA_F",
                 "MM2_F",
                 "MM3_F",
                 "MOR_F",
                 "NE_F",
                 "ONS_F",
                 "PS_F",
                 "RAV_F",
                 "RNA_F",
                 "ROE_F",
                 "RIX_F",
                 "SOM_F",
                 "SCG_F",
                 "SHM_F",
                 "SOI_F",
                 "ALA_F",
                 "THS_F",
                 "ELD_F",
                 "TSP_F",
                 "TSB_F",
                 "TOR_F",
                 "UMA_F",
                 "UGL_F",
                 "UNH_F",
                 "UL_F",
                 "UZ_F",
                 "VI_F",
                 "WAR_F",
                 "WL_F",
                 "WWK_F",
                 "M14_F",
                 "ARB_F",
                 "CNS_F",
                 "DKA_F",
                 "DST_F",
                 "EMN_F",
                 "EMA_F",
                 "HOU_F",
                 "IN_F",
                 "JOU_F",
                 "NPH_F",
                 "OGW_F",
                 "OD_F",
                 "PLC_F",
                 "RTR_F",
                 "SOK_F",
                 "ST_F",
                 "TE_F",
                 "UST_F",
                 "UD_F",
                 "ZEN_F")
total = 150 #240 sets on the list above, take my word for it ;)
pb <- txtProgressBar(min=0, max = total, style = 3) # loading bar formatting
Gold_Foil_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
tic()
for(i in sets_Foil){
  
  try(url <- paste0("https://www.mtggoldfish.com/index/",print(i),"#paper"))
  html <- read_html(url)
  tbls_fs <- html %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  tbls_ls <-data.frame(tbls_fs)
  Gold_Foil_Market <-rbind(Gold_Foil_Market,tbls_ls)
  setTxtProgressBar(pb,i)
}
toc()
Foil_Market <- Gold_Foil_Market
#Goldfish Market Formatting####
Gold_Market <- Gold_Market[c(5,1,2,4)] #Snag only the desired columns, we are leaving plenty of good material out here, just not my focus now
Gold_Market$Price <- as.numeric(Gold_Market$Price) #Convert to numeric from characters
Gold_Market$Price <- round(Gold_Market$Price * 0.845,2) #ARBITRARY mkt adjustment factor to serve as estimate for actual value. Thank you mtg goldfish but you also kinda suck here.
Gold_Market$Daily <- paste(Gold_Market$Card,Gold_Market$Set, sep="") #Create secondary goldfish key to act as merger column
Foil_Market <- Foil_Market[c(5,1,2,4)] #Snag only the desired columns, we are leaving plenty of good material out here, just not my focus now
Foil_Market$Price <- as.numeric(Foil_Market$Price) #Convert to numeric from characters
Foil_Market$Price <- round(Foil_Market$Price * 0.845,2) #ARBITRARY mkt adjustment factor to serve as estimate for actual value. Thank you mtg Foilfish but you also kinda suck here.
Foil_Market$Daily <- paste(Foil_Market$Card,Foil_Market$Set, sep="")
test$Gold_Market <- ifelse(test$`NF/F` == " FOIL", Foil_Market$Price[match(test$MTG_Gold_Key,Foil_Market$Daily)] ,Gold_Market$Price[match(test$MTG_Gold_Key,Gold_Market$Daily)])
test <- test[c(1,2,8,9,10,12,3,4,5,6,7,11)] #Rearrange the columns once more for the honey pot
#test$CK_Key = substr(test$CK_Key,1,nchar(test$CK_Key)-1) #Noticed here that my CK key had an extra space at the end that was messing with final export. This line should belong up in CK formatting but since this is where the discovery is most pertinet for followup steps, I've left it here
Basic_Market_Review <- test
Basic_Market_Review <- Basic_Market_Review[c(2,7,9,10,11,6,3,4)]
Basic_Market_Review$Gold_Market[is.na(Basic_Market_Review$Gold_Market)] <- 0
names(Basic_Market_Review) = c("Key","Card","Set","Rarity","F/NF","MKT_Est","BL","BL Qty")
#CK- Bestsellers####
#https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=2
CK_Store_Front <-NULL #Assign NULL value
CK_Prices <- NULL
total = 742 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
tic()
for(i in 1:742){
  url <- paste0("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i)
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  html <- read_html(url)
  json <- html %>% html_nodes(".itemContentWrapper") %>% html_text()
  json <-data.frame(json)
  CK_Store_Front <-rbind(CK_Store_Front,json)
  CK_name <- html %>% html_nodes(".productDetailTitle") %>% html_text()
  CK_set <- html %>% html_nodes(".productDetailSet") %>% html_text()
  CK_set_a <- data.frame(do.call('rbind', strsplit(as.character(CK_set),'(',fixed=TRUE)))
  CK_set_b <- data.frame(do.call('rbind', strsplit(as.character(CK_set_a$X1),'\n',fixed=TRUE)))
  CK_set <- trimws(CK_set_b$X2)
  CK_rarity <- data.frame(do.call('rbind',strsplit(as.character(CK_set_a$X2),')',fixed=TRUE)))
  CK_rarity <- trimws(CK_rarity$X1)
  prices <- html %>% html_nodes(".stylePrice") %>% html_text()
  prices <- data.frame(do.call('rbind',strsplit(as.character(prices),'$',fixed=TRUE)))
  prices <- data.frame(do.call('rbind',strsplit(as.character(prices$X2),'\n',fixed=TRUE)))
  prices <- prices[,1]
  prices <- as.data.frame(prices)
  prices = prices[seq(1, nrow(prices), 4), ]
  prices <- as.data.frame(prices)
  CK_name <- as.data.frame(CK_name)
  CK_set <- as.data.frame(CK_set)
  CK_rarity <- as.data.frame(CK_rarity)
  prices <- as.data.frame(prices)
  CK_NF_MKT <- data.frame(
    X2 <- CK_name,
    X3 <- CK_set,
    X4 <- CK_rarity,
    X5 <- prices)
  CK_Prices <- rbind(CK_Prices, CK_NF_MKT)
  
  setTxtProgressBar(pb,i)
  
}
toc()
CK_Prices_df <- as.data.frame(CK_Prices)
CK_Prices_df$key <- paste(CK_Prices_df$CK_name,CK_Prices_df$CK_set,CK_Prices_df$CK_rarity," ",sep="")
#CK Market Formatting####
CK <-data.frame(do.call('rbind', strsplit(as.character(CK_Store_Front$json),'\n',fixed=TRUE))) #Delimiting
CK2<-data.frame(do.call('rbind', strsplit(as.character(CK$X5),' (',fixed=TRUE))) #Delimiting card name and sets
CK3<-data.frame(do.call('rbind', strsplit(as.character(CK2$X2),')',fixed=TRUE))) #Delimiting card name and sets to make match with prior data frames
CK$X4<-CK2$X1 #Replace superfluous columns with new delimited formats
CK$X5<- CK3$X1 #Replace superfluous columns with new delimited formats
CK$X3 = as.character(CK$X3)
CK4 <- data.frame(do.call('rbind', strsplit(as.character(CK$X3),'\r',fixed=TRUE)))
CK$X3 <- CK4$do.call..rbind...strsplit.as.character.CK.X3.....r...fixed...TRUE..
#CK$X3 = substr(CK$X3,1,nchar(CK$X3)-1) 
CK$X2 <- paste(CK$X3,CK$X4,CK$X5, sep="") #Create primary key
CK_MKT <- CK[c(1,2,3,4,5,11)] #Subset the data, there is a lot* of superfluous data in this data frame
CK_MKT$X1 <- seq.int(nrow(CK_MKT)) #Assign rankings to their Highest demand cards

test2 <- test #Create seperate data frame (df) to the og to ensure I recreate my primary key correctly again
#test2$CK_Key = substr(test$CK_Key,1,nchar(test$CK_Key)-1) #account for that pesky space character again
test2$CK_Key <- trimws(test2$CK_Key)
test$CK_Rank <- CK_MKT$X1[match(test2$CK_Key,CK_MKT$X2)] #Merge the CK best selling rankings with our original CK Buylist scrape
Low_Confidence_Report <- test
#TCG- Market####
Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")
Updated_Tracking_Keys$Semi <- paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep="")
Start_Time <- Sys.time()
A <- 0
B <- 1000
C <- 1000
TCG__Best_Sellers <- NULL
body <- paste('{
    "algorithm": "salesrel",
    "from": "',A,'",
    "size": "',B,'",
    "filters": {
        "range": {},
        "term": {
            "productLineName": [
                "magic"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Mythic",
                "Uncommon",
                "Rare"
            ]
        }
    }
}',
              sep="")
A <- B + 1
B <- 999
TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(30);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
for(i in 1:C){
  Name <- TCG_Results_1[[1]]$results[[i]]$productName
  Set <- TCG_Results_1[[1]]$results[[i]]$setName
  Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
  MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
  Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
  MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
  Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
  Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID)
  TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
}
repeat{
  body <- paste('{
    "algorithm": "salesrel",
    "from": "',A,'",
    "size": "',B,'",
    "filters": {
        "range": {},
        "term": {
            "productLineName": [
                "magic"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Mythic",
                "Uncommon",
                "Rare"
            ]
        }
    }
}',
                sep="")
  A <- A + 1000
  B <- 999
  C <- 999
  TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
  TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
  repeat{
    if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(30);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
    if((length(TCG_Results_1[[1]]$results) != 0)) break
  }
  for(i in 1:C){
    Name <- TCG_Results_1[[1]]$results[[i]]$productName
    Set <- TCG_Results_1[[1]]$results[[i]]$setName
    Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
    MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
    Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
    MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
    Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
    Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID)
    TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
  }
  if(nrow(TCG__Best_Sellers) >= 9991) break
  Sys.sleep(10)
}
TCG__Best_Sellers <- unique(TCG__Best_Sellers)
TCG__Best_Sellers <- as.data.frame(TCG__Best_Sellers)
TCG__Best_Sellers$Rank <- seq(nrow(TCG__Best_Sellers))
TCG__Best_Sellers$Rarity <- ifelse(TCG__Best_Sellers$Rarity == "Mythic","M", ifelse(TCG__Best_Sellers$Rarity == "Rare", "R", ifelse(TCG__Best_Sellers$Rarity == "Uncommon", "U", ifelse(TCG__Best_Sellers$Rarity == "Common", "C", TCG__Best_Sellers$Rarity))))
TCG__Best_Sellers$Key <- paste(TCG__Best_Sellers$Name,TCG__Best_Sellers$Set,TCG__Best_Sellers$Rarity,sep="")
TCG__Best_Sellers <- TCG__Best_Sellers[moveme(names(TCG__Best_Sellers), "Key first")]
End_Time <- Sys.time()
print(paste("TCG Best Sellers Lasted:",round(End_Time - Start_Time,2)))

#sheets_deauth()
gs4_auth(email = "GMAIL", use_oob = T)
sheet_write(
  TCG__Best_Sellers,
  ss = "/d/1Ef2FgpR-bOg28a8JetHTTIXFH4FeR3eSEj5wpIAVjlU",
  sheet = "TCG_Real_View"
)


TCG <- TCG__Best_Sellers#TCG Formatting####
colnames(TCG) <- c("Primary_Key","Card_Name","Set","Rarity","MKT_EST","Vendor Listings","MKT","Product_ID","Rank")
#TCG <- Results[c(7,2,1,6,4,3,5)]
TCG$MKT_EST <- as.numeric(as.character(TCG$MKT_EST))
#Line 388 is CRUCIAL####
#TCG$MKT <- gsub("[^0-9.-]", 0,TCG$MKT)
TCG$MKT <- as.numeric(as.character(TCG$MKT))
Vendor <- TCG
TCG_Vendor <- as.data.frame(Vendor)
TCG_Vendor$Rank <- seq.int(nrow(TCG_Vendor))
Middle_Confidence_Report <- TCG_Vendor

TCG_Export = TCG
Sets_V2 <- Sets[c(1:5)]
colnames(Sets_V2) <- c("CK_BL_Scrape_Sets","MTG_Goldfish_Sets","GF_Abbr","GF_Abbr_Foil","TCG_Key")
TCG_Export$Set <- Sets_V2$CK_BL_Scrape_Sets[match(TCG$Set,Sets_V2$TCG_Key)]
TCG_Export$Primary_Key <- paste(TCG_Export$Card_Name,TCG_Export$Set, TCG_Export$Rarity, sep = "")
TCG_Export$TCG_Rank <- TCG_Vendor$Rank


test$CK_Key <- trimws(test$CK_Key)
test$TCG_Rank <-TCG_Export$TCG_Rank[match(test$CK_Key,TCG_Export$Primary_Key)]
test$TCG_Price <- TCG_Export$MKT_EST[match(test$CK_Key,TCG_Export$Primary_Key)]
test$Gold_Market[is.na(test$Gold_Market)] <- 0
test$CK_Rank[is.na(test$CK_Rank)] <- ""
test$TCG_Rank[is.na(test$TCG_Rank)] <- ""


Pricing = test[c(1,2,3,4,5,14,6,7,8,9,10,11,12,13,15)]
Pricing = as.data.frame(Pricing)
#Pricing$TCG_Price <- Price_Trim(as.numeric(Pricing$TCG_Price))
Pricing$TCG_Price <- as.numeric(Pricing$TCG_Price)
Pricing$Gold_Market <- as.numeric(Pricing$Gold_Market)
Pricing$MKT_Est <- ifelse(Pricing$`NF/F` == " FOIL", Pricing$Gold_Market,ifelse((is.na(Pricing$TCG_Price)==T),Pricing$Gold_Market,Pricing$TCG_Price))
Pricing1 = Pricing
Pricing1$MKT_Est = as.numeric(Pricing1$MKT_Est)
Pricing1$BL_Value = as.numeric(Pricing1$BL_Value)
Pricing$Arbit <- ifelse(is.na(Pricing1$MKT_Est) != TRUE,(Pricing1$BL_Value - Pricing1$MKT_Est),0)
Pricing$Arbit = round(Pricing$Arbit,2)
Pricing$BL_Value[is.na(Pricing$BL_Value)] <- 0
Pricing$Arbit[is.na(Pricing$Arbit)] <- 0
Pricing_Export <- Pricing[c(2,8,10,11,12,14,6,15,16,3,17,4)]
CK_Prices_df$prices <- as.numeric(as.character(CK_Prices_df$prices))
Pricing_Export$CK_MKT <- CK_Prices_df$prices[match(as.factor(trimws(Pricing_Export$CK_Key)),as.factor(trimws(CK_Prices_df$key)))]
#
Ranking <- Pricing_Export
Ranking$CK_Rank = as.numeric(Ranking$CK_Rank)
Ranking$TCG_Rank = as.numeric(Ranking$TCG_Rank)
Ranking <- Ranking[order(Ranking$CK_Rank),]
#If Weighted and Adj are "Inf" <- change Line 1596 from c(1) to c(2)####

Anchor_CK_price <- Ranking[1,ncol(Ranking)]
#View(Ranking[,(9)])
Ranking$CK_Rank <- round(((Ranking$CK_MKT/Anchor_CK_price)*Ranking$CK_Rank),5)
Ranking <- Ranking[order(-Ranking$CK_Rank),]
Worst_CK_Rank <- (Ranking[1,6])+1
Ranking$CK_Rank <- ifelse(Ranking$CK_Rank == 0,Worst_CK_Rank,Ranking$CK_Rank)
Ranking$CK_Rank[is.na(Ranking$CK_Rank)] <- Worst_CK_Rank
Ranking <- Ranking[order(Ranking$CK_Rank),]
Absolute_CK_Ranking <- Ranking$CK_Rank
Absolute_CK_Ranking <- seq.int(nrow(Ranking))
Ranking$Abs_CK_Rank <- Absolute_CK_Ranking
Ranking1 <- Ranking

Ranking1$TCG_Rank[is.na(Ranking1$TCG_Rank)==T] <- ""
Ranking1$CK_Rank <- as.numeric(Ranking1$CK_Rank)
Ranking1$TCG_Rank <- as.numeric(Ranking1$TCG_Rank)
Ranking1$Weighted_Rank = round(ifelse(((is.na(Ranking1$CK_Rank) != TRUE) & (is.na(Ranking1$TCG_Rank) != TRUE)), (((Ranking1$CK_Rank*.0818)+(Ranking1$TCG_Rank*.5568)/(.5568+.0818))),ifelse(((is.na(Ranking1$CK_Rank) != TRUE) & (is.na(Ranking1$TCG_Rank) = TRUE)),Ranking1$CK_Rank,ifelse(((is.na(Ranking1$CK_Rank) = TRUE) & (is.na(Ranking1$TCG_Rank) != TRUE)),Ranking1$TCG_Rank,40001))),2)

Ranking1 <- Ranking1[order(Ranking1$Weighted_Rank),]

Ranking1$CK_Rank <- Ranking$CK_Rank
Ranking1$TCG_Rank <- Ranking$TCG_Rank
Ranking1 = as.data.frame(Ranking1)
Ranking2 = Ranking1
Ranking2$CK_Rank = as.numeric(Ranking1$CK_Rank)
Ranking2$TCG_Rank = as.numeric(Ranking1$TCG_Rank)
Ranking2$Demand_Pct_Conf = round(ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE) & (Ranking2$CK_Rank < Worst_CK_Rank)), 64,ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE)),56,ifelse((Ranking2$CK_Rank < Worst_CK_Rank),8,0))),0)
Ranking2$CK_Rank <- Ranking$CK_Rank
Ranking2$TCG_Rank <- Ranking$TCG_Rank
Ranking2 <- Ranking2[order(Ranking2$CK_Rank),]

Ranking_Export <- Ranking2[c(1,2,3,4,5,6,14,15,9,10,11,12,7,13,8)]
Ranking_Export$Weighted_Rank <- seq.int(nrow(Ranking_Export))
Ranking_Export$Vendor <- TCG_Export$`Vendor Listings`[match(as.factor(Ranking_Export$CK_Key),as.factor(trimws(TCG_Export$Primary_Key)))]
Ranking_Export$TCG_Rank[is.na(Ranking_Export$TCG_Rank)] <- ""
Ranking_Export$Vendor <- as.character(Ranking_Export$Vendor)
Ranking_Export$Vendor[is.na(Ranking_Export$Vendor)] <- ""
Ranking_Export <- Ranking_Export[c(1,2,3,4,5,6,7,8,9,10,11,12,16,15,14,13)]
Final_Export <- Pricing_Export
Final_Export$Vendor <- Ranking_Export$Vendor[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Weighted_Rank <- Ranking_Export$Weighted_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Adj_CK_Ranking <- Ranking_Export$CK_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Demand_PCT_Conf <-  ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),0,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),8,ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),56,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),64,0))))
Final_Export$CK_MKT <-  Ranking_Export$CK_MKT[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$MKT_Est <- ifelse(Final_Export$MKT_Est == 0.00, Final_Export$CK_MKT, Final_Export$MKT_Est)
Final_Export <- Final_Export[c(1,2,3,4,5,12,10,9,11,14,7,16,15,17)]
names(Final_Export)<- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT","Arb","Sellers","TCG_Rank","CK_ADJ_Rank","OVR_Rank","%_of_Market")
Final_Export$CK_ADJ_Rank <- as.numeric(as.character(Final_Export$CK_ADJ_Rank))
Final_Export$CK_ADJ_Rank <- round(Final_Export$CK_ADJ_Rank,2)
Final_Export <- Final_Export[order(Final_Export$CK_ADJ_Rank),]
Final_Export$CK_ADJ_Rank <- seq.int(nrow(Final_Export))
Final_Export <- Final_Export[order(Final_Export$OVR_Rank),]
Final_Export$OVR_Rank <- seq.int(nrow(Final_Export))
Final_Export$MKT_TS_Single <- round((Final_Export$MKT * 1.08875)+.78,2)
Final_Export$MKT_TS_Set <- round(((Final_Export$MKT * 4)* 1.08875)+.78,2)
Final_Export$`Single_Arb_%` <- round((Final_Export$BL - Final_Export$MKT_TS_Single)/Final_Export$MKT_TS_Single,2)
Final_Export$`Set_Arb_%` <- round(((Final_Export$BL*4) - Final_Export$MKT_TS_Set)/Final_Export$MKT_TS_Set,2)

Final_Export_1 <- Final_Export
Final_Export_1$MKT <- as.numeric(Final_Export_1$MKT)

Final_Export_1 <- Final_Export[c(1,2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,7)]
#seq.int(nrow(CK_MKT))
#Final_Export$BL_Value <- test$BL_Value[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export_1 <- Final_Export_1[order(-Final_Export_1$`Single_Arb_%`),]
Final_Export_1<-subset(Final_Export_1, Final_Export_1$MKT!=0)
Final_Export_1<-subset(Final_Export_1, Final_Export_1$MKT!=0)

#Export My Masterpiece####
setwd("/home/cujo253/Reports/High Confidence Reps")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Premium",".csv",sep="")
write.csv(Final_Export_1, file=csvFileName, row.names = FALSE) 

setwd("/home/cujo253/Reports/TCG Vendor")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_TCG",".csv",sep="")
write.csv(TCG_Vendor, file=csvFileName, row.names = FALSE) 

# setwd("/home/cujo253/Reports/Low Confidence Reps")
# Basic_Market_Review <- Basic_Market_Review[c(1,2,3,4,5,8,7,6)]
# names(Basic_Market_Review)<- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT")
# csvFileName <- paste(currentDate,"_Basic",".csv",sep="")
# write.csv(Basic_Market_Review, file=csvFileName, row.names = FALSE) 


#Funny Money Report####
CK_Prices_df$key <- trimws(CK_Prices_df$key)
CK_Prices_df$TCG_Price <- Final_Export$MKT[match(CK_Prices_df$key,Final_Export$Key)]
CK_Prices_df$TCG_Price <- as.numeric(CK_Prices_df$TCG_Price)
CK_Prices_df$CK_TCG_PCT_DIFF <- as.numeric(as.character(CK_Prices_df$prices))/CK_Prices_df$TCG_Price
CK_Price_Comparison <- CK_Prices_df[c(5,1,2,3,6,4)]

names(CK_Price_Comparison) <- c("Key","Card_Name","Set","Rarity","TCG_Price","CK_Price")

CK_Price_Comparison$TCG_Price[is.na(CK_Price_Comparison$TCG_Price)] <- 1
CK_Price_Comparison$TCG_Price <- ifelse(CK_Price_Comparison$TCG_Price == 0.00,1.00, CK_Price_Comparison$TCG_Price)
CK_Price_Comparison$CK_Price <- as.numeric(as.character(CK_Price_Comparison$CK_Price))
CK_Price_Comparison$Price_Diff <- round((CK_Price_Comparison$CK_Price-CK_Price_Comparison$TCG_Price),2)
CK_Price_Comparison$Price_Diff <- ifelse(CK_Price_Comparison$Price_Diff == CK_Price_Comparison$CK_Price, "Not Captured", CK_Price_Comparison$Price_Diff)
CK_Price_Comparison <- CK_Price_Comparison[which(CK_Price_Comparison$Price_Diff != "Not Captured"),]
CK_Price_Comparison <- CK_Price_Comparison[order(-as.numeric(CK_Price_Comparison$Price_Diff)),]
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
CK_Price_Comparison$Set_Group <- Exclusion$Excl_Excl[match(CK_Price_Comparison$Set,Exclusion$Set_Excl)]
CK_Price_Comparison$CK_BL <- Final_Export$BL[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$CK_BL_Backing <- round((CK_Price_Comparison$CK_BL/CK_Price_Comparison$CK_Price),2)
CK_Price_Comparison$TCG_BL_Backing <- round((CK_Price_Comparison$CK_BL/CK_Price_Comparison$TCG_Price),2)
CK_Price_Comparison$TCG_Vendors <- Final_Export$Sellers[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$BL_Desired_Amt <- Final_Export$BL_QTY[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$`F/NF` <- ""
CK_Price_Comparison <- CK_Price_Comparison[c(1,2,3,4,14,13,9,5,6,7,12,10,11,8)]
names(CK_Price_Comparison) <- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","TCG_MKT","CK_MKT","MKT_Diff","Sellers","CK_MKT_%","TCG_MKT_%","Group")
CK_Price_Excluded_Sets <- CK_Price_Comparison[which(CK_Price_Comparison$Group != "Exclude"),]
CK_Price_Excluded_Sets$TCG_Rank <- Final_Export$TCG_Rank[match(CK_Price_Excluded_Sets$Key,Final_Export$Key)]
CK_Price_Excluded_Sets$CK_Rank <- Final_Export$CK_ADJ_Rank[match(CK_Price_Excluded_Sets$Key,Final_Export$Key)]
#View(CK_Price_Excluded_Sets)
#Final Preparations For Funny Money####
CK_Price_Excluded_Sets[is.na(CK_Price_Excluded_Sets)==TRUE]<-""
CK_Price_Excluded_Sets <- CK_Price_Excluded_Sets[which( (CK_Price_Excluded_Sets$TCG_MKT != 1) & (CK_Price_Excluded_Sets$BL != "")),]
Funny_Money_Analysis <- CK_Price_Excluded_Sets

setwd("/home/cujo253/Funny Money")

csvFileName <- paste(currentDate,"_CK_Credit_Data",".csv",sep="")
write.csv(Funny_Money_Analysis, file=csvFileName, row.names = FALSE) 

setwd("/home/cujo253/Reports/High Confidence Reps")
Final_Export_1$Exclusion <- Exclusion$Excl_Excl[match(Final_Export_1$Set,Exclusion$Set_Excl)]

currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Premium",".csv",sep="")
write.csv(Final_Export_1, file=csvFileName, row.names = FALSE)

#Load in Dated Premium Reports####
#View(Final_Export)
Special_Rep <- Final_Export[c(1,2,3,4,7,15,17,18,10,11,12)]
#View(Special_Rep$Sellers)
YEST <- Sys.Date()-1
WK <- Sys.Date()-7
MTH <- Sys.Date()-30
YEST_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",YEST,"_Premium.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",WK,"_Premium.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",MTH,"_Premium.csv", sep = "")
Yesterday <- read_csv(YEST_BL_Import, 
                      col_types = cols(`F/NF` = col_character(), 
                                       Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Week_Ago <- read_csv(WK_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Month_Ago <-read_csv(MTH_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
#View(Week_Ago)
Yesterday$Sellers <- gsub("[^0-9.-]", 0,Yesterday$Sellers)
Yesterday$Sellers <- as.numeric(Yesterday$Sellers)
Special_Rep$Sellers <- gsub("[^0-9.-]", 0,Special_Rep$Sellers)
Special_Rep$Sellers<- as.numeric(Special_Rep$Sellers)
Week_Ago$Sellers <- gsub("[^0-9.-]", 0,Week_Ago$Sellers)
Week_Ago$Sellers <- as.numeric(Week_Ago$Sellers)
Month_Ago$Sellers <- gsub("[^0-9.-]", 0,Month_Ago$Sellers)
Month_Ago$Sellers <- as.numeric(Month_Ago$Sellers)
Key_Amalgamation <- NULL
Key_Amalgamation <- cbind(Special_Rep$Key, Yesterday$Key)
Key_Amalgamation <- cbind(Key_Amalgamation, Week_Ago$Key)
Key_Amalgamation <- cbind(Key_Amalgamation, Month_Ago$Key)
Key_Amalgamation <- as.data.frame(Key_Amalgamation)
Key_Amalgamation <- reshape2::melt(Key_Amalgamation, id.vars=c(),var='Key')
Unique_Keys <- unique(Key_Amalgamation$value)
Unique_Keys <- as.data.frame(Unique_Keys)

Names <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
Names <- as.data.frame(Names)

Unique_Keys$Name <- Names$name[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Set <- Names$Set[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Rarity <- Names$Rarity[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Foil <- Names$`F/NF`[match(Unique_Keys$Unique_Keys,Names$Key)]
#View(Unique_Keys)
#Growth Reports####
BuyList_Growth <- Unique_Keys
BuyList_Growth$Todays_BL <- Special_Rep$BL[match(BuyList_Growth$Unique_Keys,Special_Rep$Key)]
BuyList_Growth$Yesterday_BL <- Yesterday$BL[match(BuyList_Growth$Unique_Keys,Yesterday$Key)]
BuyList_Growth$Week_Ago_BL <- Week_Ago$BL[match(BuyList_Growth$Unique_Keys,Week_Ago$Key)]
BuyList_Growth$Month_Ago_BL <- Month_Ago$BL[match(BuyList_Growth$Unique_Keys,Month_Ago$Key)]

BuyList_Growth$Yesterday_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Yesterday_BL)/BuyList_Growth$Yesterday_BL,4)
BuyList_Growth$Week_Ago_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Week_Ago_BL)/BuyList_Growth$Week_Ago_BL,4)
BuyList_Growth$Month_Ago_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Month_Ago_BL)/BuyList_Growth$Month_Ago_BL,4)

BuyList_Growth$BuyList_Backing <- Funny_Money_Analysis$CK_MKT[match(BuyList_Growth$Unique_Keys, Funny_Money_Analysis$Key)]
BuyList_Growth$BuyList_Backing <- 1 - round((BuyList_Growth$BuyList_Backing - BuyList_Growth$Todays_BL)/BuyList_Growth$BuyList_Backing,4)

BuyList_Growth[is.na(BuyList_Growth)] <- ""
BuyList_Growth[BuyList_Growth == "Inf"] <- ""

Consistent_BuyLists <- subset(BuyList_Growth, is.na(BuyList_Growth$Todays_BL) != TRUE & is.na(BuyList_Growth$Yesterday_BL) != TRUE & is.na(BuyList_Growth$Week_Ago_BL) != TRUE & is.na(BuyList_Growth$Month_Ago_BL) != TRUE & is.na(BuyList_Growth$Yesterday_BL_Chg) != TRUE & is.na(BuyList_Growth$Week_Ago_BL_Chg) != TRUE & is.na(BuyList_Growth$Month_Ago_BL_Chg) != TRUE)

#View(Consistent_BuyLists)
#View(BuyList_Growth)

YEST <- Sys.Date()-2
WK <- Sys.Date()-8
MTH <- Sys.Date()-30
YEST_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",YEST,"_TCG.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",WK,"_TCG.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",MTH,"_TCG.csv", sep = "")
Yesterday <- read_csv(YEST_BL_Import,col_types = cols(`Vendor Listings` = col_number())) 
Week_Ago <- read_csv(WK_BL_Import,col_types = cols(`Vendor Listings` = col_number()))
Month_Ago <-read_csv(MTH_BL_Import,col_types = cols(`Vendor Listings` = col_number()))


Vendor_Growth <- Unique_Keys
TCG_Vendor$`Vendor Listings` <- gsub("[^0-9.-]", 0,TCG_Vendor$`Vendor Listings`)
Yesterday$`Vendor Listings` <- gsub("[^0-9.-]", 0,Yesterday$`Vendor Listings`)
Week_Ago$`Vendor Listings` <- gsub("[^0-9.-]", 0,Week_Ago$`Vendor Listings`)
Month_Ago$`Vendor Listings` <- gsub("[^0-9.-]", 0,Month_Ago$`Vendor Listings`)

#View(Special_Rep)
Vendor_Growth$Todays_Sellers <- as.numeric(as.character(TCG_Vendor$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,TCG_Vendor$Primary_Key)]))
Vendor_Growth$Yesterday_Sellers <- as.numeric(as.character(Yesterday$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,Yesterday$Primary_Key)]))
Vendor_Growth$Week_Ago_Sellers <- as.numeric(as.character(Week_Ago$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,Week_Ago$Primary_Key)]))
Vendor_Growth$Month_Ago_Sellers <- as.numeric(as.character(Month_Ago$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,Month_Ago$Primary_Key)]))

# Vendor_Growth$Todays_Sellers <- ifelse((Vendor_Growth$Todays_Sellers >= Vendor_Growth$Yesterday_Sellers * 1.2) | (Vendor_Growth$Todays_Sellers <= Vendor_Growth$Yesterday_Sellers * .8), NA, Vendor_Growth$Todays_Sellers)
# Vendor_Growth$Week_Ago_Sellers <- ifelse( (Vendor_Growth$Week_Ago_Sellers <= Vendor_Growth$Month_Ago_Sellers * 1.50 | Vendor_Growth$Week_Ago_Sellers >= Vendor_Growth$Month_Ago_Sellers * .50 ) & (nchar(Vendor_Growth$Week_Ago_Sellers)>2) , Vendor_Growth$Month_Ago_Sellers, Vendor_Growth$Week_Ago_Sellers)
# Vendor_Growth$Month_Ago_Sellers <- ifelse(nchar(Vendor_Growth$Month_Ago_Sellers) < nchar(Vendor_Growth$Week_Ago_Sellers)-2 | nchar(Vendor_Growth$Month_Ago_Sellers) > nchar(Vendor_Growth$Week_Ago_Sellers)+2, NA, Vendor_Growth$Month_Ago_Sellers)
# Vendor_Growth$Month_Ago_Sellers <- ifelse((is.na(Vendor_Growth$Week_Ago_Sellers) == T) & (nchar(Vendor_Growth$Month_Ago_Sellers)+1 == nchar(Vendor_Growth$Yesterday_Sellers) | nchar(Vendor_Growth$Month_Ago_Sellers)-1 == nchar(Vendor_Growth$Yesterday_Sellers ) | nchar(Vendor_Growth$Month_Ago_Sellers) == nchar(Vendor_Growth$Yesterday_Sellers )),Vendor_Growth$Month_Ago_Sellers,NA)
# Vendor_Growth$Month_Ago_Sellers <- ifelse(Vendor_Growth$Month_Ago_Sellers <= .80 * Vendor_Growth$Yesterday_Sellers, NA, Vendor_Growth$Month_Ago_Sellers)


Vendor_Growth$Yesterday_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Yesterday_Sellers)/Vendor_Growth$Yesterday_Sellers,4)*(-1)
Vendor_Growth$Week_Ago_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Week_Ago_Sellers)/Vendor_Growth$Week_Ago_Sellers,4)*(-1)
Vendor_Growth$Month_Ago_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Month_Ago_Sellers)/Vendor_Growth$Month_Ago_Sellers,4)*(-1)

Vendor_Growth$Month_Ago_Sellers_Chg <- ifelse(nchar(Vendor_Growth$Month_Ago_Sellers)>2 & (Vendor_Growth$Month_Ago_Sellers_Chg > .50 | Vendor_Growth$Month_Ago_Sellers_Chg > .50), NA, Vendor_Growth$Month_Ago_Sellers_Chg)
Consistent_Vendors <- Vendor_Growth[c(-5)]
#Consistent_Vendors <- na.omit(Consistent_Vendors)
YEST <- Sys.Date()-1
WK <- Sys.Date()-7
MTH <- Sys.Date()-30
YEST_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",YEST,"_Premium.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",WK,"_Premium.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",MTH,"_Premium.csv", sep = "")
Yesterday <- read_csv(YEST_BL_Import, 
                      col_types = cols(`F/NF` = col_character(), 
                                       Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Week_Ago <- read_csv(WK_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Month_Ago <-read_csv(MTH_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))

TCG_Growth <- Unique_Keys
TCG_Growth$Todays_TCG <- Special_Rep$TCG_Rank[match(TCG_Growth$Unique_Keys,Special_Rep$Key)]
TCG_Growth$Yesterday_TCG <- Yesterday$TCG_Rank[match(TCG_Growth$Unique_Keys,Yesterday$Key)]
TCG_Growth$Week_Ago_TCG <- Week_Ago$TCG_Rank[match(TCG_Growth$Unique_Keys,Week_Ago$Key)]
TCG_Growth$Month_Ago_TCG <- Month_Ago$TCG_Rank[match(TCG_Growth$Unique_Keys,Month_Ago$Key)]

TCG_Growth$Todays_TCG <- as.numeric(TCG_Growth$Todays_TCG)
TCG_Growth$Yesterday_TCG <- as.numeric(TCG_Growth$Yesterday_TCG)
TCG_Growth$Week_Ago_TCG <- as.numeric(TCG_Growth$Week_Ago_TCG)
TCG_Growth$Month_Ago_TCG <- as.numeric(TCG_Growth$Month_Ago_TCG)

TCG_Growth$Yesterday_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Yesterday_TCG)/TCG_Growth$Yesterday_TCG,4)*(-1)
TCG_Growth$Week_Ago_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Week_Ago_TCG)/TCG_Growth$Week_Ago_TCG,4)*(-1)
TCG_Growth$Month_Ago_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Month_Ago_TCG)/TCG_Growth$Month_Ago_TCG,4)*(-1)
Consistent_Sellers <- subset(TCG_Growth, is.na(TCG_Growth$Todays_TCG) != TRUE & is.na(TCG_Growth$Yesterday_TCG) != TRUE & is.na(TCG_Growth$Week_Ago_TCG) != TRUE & is.na(TCG_Growth$Month_Ago_TCG) != TRUE & is.na(TCG_Growth$Yesterday_TCG_Chg) != TRUE & is.na(TCG_Growth$Week_Ago_TCG_Chg) != TRUE & is.na(TCG_Growth$Month_Ago_TCG_Chg) != TRUE)
Consistent_Sellers <- Consistent_Sellers[c(-5)]
#View(TCG_Growth)
#View(Consistent_Vendors)

setwd("/home/cujo253/Reports/Growth Reports")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_C_BuyList_Growth",".csv",sep="")
write.csv(Consistent_BuyLists, file=csvFileName, row.names = FALSE)
csvFileName <- paste(currentDate,"_C_Demand_Growth",".csv",sep="")
write.csv(Consistent_Sellers, file=csvFileName, row.names = FALSE)
csvFileName <- paste(currentDate,"_C_Vendor_Growth",".csv",sep="")
write.csv(Consistent_Vendors, file=csvFileName, row.names = FALSE)
#Today's Buy List Movers####
#View(Consistent_BuyLists)
Consistent_BuyLists$Yesterday_BL_Chg <- as.numeric(Consistent_BuyLists$Yesterday_BL_Chg)
Consistent_BuyLists$Week_Ago_BL_Chg <- as.numeric(Consistent_BuyLists$Week_Ago_BL_Chg)
Consistent_BuyLists$Month_Ago_BL_Chg <- as.numeric(Consistent_BuyLists$Month_Ago_BL_Chg)
Consistent_BuyLists$Todays_BL <- as.numeric(Consistent_BuyLists$Todays_BL)
Consistent_BuyLists$Yesterday_BL <- as.numeric(Consistent_BuyLists$Yesterday_BL )
Consistent_BuyLists$Week_Ago_BL <- as.numeric(Consistent_BuyLists$Week_Ago_BL)
Consistent_BuyLists$Month_Ago_BL <- as.numeric(Consistent_BuyLists$Month_Ago_BL)
Consistent_BuyLists$Foil <- as.factor(Consistent_BuyLists$Foil)

CB_Short <- Consistent_BuyLists[which(Consistent_BuyLists$Yesterday_BL >= 1.25),]
CB_Short <- CB_Short[which(CB_Short$Todays_BL >= 1.25),]
#View(CB_Short)
CB_Short_F <- CB_Short[which(CB_Short$Foil == "FOIL"),]
CB_Short_NF <- CB_Short[which(CB_Short$Foil != "FOIL"),]
CB_Short_NF <- CB_Short_NF[which(CB_Short_NF$BuyList_Backing != "" & CB_Short_NF$BuyList_Backing >= .55 & is.na(CB_Short_NF$Week_Ago_BL_Chg) != TRUE & CB_Short_NF$Week_Ago_BL_Chg > 0 & is.na(CB_Short_NF$Month_Ago_BL_Chg) != TRUE & CB_Short_NF$Month_Ago_BL_Chg > 0),]
CB_Short_F <- CB_Short_F[which(is.na(CB_Short_F$Week_Ago_BL_Chg) != TRUE & CB_Short_F$Week_Ago_BL_Chg > 0 & is.na(CB_Short_F$Month_Ago_BL_Chg) != TRUE & CB_Short_F$Month_Ago_BL_Chg > 0),]
CB_Short_F <- CB_Short_F[order(-CB_Short_F$Yesterday_BL_Chg),]
CB_Short_NF <- CB_Short_NF[order(-CB_Short_NF$Yesterday_BL_Chg),]
#CB_Short_NF$Growth <- ifelse((CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL), "Yes", ifelse(((CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL < CB_Short_NF$Month_Ago_BL)|(CB_Short_NF$Yesterday_BL < CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL)), "Mixed", "No"))
CB_Short_NF$Growth <- ifelse((CB_Short_NF$Todays_BL > CB_Short_NF$Yesterday_BL & CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL), "Yes", ifelse(((CB_Short_NF$Todays_BL < CB_Short_NF$Yesterday_BL & CB_Short_NF$Yesterday_BL < CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL < CB_Short_NF$Month_Ago_BL)), "No", "Mixed"))
CB_Short_NF$Highest <- ifelse(CB_Short_NF$Todays_BL >= CB_Short_NF$Yesterday_BL & CB_Short_NF$Todays_BL >= CB_Short_NF$Week_Ago_BL & CB_Short_NF$Todays_BL >= CB_Short_NF$Month_Ago_BL , "Today", ifelse(CB_Short_NF$Yesterday_BL >= CB_Short_NF$Todays_BL & CB_Short_NF$Yesterday_BL >= CB_Short_NF$Week_Ago_BL & CB_Short_NF$Yesterday_BL >= CB_Short_NF$Month_Ago_BL, "Yesterday",ifelse(CB_Short_NF$Week_Ago_BL >= CB_Short_NF$Todays_BL & CB_Short_NF$Week_Ago_BL >= CB_Short_NF$Yesterday_BL & CB_Short_NF$Week_Ago_BL >= CB_Short_NF$Month_Ago_BL, "Week_Ago",ifelse(CB_Short_NF$Month_Ago_BL >= CB_Short_NF$Todays_BL & CB_Short_NF$Month_Ago_BL >= CB_Short_NF$Yesterday_BL & CB_Short_NF$Month_Ago_BL >= CB_Short_NF$Week_Ago_BL,"Month_Ago","")))) 
#View(CB_Short_NF)
setwd("/home/cujo253/Reports/Growth Reports")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Refined_BuyList_Growth",".csv",sep="")
write.csv(CB_Short_NF, file=csvFileName, row.names = FALSE)

CB_Short_F$Growth <- ifelse((CB_Short_F$Yesterday_BL > CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL > CB_Short_F$Month_Ago_BL), "Yes", ifelse(((CB_Short_F$Yesterday_BL > CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL < CB_Short_F$Month_Ago_BL)|(CB_Short_F$Yesterday_BL < CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL > CB_Short_F$Month_Ago_BL)), "Mixed", "No"))
CB_Short_F$Growth <- ifelse((CB_Short_F$Todays_BL > CB_Short_F$Yesterday_BL & CB_Short_F$Yesterday_BL > CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL > CB_Short_F$Month_Ago_BL), "Yes", ifelse(((CB_Short_F$Todays_BL < CB_Short_F$Yesterday_BL & CB_Short_F$Yesterday_BL < CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL < CB_Short_F$Month_Ago_BL)), "No", "Mixed"))
CB_Short_F$Highest <- ifelse(CB_Short_F$Todays_BL >= CB_Short_F$Yesterday_BL & CB_Short_F$Todays_BL >= CB_Short_F$Week_Ago_BL & CB_Short_F$Todays_BL >= CB_Short_F$Month_Ago_BL , "Today", ifelse(CB_Short_F$Yesterday_BL >= CB_Short_F$Todays_BL & CB_Short_F$Yesterday_BL >= CB_Short_F$Week_Ago_BL & CB_Short_F$Yesterday_BL >= CB_Short_F$Month_Ago_BL, "Yesterday",ifelse(CB_Short_F$Week_Ago_BL >= CB_Short_F$Todays_BL & CB_Short_F$Week_Ago_BL >= CB_Short_F$Yesterday_BL & CB_Short_F$Week_Ago_BL >= CB_Short_F$Month_Ago_BL, "Week_Ago",ifelse(CB_Short_F$Month_Ago_BL >= CB_Short_F$Todays_BL & CB_Short_F$Month_Ago_BL >= CB_Short_F$Yesterday_BL & CB_Short_F$Month_Ago_BL >= CB_Short_F$Week_Ago_BL,"Month_Ago","")))) 
YEST <- Sys.Date()-1
WK <- Sys.Date()-7
MTH <- Sys.Date()-30

YEST_BL_Import <- paste("/home/cujo253/Funny Money/",YEST,"_CK_Credit_Data.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Funny Money/",WK,"_CK_Credit_Data.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Funny Money/",MTH,"_CK_Credit_Data.csv", sep = "")

Yesterday <- read_csv(YEST_BL_Import, col_types = cols(BL = col_number(), BL_QTY = col_character(), CK_MKT = col_number(), `CK_MKT_%` = col_number(), `F/NF` = col_character(), MKT_Diff = col_number(), Sellers = col_number(), TCG_MKT = col_number(), `TCG_MKT_%` = col_number()))
Week_Ago <- read_csv(WK_BL_Import, col_types = cols(BL = col_number(), BL_QTY = col_character(), CK_MKT = col_number(), `CK_MKT_%` = col_number(), `F/NF` = col_character(), MKT_Diff = col_number(), Sellers = col_number(), TCG_MKT = col_number(), `TCG_MKT_%` = col_number()))
Month_Ago <- read_csv(MTH_BL_Import, col_types = cols(BL = col_number(), BL_QTY = col_character(), CK_MKT = col_number(), `CK_MKT_%` = col_number(), `F/NF` = col_character(), MKT_Diff = col_number(), Sellers = col_number(), TCG_MKT = col_number(), `TCG_MKT_%` = col_number()))

CB_Short_NF$BuyList_Backing <- as.numeric(CB_Short_NF$BuyList_Backing)
CB_Short_NF$Todays_BL_Accel <- round((CB_Short_NF$BuyList_Backing - Yesterday$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Yesterday$Key)])/Yesterday$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Yesterday$Key)],2)
CB_Short_NF$Yesterday_BL_Accel <- round((Yesterday$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Yesterday$Key)] - Week_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)])/Week_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)],2)
CB_Short_NF$Week_Ago_BL_Accel <- round((Week_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)] - Month_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)])/Month_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)],2)

CB_Short_NF$CK_MKT <- Funny_Money_Analysis$CK_MKT[match(CB_Short_NF$Unique_Keys,Funny_Money_Analysis$Key)]
CB_Short_NF$Yest_CK_MKT <- Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)]
CB_Short_NF$Week_CK_MKT <- Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)]
CB_Short_NF$Month_CK_MKT <- Month_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)]

CB_Short_NF$Todays_MKT_Accel <- round((CB_Short_NF$CK_MKT - Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)])/Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)],2)
CB_Short_NF$Yesterday_MKT_Accel <- round((Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)] - Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)])/Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)],2)
CB_Short_NF$Week_Ago_MKT_Accel <- round((Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)] - Month_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)])/Month_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)],2)

CB_CK_Final <- CB_Short_NF[which(CB_Short_NF$Growth != "Mixed" & CB_Short_NF$Highest == "Today"),]
CB_CK_Final$Growth <- ifelse((CB_CK_Final$CK_MKT > CB_CK_Final$Yest_CK_MKT & CB_CK_Final$Yest_CK_MKT > CB_CK_Final$Week_CK_MKT & CB_CK_Final$Week_CK_MKT > CB_CK_Final$Month_CK_MKT), "Yes", ifelse(((CB_CK_Final$CK_MKT < CB_CK_Final$Yest_CK_MKT & CB_CK_Final$Yest_CK_MKT < CB_CK_Final$Week_CK_MKT & CB_CK_Final$Week_CK_MKT < CB_CK_Final$Month_CK_MKT)), "No", "Mixed"))

Stringent_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0 & CB_CK_Final$Week_Ago_BL_Accel > 0 & CB_CK_Final$Growth == "Yes"),]
Goldilocks_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0 & CB_CK_Final$Growth == "Yes"),]
Relaxed_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0),]

setwd("/home/cujo253/Reports/A_Game/Strict")
csvFileName <- paste(currentDate,"_Strict",".csv",sep="")
write.csv(Stringent_CB_CK_Final, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/A_Game/Goldilocks")
csvFileName <- paste(currentDate,"_Goldilocks",".csv",sep="")
write.csv(Goldilocks_CB_CK_Final, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/A_Game/Relaxed")
csvFileName <- paste(currentDate,"_Relaxed",".csv",sep="")
write.csv(Relaxed_CB_CK_Final, file=csvFileName, row.names = FALSE)

#Reduce & Add TCG to Top Movers####
Stringent_TCG <- Stringent_CB_CK_Final[,c(1,2,3,4,5,6,12,13,19,22)]
Goldilocks_TCG <- Goldilocks_CB_CK_Final[,c(1,2,3,4,5,6,12,13,19,22)]
Relaxed_TCG <- Relaxed_CB_CK_Final[,c(1,2,3,4,5,6,12,13,19,22)]
YEST <- Sys.Date()-2
WK <- Sys.Date()-7
MTH <- Sys.Date()-30
YEST_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",YEST,"_TCG.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",WK,"_TCG.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",MTH,"_TCG.csv", sep = "")
Yesterday <- read_csv(YEST_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))
Week_Ago <- read_csv(WK_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))
Month_Ago <-read_csv(MTH_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))

Special_Rep$Sellers <- as.numeric(Special_Rep$Sellers)
TCG_Vendor$Primary_Key <- as.character(TCG_Vendor$Primary_Key)
TCG_Vendor$Primary_Key <- trimws(TCG_Vendor$Primary_Key)
TCG_Vendor$Primary_Key <- as.factor(TCG_Vendor$Primary_Key)

Goldilocks_TCG$Vendors <- Special_Rep$Sellers[match(Goldilocks_TCG$Unique_Keys,Special_Rep$Key)]
Goldilocks_TCG$Month_Vendors <- Month_Ago$`Vendor Listings`[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Primary_Key)]
Goldilocks_TCG$TCG_Demand <- Special_Rep$TCG_Rank[match(Goldilocks_TCG$Unique_Keys,Special_Rep$Key)]
Goldilocks_TCG$Month_TCG_Demand <-Month_Ago$Rank[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Primary_Key)]
Goldilocks_TCG$CK_Demand <- Special_Rep$CK_ADJ_Rank[match(Goldilocks_TCG$Unique_Keys,Special_Rep$Key)]
MTH_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",MTH,"_Premium.csv", sep = "")
Month_Ago <-read_csv(MTH_BL_Import, col_types = cols(`F/NF` = col_character(), Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
#View(Month_Ago)
Goldilocks_TCG$Month_CK_Demand <- Month_Ago$CK_ADJ_Rank[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Key)]
MTH_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",MTH,"_TCG.csv", sep = "")
Month_Ago <-read_csv(MTH_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))
Goldilocks_TCG$TCG_MKT <- Final_Export$MKT[match(Goldilocks_TCG$Unique_Keys, Final_Export$Key)]
Goldilocks_TCG$Month_TCG_Market <- Month_Ago$MKT_EST[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Primary_Key)]
Goldilocks_Final <- Goldilocks_TCG[,c(1,2,3,4,5,6,8,7,9)]
Goldilocks_Final$CK_MKT_MoM <- Goldilocks_TCG$CK_MKT - Goldilocks_TCG$Month_CK_MKT
Goldilocks_Final$TCG_MKT <- Goldilocks_TCG$TCG_MKT
Goldilocks_Final$TCG_MoM <- Goldilocks_TCG$TCG_MKT - Goldilocks_TCG$Month_TCG_Market
Goldilocks_Final$CK_DMD <- Goldilocks_TCG$CK_Demand
Goldilocks_Final$CK_DMD_MoM <- (Goldilocks_TCG$CK_Demand - Goldilocks_TCG$Month_CK_Demand)*-1
Goldilocks_Final$TCG_DMD <- Goldilocks_TCG$TCG_Demand
Goldilocks_TCG$TCG_Demand <- as.numeric(Goldilocks_TCG$TCG_Demand)
Goldilocks_TCG$Month_TCG_Market <- as.numeric(Goldilocks_TCG$Month_TCG_Demand)
Goldilocks_Final$TCG_DMD_MoM <- (Goldilocks_TCG$TCG_Demand - Goldilocks_TCG$Month_TCG_Demand)*-1
Goldilocks_Final$Vendors <- Goldilocks_TCG$Vendors
Goldilocks_Final$Vendor_MoM <- Goldilocks_TCG$Vendors - Goldilocks_TCG$Month_Vendors


#Google Components####
library(devtools)
#devtools::install_github("tidyverse/googlesheets4", force = TRUE)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "GMAIL")

drive_auth(email = "GMAIL")
drive_auth(use_oob=TRUE)

# my_dfs <- list(Final_Export_1, Funny_Money_Analysis )
# sheets_deauth()
# sheets_auth()
# sheets_create(
#   paste(currentDate,"_Market_Review",sep=""),
#   sheets = my_dfs
# )
ss <- drive_get("Market_Review")
#sheets_deauth()
gs4_auth(email = "GMAIL")
sheet_write(
  Final_Export_1,
  ss = ss,
  sheet = "TCG_Market_Angle"
)

sheet_write(
  Funny_Money_Analysis,
  ss = ss,
  sheet = "Funny_Money_Conversion"
)


CB_Short <- CB_Short[which(CB_Short$Name != ""),]
# Consistent_Vendors<- Consistent_Vendors[which(as.numeric(as.character(Consistent_Vendors$Yesterday_Sellers_Chg)) < .15),]
# Consistent_Vendors<- Consistent_Vendors[which(as.numeric(as.character(Consistent_Vendors$Yesterday_Sellers_Chg)) > (-.15)),]
CB_Short <- CB_Short[order(-CB_Short$Yesterday_BL_Chg),]
Consistent_Sellers <- Consistent_Sellers[order(-Consistent_Sellers$Yesterday_TCG_Chg),]
Consistent_Vendors <- Consistent_Vendors[order(-Consistent_Vendors$Yesterday_Sellers_Chg),]

ss <- drive_get("Growth_Reports")
#sheets_deauth()
sheet_write(
  as.data.frame(CB_Short),
  ss = ss,
  sheet = "BL"
)

sheet_write(
  as.data.frame(Consistent_Sellers),
  ss = ss,
  sheet = "Demand"
)

sheet_write(
  as.data.frame(Consistent_Vendors),
  ss = ss,
  sheet = "Vendors"
)


# Refined <- list(Relaxed_CB_CK_Final,Goldilocks_CB_CK_Final, Stringent_CB_CK_Final)
# sheets_create(
#   paste(currentDate,"_Wolfs_Warrens",sep=""),
#   sheets = Refined
# )

ss <- drive_get("Wolfs_Warrens")
#sheets_deauth()
sheet_write(
  Relaxed_CB_CK_Final,
  ss = ss,
  sheet = "Relaxed"
)
sheet_write(
  Goldilocks_CB_CK_Final,
  ss = ss,
  sheet = "Goldilocks"
)
sheet_write(
  Stringent_CB_CK_Final,
  ss = ss,
  sheet = "Stringent"
)



ss <- drive_get("Bills & MTG 2020")
#sheets_deauth()
sheet_write(
  Final_Export_1,
  ss = ss,
  sheet = "BL"
)

#Metric Aggregation####
currentDate <- Sys.Date()
YesterdayDate <- Sys.Date()-1
Yesterdays_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"BuyList_History.csv", sep ="")
Yesterdays_Vendor_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"Vendor_History.csv", sep ="")
Yesterdays_TCG_Ranks <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"TCG_History.csv", sep ="")
Yesterdays_CK_Ranks <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"CK_History.csv", sep ="")

Buylist_Tracker <- read_csv(Yesterdays_Buylist_Tracker, col_types = cols(.default = "c"))
Vendor_Tracker <- read_csv(Yesterdays_Vendor_Tracker,col_types = cols(.default = "c"))
#Vendor_Tracker[6:ncol(Vendor_Tracker)] <- lapply(Vendor_Tracker[6:ncol(Vendor_Tracker)], factor)
TCG_Ranks <- read_csv(Yesterdays_TCG_Ranks,col_types = cols(.default = "c"))
CK_Ranks <- read_csv(Yesterdays_CK_Ranks,col_types = cols(.default = "c"))


TodaysPremium <- paste("/home/cujo253/Reports/High Confidence Reps/",currentDate,"_Premium.csv", sep="")

Data <- read_csv(TodaysPremium,col_types = cols(.default = "c"))

# bq_auth(email = "wolfoftinstreet@gmail.com", use_oob = TRUE)
# Title_Date <- gsub("\\-","\\_",currentDate)
# tmp <- Data[1:12]
# colnames(tmp)[5] <- "Foil_Status"
# mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste(Title_Date,"_premium",sep=""))
# bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")



Buylist_Tracker$V1 <- Data$BL[match(Buylist_Tracker$Key,Data$Key)]
Vendor_Tracker$V1 <- Data$Sellers[match(Buylist_Tracker$Key,Data$Key)]
TCG_Ranks$V1 <- Data$TCG_Rank[match(Buylist_Tracker$Key,Data$Key)]
CK_Ranks$V1 <- Data$CK_ADJ_Rank[match(Buylist_Tracker$Key,Data$Key)]

formatted_date <- format(currentDate, format = "%Y-%m-%d")

names(Buylist_Tracker)[ncol(Buylist_Tracker)] <- formatted_date
names(Vendor_Tracker)[ncol(Vendor_Tracker)] <- formatted_date
names(TCG_Ranks)[ncol(TCG_Ranks)] <- formatted_date
names(CK_Ranks)[ncol(CK_Ranks)] <- formatted_date


Buylist_Tracker[is.na(Buylist_Tracker)] <- ""
Vendor_Tracker[is.na(Vendor_Tracker)] <- ""
TCG_Ranks[is.na(TCG_Ranks)] <- ""
CK_Ranks[is.na(CK_Ranks)] <- ""


Buylist_Tracker$Foil <- ifelse(Buylist_Tracker$Foil == "FOIL", Buylist_Tracker$Foil, "")
Vendor_Tracker$Foil <- ifelse(Vendor_Tracker$Foil == "FOIL", Vendor_Tracker$Foil, "")
TCG_Ranks$Foil <- ifelse(TCG_Ranks$Foil == "FOIL", TCG_Ranks$Foil, "")
CK_Ranks$Foil <- ifelse(CK_Ranks$Foil == "FOIL", CK_Ranks$Foil, "")


setwd("/home/cujo253/Metrics/TBD Updated Roster")
csvFileName <- paste(currentDate,"BuyList_History",".csv",sep="")
write.csv(Buylist_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"Vendor_History",".csv",sep="")
write.csv(Vendor_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"TCG_History",".csv",sep="")
write.csv(TCG_Ranks, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"CK_History",".csv",sep="")
write.csv(CK_Ranks, file=csvFileName, row.names = FALSE) 

MBT <- Buylist_Tracker[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}

New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,1,ifelse(New[,c(2:ncol(New))]<0,-1,0))

Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key

Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
Three_Weeks_Binary <- Binary_Form[,c((ncol(Binary_Form)-21):ncol(Binary_Form))]
SPS_21 <- rowSums(Three_Weeks_Binary)
SPS_21 <- as.data.frame(SPS_21)

Fifteen_Days_Binary <- Binary_Form[,c((ncol(Binary_Form)-15):ncol(Binary_Form))]
SPS_15 <- rowSums(Fifteen_Days_Binary)
SPS_15 <- as.data.frame(SPS_15)

Seven_Day_Binary <- Binary_Form[,c((ncol(Binary_Form)-7):ncol(Binary_Form))]
SPS_7 <- rowSums(Seven_Day_Binary)
SPS_7 <- as.data.frame(SPS_7)


bl_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
bl_metrics$Key <- Buylist_Tracker$Key
bl_metrics <- bl_metrics[moveme(names(bl_metrics), "Key first")]
tbl_metrics <- t(bl_metrics)
tbl_metrics <- as.data.frame(tbl_metrics)

tbl_metrics <- as.data.frame(tbl_metrics)
tbl_numbers <- (tbl_metrics[-1,])
tbl_numbers <- lapply(tbl_numbers,as.character)
tbl_numbers <- lapply(tbl_numbers,as.numeric)
tbl_numbers <- data.frame(tbl_numbers)
why <- colSums(tbl_numbers)

BL_Final <- data.frame(Buylist_Tracker[,1:5],bl_metrics$SPS_21.SPS_21,bl_metrics$SPS_15.SPS_15,bl_metrics$SPS_7.SPS_7,why)
colnames(BL_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
BL_Final <- BL_Final[order(-BL_Final$Rank_Sums),]
BL_Final_M <- BL_Final[which(BL_Final$Rarity == "M"),]
BL_Final_R <- BL_Final[which(BL_Final$Rarity == "R"),]
BL_Final_U <- BL_Final[which(BL_Final$Rarity == "U"),]
BL_Final_C <- BL_Final[which(BL_Final$Rarity == "C"),]


MBT <-Vendor_Tracker[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
Sys.sleep(sample(1:3, 1))
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key
#csvFileName <- paste(currentDate,"Binary_Form",".csv",sep="")
#write.csv(Binary_Form, file=csvFileName, row.names = FALSE)
Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
Three_Weeks_Binary <- Binary_Form[,c((ncol(Binary_Form)-21):ncol(Binary_Form))]
SPS_21 <- rowSums(Three_Weeks_Binary)
SPS_21 <- as.data.frame(SPS_21)

Fifteen_Days_Binary <- Binary_Form[,c((ncol(Binary_Form)-15):ncol(Binary_Form))]
SPS_15 <- rowSums(Fifteen_Days_Binary)
SPS_15 <- as.data.frame(SPS_15)

Seven_Day_Binary <- Binary_Form[,c((ncol(Binary_Form)-7):ncol(Binary_Form))]
SPS_7 <- rowSums(Seven_Day_Binary)
SPS_7 <- as.data.frame(SPS_7)

ven_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
ven_metrics$Key <- Vendor_Tracker$Key
ven_metrics <- ven_metrics[moveme(names(ven_metrics), "Key first")]
tven_metrics <- t(ven_metrics)
tven_metrics <- as.data.frame(tven_metrics)
tven_metrics <- as.data.frame(tven_metrics)
tven_numbers <- (tven_metrics[-1,])
tven_numbers <- lapply(tven_numbers,as.character)
tven_numbers <- lapply(tven_numbers,as.numeric)
tven_numbers <- data.frame(tven_numbers)

why <- colSums(tven_numbers)

VEN_Final <- data.frame(Vendor_Tracker[,1:5],ven_metrics$SPS_21.SPS_21,ven_metrics$SPS_15.SPS_15,ven_metrics$SPS_7.SPS_7,why)
colnames(VEN_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
VEN_Final <- VEN_Final[order(-VEN_Final$Rank_Sums),]
VEN_Final_M <- VEN_Final[which(VEN_Final$Rarity == "M"),]
VEN_Final_R <- VEN_Final[which(VEN_Final$Rarity == "R"),]
VEN_Final_U <- VEN_Final[which(VEN_Final$Rarity == "U"),]
VEN_Final_C <- VEN_Final[which(VEN_Final$Rarity == "C"),]

MBT <- TCG_Ranks[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}

New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key

Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
Three_Weeks_Binary <- Binary_Form[,c((ncol(Binary_Form)-21):ncol(Binary_Form))]
SPS_21 <- rowSums(Three_Weeks_Binary)
SPS_21 <- as.data.frame(SPS_21)

Fifteen_Days_Binary <- Binary_Form[,c((ncol(Binary_Form)-15):ncol(Binary_Form))]
SPS_15 <- rowSums(Fifteen_Days_Binary)
SPS_15 <- as.data.frame(SPS_15)

Seven_Day_Binary <- Binary_Form[,c((ncol(Binary_Form)-7):ncol(Binary_Form))]
SPS_7 <- rowSums(Seven_Day_Binary)
SPS_7 <- as.data.frame(SPS_7)

tcg_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
tcg_metrics$Key <- TCG_Ranks$Key
tcg_metrics <- bl_metrics[moveme(names(bl_metrics), "Key first")]
ttcg_metrics <- t(bl_metrics)
ttcg_metrics <- as.data.frame(ttcg_metrics)
ttcg_metrics <- as.data.frame(ttcg_metrics)
ttcg_numbers <- (ttcg_metrics[-1,])
ttcg_numbers <- lapply(ttcg_numbers,as.character)
ttcg_numbers <- lapply(ttcg_numbers,as.numeric)
ttcg_numbers <- data.frame(ttcg_numbers)
why <- colSums(ttcg_numbers)

TCG_Final <- data.frame(TCG_Ranks[,1:5],tcg_metrics$SPS_21.SPS_21,tcg_metrics$SPS_15.SPS_15,tcg_metrics$SPS_7.SPS_7,why)
colnames(TCG_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
TCG_Final <- TCG_Final[order(-TCG_Final$Rank_Sums),]
TCG_Final_M <- TCG_Final[which(TCG_Final$Rarity == "M"),]
TCG_Final_R <- TCG_Final[which(TCG_Final$Rarity == "R"),]
TCG_Final_U <- TCG_Final[which(TCG_Final$Rarity == "U"),]
TCG_Final_C <- TCG_Final[which(TCG_Final$Rarity == "C"),]

MBT <-  CK_Ranks[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key

Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
Three_Weeks_Binary <- Binary_Form[,c((ncol(Binary_Form)-21):ncol(Binary_Form))]
SPS_21 <- rowSums(Three_Weeks_Binary)
SPS_21 <- as.data.frame(SPS_21)

Fifteen_Days_Binary <- Binary_Form[,c((ncol(Binary_Form)-15):ncol(Binary_Form))]
SPS_15 <- rowSums(Fifteen_Days_Binary)
SPS_15 <- as.data.frame(SPS_15)

Seven_Day_Binary <- Binary_Form[,c((ncol(Binary_Form)-7):ncol(Binary_Form))]
SPS_7 <- rowSums(Seven_Day_Binary)
SPS_7 <- as.data.frame(SPS_7)

ck_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
ck_metrics$Key <- CK_Ranks$Key
ck_metrics <- ck_metrics[moveme(names(ck_metrics), "Key first")]
tck_metrics <- t(ck_metrics)
tck_metrics <- as.data.frame(tck_metrics)
tck_metrics <- as.data.frame(tck_metrics)
tck_numbers <- (tck_metrics[-1,])
tck_numbers <- lapply(tck_numbers,as.character)
tck_numbers <- lapply(tck_numbers,as.numeric)
tck_numbers <- data.frame(tck_numbers)
why <- colSums(tck_numbers)

CK_Final <- data.frame(CK_Ranks[,1:5],ck_metrics$SPS_21.SPS_21,ck_metrics$SPS_15.SPS_15,ck_metrics$SPS_7.SPS_7,why)
colnames(CK_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
CK_Final <- CK_Final[order(-CK_Final$Rank_Sums),]
CK_Final_M <- CK_Final[which(CK_Final$Rarity == "M"),]
CK_Final_R <- CK_Final[which(CK_Final$Rarity == "R"),]
CK_Final_U <- CK_Final[which(CK_Final$Rarity == "U"),]
CK_Final_C <- CK_Final[which(CK_Final$Rarity == "C"),]


BL_Final$Rank_Groups <- as.numeric(as.factor(BL_Final$Rank_Sums))
VEN_Final$Rank_Groups <- as.numeric(as.factor(VEN_Final$Rank_Sums))
TCG_Final$Rank_Groups <- as.numeric(as.factor(TCG_Final$Rank_Sums))
CK_Final$Rank_Groups <- as.numeric(as.factor(CK_Final$Rank_Sums))

BL_Upper_Esch <- BL_Final[which(BL_Final$Rank_Groups >= (max(BL_Final$Rank_Groups)-9)),]
VEN_Upper_Esch <- VEN_Final[which(VEN_Final$Rank_Groups >= (max(VEN_Final$Rank_Groups)-9)),]
TCG_Upper_Esch <- TCG_Final[which(TCG_Final$Rank_Groups >= (max(TCG_Final$Rank_Groups)-9)),]
CK_Upper_Esch <- CK_Final[which(CK_Final$Rank_Groups >= (max(CK_Final$Rank_Groups)-9)),]

Combined_Upper_Esch <- rbind(BL_Upper_Esch[,1:5], VEN_Upper_Esch[,1:5], TCG_Upper_Esch[,1:5], CK_Upper_Esch[,1:5])
Combined_Upper_Esch[,5][is.na(Combined_Upper_Esch[,5])] <- ""
Unique_Combined_Upper_Esch <- unique(Combined_Upper_Esch)
#View(Unique_Combined_Upper_Esch)
nrow(Combined_Upper_Esch)
nrow(Unique_Combined_Upper_Esch)

CUE <- NULL
BLUE <- NULL
VENUE <- NULL
TCGUE <- NULL
CKUE <- NULL
library(dplyr)
CUE <- Combined_Upper_Esch %>% group_by(Key) %>% add_tally()
BLUE <- BL_Upper_Esch %>% group_by(Key) %>% add_tally()
VENUE <- VEN_Upper_Esch %>% group_by(Key) %>% add_tally()
TCGUE <- TCG_Upper_Esch %>% group_by(Key) %>% add_tally()
CKUE <- CK_Upper_Esch %>% group_by(Key) %>% add_tally()

Unique_Combined_Upper_Esch$Total_KPI_CT <- CUE$n[match(Unique_Combined_Upper_Esch$Key,CUE$Key)]
Unique_Combined_Upper_Esch$BL_KPI <- BLUE$n[match(Unique_Combined_Upper_Esch$Key,BLUE$Key)]
Unique_Combined_Upper_Esch$VEN_KPI <- VENUE$n[match(Unique_Combined_Upper_Esch$Key,VENUE$Key)]
Unique_Combined_Upper_Esch$TCG_KPI <- TCGUE$n[match(Unique_Combined_Upper_Esch$Key,TCGUE$Key)]
Unique_Combined_Upper_Esch$CK_KPI <- CKUE$n[match(Unique_Combined_Upper_Esch$Key,CKUE$Key)]

Unique_Combined_Upper_Esch$BL_Bracket <- BL_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,BL_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$VEN_Bracket <- VEN_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,VEN_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$TCG_Bracket <- TCG_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,TCG_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$CK_Bracket <- CK_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,CK_Upper_Esch$Key)]

Unique_Combined_Upper_Esch$BL_Bracket <- ifelse(Unique_Combined_Upper_Esch$BL_Bracket == max(BL_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$VEN_Bracket <- ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == max(VEN_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$TCG_Bracket <- ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == max(TCG_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$CK_Bracket <- ifelse(Unique_Combined_Upper_Esch$CK_Bracket == max(CK_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-10),10,""))))))))))

Buylist_Tiers <- max(BL_Final$Rank_Groups)
Vendor_Tiers <- max(VEN_Final$Rank_Groups)
TCG_Demand_Tiers <- max(TCG_Final$Rank_Groups)
CK_Demand_Tiers <- max(CK_Final$Rank_Groups)


Unique_Combined_Upper_Esch[,c(11:14)][is.na(Unique_Combined_Upper_Esch[c(11:14)])] <- 10
ncol(Unique_Combined_Upper_Esch)
Unique_Combined_Upper_Esch[is.na(Unique_Combined_Upper_Esch)] <- ""
library(magrittr)
Unique_Combined_Upper_Esch$WMS <-   Unique_Combined_Upper_Esch[,c(11:14)] %>% 
  rowwise() %>% # compute for each row
  do(data.frame(
    WMS=weighted.mean(
      x=c(as.numeric(.$BL_Bracket),as.numeric(.$VEN_Bracket),as.numeric(.$TCG_Bracket),as.numeric(.$CK_Bracket)),
      w=c(.35,.57,.05,.03)
    )
  )
  ) %>% 
  ungroup() %>% # undo row groups
  use_series("WMS")

Unique_Combined_Upper_Esch <- Unique_Combined_Upper_Esch[order(Unique_Combined_Upper_Esch$WMS),]
Unique_Combined_Upper_Esch$Ranking <- seq.int(nrow(Unique_Combined_Upper_Esch))
OVR_KPI_DF <- data.frame(Unique_Combined_Upper_Esch[,1:5],Unique_Combined_Upper_Esch[,16])
colnames(OVR_KPI_DF) <- c("Key","Name","Set","Rarity","F/NF","Ranking")
OVR_KPI_DF$Retail <- Data$MKT[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF$Buylist <- Data$BL[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF$Vendors <- Data$Sellers[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF[is.na(OVR_KPI_DF)] <- ""
M_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "M"),]
R_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "R"),]
U_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "U"),]
C_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "C"),]
M_KPI$Ranking <- seq.int(nrow(M_KPI))
R_KPI$Ranking <- seq.int(nrow(R_KPI))
U_KPI$Ranking <- seq.int(nrow(U_KPI))
C_KPI$Ranking <- seq.int(nrow(C_KPI))
nrow(M_KPI)
nrow(R_KPI)
nrow(U_KPI)
nrow(C_KPI)

#View(OVR_KPI_DF)
setwd("/home/cujo253/Reports/KPI/Master")
csvFileName <- paste(currentDate,"_Master_KPI",".csv",sep="")
write.csv(OVR_KPI_DF, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Mythic")
csvFileName <- paste(currentDate,"_Mythic_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Rare")
csvFileName <- paste(currentDate,"_Rare_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Uncommon")
csvFileName <- paste(currentDate,"_Uncommon_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Common")
csvFileName <- paste(currentDate,"_Common_KPI",".csv",sep="")
write.csv(C_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/")


library(devtools)
#install.packages("googlesheets4")
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "GMAIL")
# gs4_auth(token = drive_token())

drive_auth(email = "GMAIL", use_oob=TRUE)
gs4_auth(email = "GMAIL", use_oob=TRUE)


# MKPI <- list(OVR_KPI_DF,M_KPI,R_KPI,U_KPI,C_KPI)
# gs4_auth()
# sheets_create(
#   paste(currentDate,"_Master_KPI_Review",sep=""),
#   sheets = MKPI
# )
ss <- drive_get("Master_KPI_Review")
#sheets_deauth()
gs4_auth(email = "GMAIL")
sheet_write(
  OVR_KPI_DF,
  ss = ss,
  sheet = "Master"
)
sheet_write(
  M_KPI,
  ss = ss,
  sheet = "Mythics"
)
sheet_write(
  R_KPI,
  ss = ss,
  sheet = "Rares"
)
sheet_write(
  U_KPI,
  ss = ss,
  sheet = "Uncommons"
)
sheet_write(
  C_KPI,
  ss = ss,
  sheet = "Commons"
)


#CK vs TCG Dollar Differences####
Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")
Mystery_Booster_Reprint <- Updated_Tracking_Keys[which(Updated_Tracking_Keys$Set == "Mystery Booster"),]

setwd("/home/cujo253/Funny Money/")
currentDate <- Sys.Date()
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
CK_Market__Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$CK_MKT[match(CK_Market__Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  CK_Market__Tracker <- cbind(CK_Market__Tracker, New_Info[1])
}

CK_Market__Tracker[6:ncol(CK_Market__Tracker)] <- sapply(CK_Market__Tracker[6:ncol(CK_Market__Tracker)], as.character)
CK_Market__Tracker[6:ncol(CK_Market__Tracker)] <- sapply(CK_Market__Tracker[6:ncol(CK_Market__Tracker)], as.numeric)
#CK_Market__Tracker <- as.data.frame(CK_Market__Tracker)

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
TCG_Market_Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$TCG_MKT[match(TCG_Market_Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  TCG_Market_Tracker <- cbind(TCG_Market_Tracker, New_Info[1])
}

TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)] <- sapply(TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)], as.character)
TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)] <- sapply(TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)], as.numeric)
range = ncol(TCG_Market_Tracker)

CK_Retail_Comparison <- TCG_Market_Tracker[1:5]
for (i in 6:range){
  CK_Retail_Comparison[i] <- round(CK_Market__Tracker[i] -  TCG_Market_Tracker[i],2)
}

MBT <- CK_Retail_Comparison[,-c(1:5)]
MBT <- sapply(MBT,as.numeric)
New <- CK_Retail_Comparison$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}

New <- as.data.frame(New)
New[is.na(New)] <- 0
New <- New[-1]
New <- sapply(New, as.numeric)


Three_Weeks_Sum <- New[,c((ncol(New)-60):ncol(New))]
sumup <- rowSums(Three_Weeks_Sum)
Up_Down <- as.data.frame(sumup)

Fifteen_Days_Sum <- New[,c((ncol(New)-21):ncol(New))]
sumup <- rowSums(Fifteen_Days_Sum)
Up_Down_Three_Weeks <- as.data.frame(sumup)

Seven_Day_Sum <- New[,c((ncol(New)-7):ncol(New))]
sumup <- rowSums(Seven_Day_Sum)
Up_Down_Seven_Days <- as.data.frame(sumup)

CK_Retail_Comparison$All_Time <- Up_Down$sumup
CK_Retail_Comparison$Three_Weeks <- Up_Down_Three_Weeks$sumup
CK_Retail_Comparison$One_Week <- Up_Down_Seven_Days$sumup

CK_Retail_Comparison_AT <- CK_Retail_Comparison[order(CK_Retail_Comparison$All_Time),]
CK_Retail_Comparison_AT <- CK_Retail_Comparison_AT[c(1:200),]
CK_Retail_Comparison_Three_Weeks <- CK_Retail_Comparison[order(CK_Retail_Comparison$Three_Weeks),]
CK_Retail_Comparison_Three_Weeks <- CK_Retail_Comparison_Three_Weeks[c(1:200),]
CK_Retail_Comparison_One_Weeks <- CK_Retail_Comparison[order(CK_Retail_Comparison$One_Week),]
CK_Retail_Comparison_One_Weeks <- CK_Retail_Comparison_One_Weeks[c(1:200),]


AT_CK <- data.frame(CK_Retail_Comparison_AT[1:5],CK_Retail_Comparison_AT[ncol(CK_Retail_Comparison_AT)-2])
Three_Week_CK <- data.frame(CK_Retail_Comparison_Three_Weeks[1:5],CK_Retail_Comparison_Three_Weeks[ncol(CK_Retail_Comparison_Three_Weeks)-1])
One_Week_CK <- data.frame(CK_Retail_Comparison_One_Weeks[1:5],CK_Retail_Comparison_One_Weeks[ncol(CK_Retail_Comparison_One_Weeks)])

AT_CK$Rank <- seq(nrow(AT_CK))
Three_Week_CK$Rank <-seq(nrow(Three_Week_CK))
One_Week_CK$Rank <- seq(nrow(One_Week_CK))

AT_CK$CK_Retail <- CK_Market__Tracker$`2020-03-22`[match(AT_CK$Key,CK_Market__Tracker$Key)]
AT_CK$TCG_Retail <- TCG_Market_Tracker$`2020-03-22`[match(AT_CK$Key, TCG_Market_Tracker$Key)]

Three_Week_CK$CK_Retail<- CK_Market__Tracker$`2020-03-22`[match(Three_Week_CK$Key,CK_Market__Tracker$Key)]
Three_Week_CK$TCG_Retail<- TCG_Market_Tracker$`2020-03-22`[match(Three_Week_CK$Key, TCG_Market_Tracker$Key)]

One_Week_CK$CK_Retail<- CK_Market__Tracker$`2020-03-22`[match(One_Week_CK$Key,CK_Market__Tracker$Key)]
One_Week_CK$TCG_Retail<- TCG_Market_Tracker$`2020-03-22`[match(One_Week_CK$Key, TCG_Market_Tracker$Key)]

Mystery_Booster_Reprint <- Updated_Tracking_Keys[which(Updated_Tracking_Keys$Set == "Mystery Booster"),]
AT_CK$`MB1` <- Mystery_Booster_Reprint$Set[match(AT_CK$name,Mystery_Booster_Reprint$name)]
Three_Week_CK$`MB1`<- Mystery_Booster_Reprint$Set[match(Three_Week_CK$name,Mystery_Booster_Reprint$name)]
One_Week_CK$`MB1`<- Mystery_Booster_Reprint$Set[match(One_Week_CK$name,Mystery_Booster_Reprint$name)]
AT_CK$`MB1`[is.na(AT_CK$`MB1`) == T] <- ""
Three_Week_CK$`MB1`[is.na(Three_Week_CK$`MB1`) == T] <- ""
One_Week_CK$`MB1`[is.na(One_Week_CK$`MB1`)==T] <- ""

library(devtools)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(googledrive)
#library(googlesheets)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "GMAIL")
ss <- drive_get("CK_VS_TCG_Review")

gs4_auth(email = "GMAIL")
sheet_write(
  AT_CK,
  ss = ss,
  sheet = "3_Months"
)
sheet_write(
  Three_Week_CK,
  ss = ss,
  sheet = "3_Weeks"
)
sheet_write(
  One_Week_CK,
  ss = ss,
  sheet = "Prior_Week"
)



#Cardsphere Mapped out####
library(XML)
library(RCurl)
library(rvest) 
library(tidyverse)
library(plyr)
library(jsonlite)
library(readr)
library(lubridate)
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
Dollar_Fix <- function(t){
  ifelse(nchar(t) == 5, right(t,4),ifelse(nchar(t) == 6,right(t,5),ifelse(nchar(t) == 7,right(t,6),ifelse(nchar(t) == 8,right(t,7),0))))
}

CK_Buylist <- fromJSON("https://api.cardkingdom.com/api/pricelist")
CK_Buylist <- as.data.frame(CK_Buylist)
CK_Buylist <- CK_Buylist[which(CK_Buylist$data.variation == ""),]
#CK_Buylist <- CK_Buylist[which(CK_Buylist$data.edition == "(Other)"),]
No_Foils <- CK_Buylist[which(CK_Buylist$data.is_foil == "false"),]
Foils <- CK_Buylist[which(CK_Buylist$data.is_foil == "true"),]
Set_Breakdown <- as.data.frame(summary(as.factor(CK_Buylist$data.edition),maxsum = 5000))
NF_Set_Breakdown <- as.data.frame(summary(as.factor(No_Foils$data.edition),maxsum = 5000))
F_Set_Breakdown <- as.data.frame(summary(as.factor(Foils$data.edition),maxsum = 5000))
colnames(Set_Breakdown) <- "Set"
colnames(NF_Set_Breakdown) <- "Set"
colnames(F_Set_Breakdown) <- "Set"
Total_Offers <- sum(Set_Breakdown$Set)
NF_Total_Offers <- sum(NF_Set_Breakdown$Set)
F_Total_Offers <- sum(F_Set_Breakdown$Set)
Set_Breakdown$Set_Composition <- round(Set_Breakdown$Set/Total_Offers,4)*100
NF_Set_Breakdown$Set_Composition <- round(NF_Set_Breakdown$Set/NF_Total_Offers,4)*100
F_Set_Breakdown$Set_Composition <- round(F_Set_Breakdown$Set/F_Total_Offers,4)*100

Slim_CK_Buylist <- CK_Buylist[c(1,6,8,9,10,11,12,13)]
Slim_CK_Buylist$Exclusion <- Exclusion$Excl_Excl[match(Slim_CK_Buylist$data.edition,Exclusion$Set_Excl)]
Slim_CK_Buylist$Exclusion[is.na(Slim_CK_Buylist$Exclusion)==TRUE] <- "Unclear"

Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$Exclusion != "Exclude"),]
Slim_CK_Buylist <- Slim_CK_Buylist[,-9]
Slim_CK_Buylist$data.is_foil <- ifelse(Slim_CK_Buylist$data.is_foil == "false", "","FOIL")

Slim_CK_Buylist$data.qty_retail <- ifelse(Slim_CK_Buylist$data.qty_retail == 0, 1,Slim_CK_Buylist$data.qty_retail)
Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$data.qty_buying != 0),]
Slim_CK_Buylist$QTY_Diff <- round((Slim_CK_Buylist$data.qty_buying-Slim_CK_Buylist$data.qty_retail)/Slim_CK_Buylist$data.qty_buying,2)
Slim_CK_Buylist$Price_Diff <- round((as.numeric(as.character(Slim_CK_Buylist$data.price_buy))/as.numeric(as.character(Slim_CK_Buylist$data.price_retail))),2)

Dollar_Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$data.price_buy > 1.50),]
Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[order(-Dollar_Slim_CK_Buylist$QTY_Diff),]
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .90 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .64 & as.numeric(as.character(Dollar_Slim_CK_Buylist$data.qty_buying)) > 100, 0 , "Awaiting Tier")

Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .90 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .64 & Dollar_Slim_CK_Buylist$Tiers != 0, 1, Dollar_Slim_CK_Buylist$Tiers)
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .80 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .60 & as.factor(Dollar_Slim_CK_Buylist$Tiers) == "Awaiting Tier", 2, Dollar_Slim_CK_Buylist$Tiers)
Curated_CK_Buylist <- Dollar_Slim_CK_Buylist[which(as.factor(Dollar_Slim_CK_Buylist$Tiers) != "Awaiting Tier"),]
Curated_CK_Buylist$Tiers <- as.numeric(as.character(Curated_CK_Buylist$Tiers))
Curated_CK_Buylist <- Curated_CK_Buylist[order(Curated_CK_Buylist$Tiers),]

#Cardsphere####
TCG_URL <- "https://www.cardsphere.com/sets" 
TCG_URL_1 <- getURL(TCG_URL)
html <- read_html(TCG_URL)
html_nodes(html,".body")

Page_Contents <- htmlTreeParse(TCG_URL_1, useInternalNode=TRUE)

Xpath_contents <- xpathSApply(Page_Contents,"//li", xmlValue)
Xpath_contents <- as.data.frame(Xpath_contents)
CardSphere_Secrets <-data.frame(do.call('rbind', strsplit(as.character(Xpath_contents$Xpath_contents),'  ',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
CardSphere_Secrets <- CardSphere_Secrets$X29
CardSphere_Secrets <- data.frame(do.call('rbind', strsplit(as.character(Xpath_contents$Xpath_contents),'\n',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
CardSphere_Secrets <- CardSphere_Secrets$X3
CardSphere_Secrets <- as.data.frame(CardSphere_Secrets)
CardSphere_Secrets <- CardSphere_Secrets[-c(1:3),]
CardSphere_Secrets <- as.data.frame(CardSphere_Secrets)
CardSphere_Secrets <- CardSphere_Secrets[-c((nrow(CardSphere_Secrets)-13):nrow(CardSphere_Secrets)),]
CardSphere_Secrets <- unlist(CardSphere_Secrets)
CardSphere_Printed_Sets <- as.data.frame(CardSphere_Secrets)

Xpath_contents <- xpathSApply(Page_Contents,"//a", xmlGetAttr, 'href')
Xpath_contents <- gsub("/sets/","",Xpath_contents)
Xpath_contents <- as.data.frame(Xpath_contents)
Xpath_contents <- Xpath_contents[-c(1:4),]
Xpath_contents <- as.data.frame(Xpath_contents)
Total_Rows <- nrow(Xpath_contents)
TBR_Rows <- nrow(Xpath_contents)-15
CardSphere_Set_Numbers <- Xpath_contents[-c(TBR_Rows:Total_Rows),]
Cardsphere_Outer_Shell <- data.frame(CardSphere_Printed_Sets,CardSphere_Set_Numbers)


All_CardSphere <- NULL
total = nrow(Cardsphere_Outer_Shell) 
pb <- txtProgressBar(min=0, max = total, style = 3)
Q <- 1
for (set in Cardsphere_Outer_Shell$CardSphere_Set_Numbers) {
  Set_Url <- paste("https://www.cardsphere.com/sets/",set,sep="")
  Set_Number <- set
  html <- read_html(Set_Url)
  cs_no <- html %>% html_nodes(".cs-row") %>% html_text()
  cs_attempt <-gsub(" '/(\r\n)+|\r+|\n\\s+|\t+/i'","",cs_no)
  cs_attempt <- as.data.frame(cs_attempt)
  cs_attempt <- cs_attempt[-c(1,nrow(cs_attempt)),]
  cs_attempt <- data.frame(do.call('rbind', strsplit(as.character(cs_attempt),'$',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
  cs_attempt <- as.data.frame(cs_attempt)
  cs_attempt$X3 <- ifelse(ncol(cs_attempt)==2, cs_attempt$X2, cs_attempt$X3)
  cs_attempt$X1 <- as.character(cs_attempt$X1)
  cs_attempt$X3 <- as.character(cs_attempt$X3)
  cs_attempt$X3 <- ifelse(cs_attempt$X3 == cs_attempt$X1, NA, cs_attempt$X3)
  cs_attempt$edition <- Cardsphere_Outer_Shell$CardSphere_Secrets[match(Set_Number,Cardsphere_Outer_Shell$CardSphere_Set_Numbers)]
  cs_attempt$'F/NF' <- ""
  colnames(cs_attempt) <- c("name","nf_price","f_price","edition",'F/NF')
  cs_nonfoil_prices <- data.frame(cs_attempt$name,cs_attempt$edition,cs_attempt$`F/NF`,cs_attempt$nf_price)
  cs_foil_prices <- data.frame(cs_attempt$name,cs_attempt$edition,cs_attempt$`F/NF`,cs_attempt$f_price)
  cs_foil_prices$cs_attempt..F.NF. <- "FOIL"
  
  cs_nonfoil_prices$cs_attempt.name <- trimws(cs_nonfoil_prices$cs_attempt.name)
  cs_nonfoil_prices$cs_attempt.edition <- trimws(cs_nonfoil_prices$cs_attempt.edition)
  
  cs_foil_prices$cs_attempt.name <- trimws(cs_foil_prices$cs_attempt.name)
  cs_foil_prices$cs_attempt.edition <- trimws(cs_foil_prices$cs_attempt.edition)
  
  cs_nonfoil_prices$Key <- paste(cs_nonfoil_prices$cs_attempt.name,cs_nonfoil_prices$cs_attempt.edition,cs_nonfoil_prices$cs_attempt..F.NF.,sep="")
  cs_foil_prices$Key <- paste(cs_foil_prices$cs_attempt.name,cs_foil_prices$cs_attempt.edition,cs_foil_prices$cs_attempt..F.NF.,sep="")
  
  cs_nonfoil_prices <- cs_nonfoil_prices[,c(5,1,2,3,4)]
  cs_foil_prices <- cs_foil_prices[,c(5,1,2,3,4)]
  #cs_nonfoil_prices <-unname(cs_nonfoil_prices)
  #cs_foil_prices <-unname(cs_foil_prices)
  cs_prices <- rbind.fill(cs_nonfoil_prices,cs_foil_prices)
  cs_prices$cs_attempt.f_price <- as.numeric(as.character(cs_prices$cs_attempt.f_price))
  cs_prices$cs_attempt.nf_price <- as.numeric(as.character(cs_prices$cs_attempt.nf_price))
  cs_prices$cs_attempt.nf_price <- ifelse(is.na(cs_prices$cs_attempt.nf_price)==TRUE, cs_prices$cs_attempt.f_price, cs_prices$cs_attempt.nf_price)
  cs_prices <- cs_prices[,-ncol(cs_prices)]
  cs_prices <- unname(cs_prices)
  colnames(cs_prices) <- c("Key","name","edition","isfoil","retail")
  All_CardSphere <- rbind(All_CardSphere, cs_prices)
  Sys.sleep(sample(.29:1.63, 1))
  setTxtProgressBar(pb,Q)
  Q <- Q+1
}

CardSphere_Final_Output <- All_CardSphere
CardSphere_Final_Output$retail_.80 <- round(CardSphere_Final_Output$retail*.80,1)
CK_Equivalent <- Slim_CK_Buylist
CK_Equivalent$meta.created_at <- paste(CK_Equivalent$data.name,CK_Equivalent$data.edition,CK_Equivalent$data.is_foil,sep="")
CardSphere_Final_Output$CK_BL_Offer <- CK_Equivalent$data.price_buy[match(CardSphere_Final_Output$Key,CK_Equivalent$meta.created_at)]
CardSphere_Final_Output <- na.omit(CardSphere_Final_Output)
CardSphere_Final_Output$Opportunities <- as.numeric(as.character(CardSphere_Final_Output$CK_BL_Offer)) - as.numeric(as.character(CardSphere_Final_Output$retail_.80))
CardSphere_Final_Output <- CardSphere_Final_Output[order(-CardSphere_Final_Output$Opportunities),]
CardsphereNF <- CardSphere_Final_Output[which(CardSphere_Final_Output$isfoil == ""),]


Reference<- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
Reference$Semi_Key <- substr(Reference$Key,1,nchar(Reference$Key)-1)
CardsphereNF$Rarity <- Reference$Rarity[match(CardsphereNF$Key,Reference$Semi_Key)]
Cardsphere_Mythics <- CardsphereNF[which(CardsphereNF$Rarity == "M"),]
Cardsphere_Rares <- CardsphereNF[which(CardsphereNF$Rarity == "R"),]
Cardsphere_Uncommons <- CardsphereNF[which(CardsphereNF$Rarity == "U"),]
Cardsphere_Commons <- CardsphereNF[which(CardsphereNF$Rarity == "C"),]
Cardsphere_Unknowns <- CardsphereNF[which(is.na(CardsphereNF$Rarity) == TRUE),]


CardsphereNF <- CardsphereNF[,-9]
Cardsphere_Mythics <- Cardsphere_Mythics[,-9]
Cardsphere_Rares <- Cardsphere_Rares[,-9]
Cardsphere_Uncommons <- Cardsphere_Uncommons[,-9]
Cardsphere_Commons <- Cardsphere_Commons[,-9]
Cardsphere_Unknowns <- Cardsphere_Unknowns[,-9]

currentDate <- Sys.Date()
library(devtools)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(googledrive)
#library(googlesheets)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "GMAIL")

#drive_auth(email = "GMAIL")
#drive_auth(use_oob=TRUE)

# cs <- list(CardsphereNF,Cardsphere_Mythics,Cardsphere_Rares,Cardsphere_Uncommons,Cardsphere_Commons,Cardsphere_Unknowns)
# #sheets_deauth()
# gs4_auth()
# sheets_create(
#   paste(currentDate,"_Cardsphere_Review",sep=""),
#   sheets = cs
# )
drive_auth(email = "GMAIL", use_oob=TRUE)
gs4_auth(email = "GMAIL", use_oob=TRUE)
ss <- drive_get("Cardsphere_Review")
#sheets_deauth()
gs4_auth(email = "GMAIL")
sheet_write(
  CardsphereNF,
  ss = ss,
  sheet = "All_CS_NF"
)
sheet_write(
  Cardsphere_Mythics,
  ss = ss,
  sheet = "Mythics"
)
sheet_write(
  Cardsphere_Rares,
  ss = ss,
  sheet = "Rares"
)
sheet_write(
  Cardsphere_Uncommons,
  ss = ss,
  sheet = "Uncommons"
)
sheet_write(
  Cardsphere_Commons,
  ss = ss,
  sheet = "Commons"
)
sheet_write(
  Cardsphere_Unknowns,
  ss = ss,
  sheet = "Unknowns"
)
drive_auth(email = "GMAIL", use_oob=TRUE)
gs4_auth(email = "GMAIL", use_oob=TRUE)

ss <- drive_get("Wolfs_Buylist_Review")
Wolfs_Buylist <- range_read(ss)
Wolfs_Buylist <- as.data.frame(Wolfs_Buylist)
Buylist <- Wolfs_Buylist
Buylist$Semi_Key <- paste(Buylist$data.name,Buylist$data.edition, sep="")
Wolfs_Buylist$data.is_foil[is.na(Wolfs_Buylist$data.is_foil) == T] <- ""
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.is_foil == ""),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy <= 20),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy >= 1),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$Printings <= 5),]
Wolfs_Buylist <- Wolfs_Buylist[order(Wolfs_Buylist$Velocity_Adjusted),]
Wolfs_Targets <- data.frame(Wolfs_Buylist$data.name,Wolfs_Buylist$data.edition,Wolfs_Buylist$data.price_retail,Wolfs_Buylist$data.price_buy,Wolfs_Buylist$Printings)
Wolfs_Targets$Semi_Key <- paste(Wolfs_Targets$Wolfs_Buylist.data.name,Wolfs_Targets$Wolfs_Buylist.data.edition,sep="")


SS <- drive_get("Cardsphere_Review")
Cardsphere_Retrieval <- sheets_read(SS, sheet = "All_CS_NF")
Cardsphere_Retrieval <- as.data.frame(Cardsphere_Retrieval)
CS <- Cardsphere_Retrieval

Cardsphere_Retrieval$isfoil[is.na(Cardsphere_Retrieval$isfoil) == T] <- ""
Cardsphere_Retrieval <- Cardsphere_Retrieval[which(Cardsphere_Retrieval$Opportunities >= 1.00),]
Cardsphere_Retrieval <- Cardsphere_Retrieval[which(Cardsphere_Retrieval$CK_BL_Offer >= .25),]

Target_List <- append(Cardsphere_Retrieval$Key,Wolfs_Targets$Semi_Key)
Target_List <- as.data.frame(Target_List)
Target_List <- unique(Target_List$Target_List)
Target_List <- as.data.frame(Target_List)

Target_List$Name <- Buylist$data.name[match(Target_List$Target_List,Buylist$Semi_Key)]
Target_List$Edition <- Buylist$data.edition[match(Target_List$Target_List,Buylist$Semi_Key)]
All_Cards <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv",col_types = cols(`F/NF` = col_character()))
All_Cards$Semi <- paste(All_Cards$name,All_Cards$Set,sep="")
Target_List$Rarity <-All_Cards$Rarity[match(Target_List$Target_List,All_Cards$Semi)]
Target_List$CK_Buylist <- CS$CK_BL_Offer[match(Target_List$Target_List,CS$Key)]
Target_List$CS_Retail <- CS$retail[match(Target_List$Target_List,CS$Key)]
Target_List$CS_Offer <- round((Target_List$CS_Retail * .72),1)
Target_List$My_Offer <- Target_List$CS_Offer - as.numeric(as.character(Target_List$CK_Buylist))
Target_List <- na.omit(Target_List)
Target_List <- Target_List[order(Target_List$My_Offer),]
Target_List <- Target_List[which(Target_List$My_Offer <= 0),]
Target_List <- Target_List[which(Target_List$My_Offer <= -1.00),]


CS_Import_List <- NULL
CS_Import_List$Name <- Target_List$Name
CS_Import_List <- as.data.frame(CS_Import_List)
CS_Import_List$Edition <- Target_List$Edition
CS_Import_List$Condition <- "Near Mint"
CS_Import_List$Language <- "English"
CS_Import_List$Finish <- ""
CS_Import_List$Tags <- ""
CS_Import_List$Quantity <- 12
CS_Import_List$Tradelist_Count <- 0

CS_Import_List <- CS_Import_List[moveme(names(CS_Import_List), "Tradelist_Count first")]
CS_Import_List <- CS_Import_List[moveme(names(CS_Import_List), "Quantity first")]
colnames(CS_Import_List)[2] <- c("Tradelist Count")

drive_auth(email = "GMAIL", use_oob=TRUE)
gs4_auth(email = "GMAIL", use_oob=TRUE)

ss <- drive_get("Cardsphere_Import_List")
#sheets_deauth()

sheet_write(
  CS_Import_List,
  ss = ss,
  sheet = "CS_Import_List"
)

remDr = remoteDriver(remoteServerAddr = "167.172.137.242", port = 4445L, browser = "chrome")
remDr$open()

remDr$navigate("https://www.cardsphere.com/login")
Sys.sleep(5)

username <- remDr$findElement(using = "id", value = "email")
username$clearElement()
username$sendKeysToElement(list("EMAIL"))

passwd <- remDr$findElement(using = "id", value = "password")
passwd$clearElement()
passwd$sendKeysToElement(list("PASSWORD"))

Post_Credential_Login <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div/div/div/form/button")
Post_Credential_Login$submitElement()
Sys.sleep(2)
remDr$navigate("https://www.cardsphere.com/wants")
Actions <- remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[3]/a')
Actions$clickElement()

Delete_Wants <- remDr$findElement(using = "class", value = 'btn-danger')
Delete_Wants$clickElement()
Delete_Wants_Check <- remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[2]/form/div/label/input')
Delete_Wants_Check$clickElement()
Delete_Final <-remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[3]/button[2]')
Delete_Final$clickElement()
New_Wants <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[1]/a')
New_Wants$clickElement()


for(i in 1:24){
  Offer_Perc <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[6]/div/div[1]')
  Offer_Perc$clickElement()
  Sys.sleep(.20)
}


for(i in 1:nrow(CS_Import_List)){
  Card_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[1]/div/input')
  Card_Search$clickElement()
  Card_Search$sendKeysToElement(list(CS_Import_List$Name[i]))
  Sys.sleep(3)
  Card_Search <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[1]')
  Sys.sleep(1)
  Card_Search$clickElement()
  Card_Search$clickElement()
  
  Sys.sleep(1)
  Set_Search <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div')
  Set_Search$clickElement()
  Set_Search$clickElement()
  # Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
  # Set_Search$clickElement()
  Sys.sleep(1)
  Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
  Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
  Sys.sleep(1)
  Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
  Set_Select$clickElement()
  Set_Select$click(buttonId = 2)
  if(Set_Search$getElementText() == "Select sets\n "){
    Sys.sleep(1)
    Set_Search <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div')
    Set_Search$clickElement()
    
    # Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
    # Set_Search$clickElement()
    Sys.sleep(1)
    Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
    Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
    Sys.sleep(1)
    Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
    Set_Select$clickElement()
    Set_Select$click(buttonId = 2)
  }
  
  
  
  Sys.sleep(2)
  Finish_Selection <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[4]/div/a/span[1]')
  Finish_Selection$clickElement()
  Finish_Selection$sendKeysToElement(list("Nonfoil"))
  
  Sys.sleep(1)
  for(i in 1:11){
    Quantity_Selection <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[9]/div/div[2]')
    Quantity_Selection$clickElement()
    Sys.sleep(.20)
  }
  Sys.sleep(1)
  Submition <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[12]/button')
  Submition$clickElement()
  Sys.sleep(3)
}


wd <- getwd()
assign('.First', function(x) {
  require("rvest","jsonlite","plyr","readr","purrr","tidyr","tibble","tidyverse","reshape2","knitr","tictoc","devtools",
          "taRifx","ggplot2","zoo","dplyr","rlist","fpp2","forecast","stats","tseries","data.table","gtools","RSelenium") #and whatever other packages you're using
  file.remove(".RData") #already been loaded
  rm(".Last", pos=.GlobalEnv) #otherwise won't be able to quit R without it restarting
  setwd(wd)
}, pos=.GlobalEnv)
assign(".Last", function() {
  system("R --no-site-file --no-init-file --quiet")
}, pos=.GlobalEnv)
q("no")