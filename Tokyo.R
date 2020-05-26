install.packages(c("rvest","tidyverse","RSelenium"))
library(tidyverse)
library(rvest)
library(RSelenium)


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

#Japanese Palantir####
remDr = remoteDriver(remoteServerAddr = "167.172.233.212", port = 4445L, browser = "chrome")
remDr$open()
remDr$navigate("https://tokyomtg.com/")
English <- remDr$findElement(using = "class", value = "fa-refresh")
English$clickElement()
Sys.sleep(2)
Currency <- remDr$findElement(using = "xpath", '//*[@id="currency-select"]/option[2]')
Currency$clickElement()
Sys.sleep(3)
remDr$navigate("https://tokyomtg.com/cardpage.html?p=s&s=8")
Select_Language <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[4]/div/div[1]/div')$clickElement()
Remove_JPN <- remDr$findElement("xpath",'//*[@id="language-checkboxes"]/div[2]/label/span')$clickElement()
Sys.sleep(1)
Select_Language <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[4]/div/div[1]/div')$clickElement()
Sys.sleep(1)
Select_Foils <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[3]/div/div[1]/div')$clickElement()
Remove_Foils <- remDr$findElement("xpath", '//*[@id="premium-checkboxes"]/div[1]/label/span')$clickElement()
Sys.sleep(1)
Select_Foils <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[3]/div/div[1]/div')$clickElement()

Refresh <- remDr$findElement("class", "filter-button")$clickElement()
Table_of_Contents <- data.frame("Pages" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), "Page_Address" = c(20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400))

Tokyos_Contents <- NULL

#Data Acquisition####
for (i in 8:291){
Desired_URL <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,sep="")
remDr$navigate(Desired_URL)
Skip = "No"
Skip = tryCatch(expr = {remDr$findElement("xpath","/html/body/div[1]/div/section/table/tbody/tr/td[2]")$getElementText()}, error = function(e){Skip = "Yes"})
No_Options = tryCatch(expr = {unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/h3")$getElementText())}, error = function(e){No_Options = "Passes"})
if(No_Options == "All of the cards are hidden by your filter settings."){Skip = "Yes"}else{Skip = Skip}  
  if(Skip == "Yes"){} else {
    Pages_In_Set <-trimws(unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/table/tbody/tr/td[2]")$getElementText()))
    Pages_In_Set <- data.frame(do.call('rbind',strsplit(as.character(Pages_In_Set), ' of ', fixed=T)))
    Pages_In_Set <- as.numeric(as.character(Pages_In_Set$X2))
    if(Pages_In_Set%%1 == 0){
    Pages_To_Scrape <- round(Pages_In_Set/20,0)
    Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }else{
      Pages_To_Scrape <- round(Pages_In_Set/20,0)+1
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }
  if(length(Unique_Contents$Pages) == 0 ){ Unique_Contents = data.frame(1,20) } else {Unique_Contents$Pages == Unique_Contents$Pages}
  colnames(Unique_Contents) <- c("Pages","Page_Address")
      for(j in 1:(1+max(Unique_Contents$Pages))){
      needed_specifier <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,"&b=",(Unique_Contents$Page_Address[j]-20),sep="")
      remDr$navigate(needed_specifier)
      Kyoko_Fukada <- remDr$getPageSource()[[1]]%>% read_html()
        
      Kyoko_Fukada_First_Secret <-  Kyoko_Fukada %>% html_nodes("h4") %>% html_text()
      Kyoko_Fukada_First_Secret <- as.data.frame(Kyoko_Fukada_First_Secret)
      Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret <- as.character(Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret)
      Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),]
      Kyoko_Fukada_Names <- gsub(" \\(English\\)","", Kyoko_Fukada_Names)
        
        
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada %>% html_nodes("h5") %>% html_text()
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Second_Secret[!grepl("SUBTOTAL",Kyoko_Fukada_Second_Secret)]
      Kyoko_Fukada_Second_Secret <- data.frame(do.call('rbind', strsplit(as.character(Kyoko_Fukada_Second_Secret),' - ',fixed=TRUE)))
      colnames(Kyoko_Fukada_Second_Secret) <- c("Edition","Rarity")
        
      Kyoko_Fukada_Third_Secret <- Kyoko_Fukada %>% html_nodes(".tab-content") %>% html_text()
      Kyoko_Fukada_Third_Secret <- gsub("\\n","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\$","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("add to cart","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("Out of stock","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("QTY\\:\\d+.*","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\)","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- data.frame(do.call('rbind',strsplit(as.character(Kyoko_Fukada_Third_Secret), ' (Stock: ', fixed=T)))
      colnames(Kyoko_Fukada_Third_Secret) <- c("Retail","Stock")
      
      Kyoko_Fukada_Unifies <- data.frame(Kyoko_Fukada_Names,Kyoko_Fukada_Second_Secret$Edition, Kyoko_Fukada_Second_Secret$Rarity,Kyoko_Fukada_Third_Secret$Retail, Kyoko_Fukada_Third_Secret$Stock)
      colnames(Kyoko_Fukada_Unifies) <- c("Card","Edition","Rarity","Retail","Stock")
      Tokyos_Contents <- rbind(Tokyos_Contents, Kyoko_Fukada_Unifies)
      Tokyos_Contents <- unique(Tokyos_Contents)
      }
  } 
}
for (i in 293){
  Desired_URL <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,sep="")
  remDr$navigate(Desired_URL)
  Skip = "No"
  Skip = tryCatch(expr = {remDr$findElement("xpath","/html/body/div[1]/div/section/table/tbody/tr/td[2]")$getElementText()}, error = function(e){Skip = "Yes"})
  No_Options = tryCatch(expr = {unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/h3")$getElementText())}, error = function(e){No_Options = "Passes"})
  if(No_Options == "All of the cards are hidden by your filter settings."){Skip = "Yes"}else{Skip = Skip}  
  if(Skip == "Yes"){} else {
    Pages_In_Set <-trimws(unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/table/tbody/tr/td[2]")$getElementText()))
    Pages_In_Set <- data.frame(do.call('rbind',strsplit(as.character(Pages_In_Set), ' of ', fixed=T)))
    Pages_In_Set <- as.numeric(as.character(Pages_In_Set$X2))
    if(Pages_In_Set%%1 == 0){
      Pages_To_Scrape <- round(Pages_In_Set/20,0)
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }else{
      Pages_To_Scrape <- round(Pages_In_Set/20,0)+1
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }
    if(length(Unique_Contents$Pages) == 0 ){ Unique_Contents = data.frame(1,20) } else {Unique_Contents$Pages == Unique_Contents$Pages}
    colnames(Unique_Contents) <- c("Pages","Page_Address")
    for(j in 1:(1+max(Unique_Contents$Pages))){
      needed_specifier <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,"&b=",(Unique_Contents$Page_Address[j]-20),sep="")
      remDr$navigate(needed_specifier)
      Kyoko_Fukada <- remDr$getPageSource()[[1]]%>% read_html()
      
      Kyoko_Fukada_First_Secret <-  Kyoko_Fukada %>% html_nodes("h4") %>% html_text()
      Kyoko_Fukada_First_Secret <- as.data.frame(Kyoko_Fukada_First_Secret)
      Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret <- as.character(Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret)
      Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),]
      Kyoko_Fukada_Names <- gsub(" \\(English\\)","", Kyoko_Fukada_Names)
      
      
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada %>% html_nodes("h5") %>% html_text()
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Second_Secret[!grepl("SUBTOTAL",Kyoko_Fukada_Second_Secret)]
      Kyoko_Fukada_Second_Secret <- data.frame(do.call('rbind', strsplit(as.character(Kyoko_Fukada_Second_Secret),' - ',fixed=TRUE)))
      colnames(Kyoko_Fukada_Second_Secret) <- c("Edition","Rarity")
      
      Kyoko_Fukada_Third_Secret <- Kyoko_Fukada %>% html_nodes(".tab-content") %>% html_text()
      Kyoko_Fukada_Third_Secret <- gsub("\\n","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\$","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("add to cart","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("Out of stock","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("QTY\\:\\d+.*","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\)","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- data.frame(do.call('rbind',strsplit(as.character(Kyoko_Fukada_Third_Secret), ' (Stock: ', fixed=T)))
      colnames(Kyoko_Fukada_Third_Secret) <- c("Retail","Stock")
      
      Kyoko_Fukada_Unifies <- data.frame(Kyoko_Fukada_Names,Kyoko_Fukada_Second_Secret$Edition, Kyoko_Fukada_Second_Secret$Rarity,Kyoko_Fukada_Third_Secret$Retail, Kyoko_Fukada_Third_Secret$Stock)
      colnames(Kyoko_Fukada_Unifies) <- c("Card","Edition","Rarity","Retail","Stock")
      Tokyos_Contents <- rbind(Tokyos_Contents, Kyoko_Fukada_Unifies)
      Tokyos_Contents <- unique(Tokyos_Contents)
    }
  } 
}
for (i in 304:311){
  Desired_URL <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,sep="")
  remDr$navigate(Desired_URL)
  Skip = "No"
  Skip = tryCatch(expr = {remDr$findElement("xpath","/html/body/div[1]/div/section/table/tbody/tr/td[2]")$getElementText()}, error = function(e){Skip = "Yes"})
  No_Options = tryCatch(expr = {unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/h3")$getElementText())}, error = function(e){No_Options = "Passes"})
  if(No_Options == "All of the cards are hidden by your filter settings."){Skip = "Yes"}else{Skip = Skip}  
  if(Skip == "Yes"){} else {
    Pages_In_Set <-trimws(unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/table/tbody/tr/td[2]")$getElementText()))
    Pages_In_Set <- data.frame(do.call('rbind',strsplit(as.character(Pages_In_Set), ' of ', fixed=T)))
    Pages_In_Set <- as.numeric(as.character(Pages_In_Set$X2))
    if(Pages_In_Set%%1 == 0){
      Pages_To_Scrape <- round(Pages_In_Set/20,0)
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }else{
      Pages_To_Scrape <- round(Pages_In_Set/20,0)+1
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }
    if(length(Unique_Contents$Pages) == 0 ){ Unique_Contents = data.frame(1,20) } else {Unique_Contents$Pages == Unique_Contents$Pages}
    colnames(Unique_Contents) <- c("Pages","Page_Address")
    for(j in 1:(1+max(Unique_Contents$Pages))){
      needed_specifier <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,"&b=",(Unique_Contents$Page_Address[j]-20),sep="")
      remDr$navigate(needed_specifier)
      Kyoko_Fukada <- remDr$getPageSource()[[1]]%>% read_html()
      
      Kyoko_Fukada_First_Secret <-  Kyoko_Fukada %>% html_nodes("h4") %>% html_text()
      Kyoko_Fukada_First_Secret <- as.data.frame(Kyoko_Fukada_First_Secret)
      Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret <- as.character(Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret)
      Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),]
      Kyoko_Fukada_Names <- gsub(" \\(English\\)","", Kyoko_Fukada_Names)
      
      
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada %>% html_nodes("h5") %>% html_text()
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Second_Secret[!grepl("SUBTOTAL",Kyoko_Fukada_Second_Secret)]
      Kyoko_Fukada_Second_Secret <- data.frame(do.call('rbind', strsplit(as.character(Kyoko_Fukada_Second_Secret),' - ',fixed=TRUE)))
      colnames(Kyoko_Fukada_Second_Secret) <- c("Edition","Rarity")
      
      Kyoko_Fukada_Third_Secret <- Kyoko_Fukada %>% html_nodes(".tab-content") %>% html_text()
      Kyoko_Fukada_Third_Secret <- gsub("\\n","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\$","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("add to cart","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("Out of stock","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("QTY\\:\\d+.*","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\)","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- data.frame(do.call('rbind',strsplit(as.character(Kyoko_Fukada_Third_Secret), ' (Stock: ', fixed=T)))
      colnames(Kyoko_Fukada_Third_Secret) <- c("Retail","Stock")
      
      Kyoko_Fukada_Unifies <- data.frame(Kyoko_Fukada_Names,Kyoko_Fukada_Second_Secret$Edition, Kyoko_Fukada_Second_Secret$Rarity,Kyoko_Fukada_Third_Secret$Retail, Kyoko_Fukada_Third_Secret$Stock)
      colnames(Kyoko_Fukada_Unifies) <- c("Card","Edition","Rarity","Retail","Stock")
      Tokyos_Contents <- rbind(Tokyos_Contents, Kyoko_Fukada_Unifies)
      Tokyos_Contents <- unique(Tokyos_Contents)
    }
  } 
}
Tokyos_Safety <- Tokyos_Contents
#Tokyos_Contents <- Tokyos_Safety
#View(Tokyos_Contents)
Tokyos_Contents$Rarity <- ifelse(Tokyos_Contents$Rarity == "Mythic","M", ifelse(Tokyos_Contents$Rarity == "Rare", "R", ifelse(Tokyos_Contents$Rarity == "Uncommon", "U", ifelse(Tokyos_Contents$Rarity == "Common", "C", Tokyos_Contents$Rarity))))
Tokyos_Contents$Key <- paste(Tokyos_Contents$Card,Tokyos_Contents$Edition,Tokyos_Contents$Rarity, sep ="")
Tokyos_Contents <- Tokyos_Contents[moveme(names(Tokyos_Contents), "Key first")]

setwd("/home/cujo253/Reports/Tokyo")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Tokyo",".csv",sep="")
write.csv(Tokyos_Contents, file=csvFileName, row.names = FALSE)

#USA Comparison####
TodaysPremium <- paste("/home/cujo253/Reports/High Confidence Reps/",currentDate,"_Premium.csv", sep="")

USA_Data <- read_csv(TodaysPremium,col_types = cols(.default = "c"))

Tokyos_Contents$USA_Retail <- as.numeric(USA_Data$MKT)[match(Tokyos_Contents$Key,USA_Data$Key)]
Tokyos_Contents <- na.omit(Tokyos_Contents)
Tokyos_Contents$Arbit <- round(as.numeric(as.character(Tokyos_Contents$Retail)) - Tokyos_Contents$USA_Retail ,2)
Tokyos_Contents_Mkt <- Tokyos_Contents[order(Tokyos_Contents$Arbit),]


Tokyos_Contents$USA_BL <- as.numeric(USA_Data$BL)[match(Tokyos_Contents$Key,USA_Data$Key)]
Tokyos_Contents <- na.omit(Tokyos_Contents)
Tokyos_Contents$Arbit_BL <- round(as.numeric(as.character(Tokyos_Contents$Retail)) - Tokyos_Contents$USA_BL ,2)
Tokyos_Contents_BL <- Tokyos_Contents[order(Tokyos_Contents$Arbit_BL),]

#View(Tokyos_Contents_BL)

library(devtools)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(googledrive)
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)

ss <- drive_get("Japans MTG Market")
#sheets_deauth()

# sheet_write(
#   Tokyos_Contents_Mkt,
#   ss = ss,
#   sheet = "Market_Comparison"
# )
sheet_write(
  Tokyos_Contents_BL,
  ss = ss,
  sheet = "Market_vs_Buylist_Comparison"
)
