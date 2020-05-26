# MTG_Ecommerce

Forecast retail, buylist, and inventory movements of _Magic, the Gathering_ trading cards.

Major trading card game websites like [tcg player](http://tcgplayer.com) and [card kingdom](http://cardkingdom.com) publish pricing and buylist information of _Magic, the Gathering_ cards in [secondary markets](https://en.wikipedia.org/wiki/Secondary_market). These scripts scrape and analyze their publications to forecast retail, buylist, and inventory movements of _Magic, the Gathering_ trading cards. Automating the collection of price discrepancies between different market's valuation of specific _Magic, the Gathering_ trading cards creates opportunities for profit.

# Usage

All of these scripts are written in R (Rstudio), and most will require some familiarization with Docker containers to recreate.

```R
remDr = remoteDriver(remoteServerAddr = "IP4", port = 4445L, browser = "chrome")
```

Above IP4 will need to be inputed according to your own Docker Containers.

```R
drive_auth(email = "GMAIL")
drive_auth(use_oob=TRUE)
```

All "GMAIL" elements will need to be replaced with your personal gmail credentials to read and write from your drive.

```R
username$sendKeysToElement(list("EMAIL"))
passwd$sendKeysToElement(list("PASSWORD"))
```
Login credentials for specific sites will also be required.

# Script Order

## Updated_Cron_Scrape.R
   + Will provide all base data sets that others will utilize. Scrapes the desired sites and returns the pricing, buylist, and inventory levels.
## Market_Values.R
   + Demonstrates the Average Card Value in all major formats.

## Arima_Attempt.R
   + Forecasting algorithms, Arima, Holts-Winter, and Feed Forward Neural Network for forecasting Buylist offers a week in advance. You will need the supplamental CSV provided altered to the start date of your data collection.

## CK_Closed_System.R
   + Deep dive into Cardkingdom Deconstructing their throughput algorithm

## Tokyo.R
   + Review Eastern Markets for global arbitrage opportunities
## Scryfall_Acquisition.R & mtgjson_acquisition.R
   + Obtains base level card information for connecting scrapes between all websites. mtgjson vastly outperforms scryfall in terms of time to complete, but the option is yours.

## Bigquery_upload.R
   + Transfer data from CSV trees to Bigquery Database
## Decklist_Collector.R
   + Review daily outputs from Wizards of the Coast to gauge player demand.
  
