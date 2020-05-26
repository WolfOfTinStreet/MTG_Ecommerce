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

### Updated_Cron_Scrape.R
   |Lines    |                 Purpose                |
   |---------|----------------------------------------|
   |1-50     | Needed Packages and Functions          |
   |51-448   | Data Scraping (Post, Rvest, RSelenium) |
   |449-996  | Create Analytical Reports For Each Site|
   |997-1436 | Consolidate KPI's Down to 4            |
   |1437-1578| Compare TCG & CK Granularly            |
   |1579-1637| Create Personal Buylist on Cardsphere  |
### Market_Values.R
   |Lines    |                  Purpose                 |
   |---------|------------------------------------------|
   |1-75     | Needed Packages and Functions            |
   |77-129   | Determining Card Legality by Format      |
   |130-770  | Review Format Retail Avgs w/ ggplot      |
   |770-1457 | Review Format Buylist Avgs w/ ggplot     |
   |1458-1594| Merge Above Avgs w/ ggplot into one graph|
   
### Arima_Attempt.R
   |Lines    |                  Purpose                 |
   |---------|------------------------------------------|
   |1-55     | Needed Packages and Functions            |
   |56-154   | Prepare Data in Needed Time Series Format|
   |155-219  | Holts-Winter Forecast                    |
   |220-293  | Feed Forward Neural Network Forecast     |
   |294-374  | Arima Forecast                           |
   |375-475  | Join Forecasts, Choose By Lowest MAPE    |
   |476-531  | Growth/Decline List Based Off MAPE & RMSE|
   
### CK_Closed_System.R
   |Lines    |                  Purpose                 |
   |---------|------------------------------------------|
   |1-51     | Needed Packages and Functions            |
   |56-71    | Create Ranking Buckets for Buylist Demand|
   |72-117   | Request API Data & Apply Ranking Buckets |
   |118-166  | Scrape Cardkingdom Bestseller List       |
   |167-253  | Back Calculate Throughput Algorithm      |

### Tokyo.R
   |Lines    |                  Purpose                 |
   |---------|------------------------------------------|
   |1-38     | Needed Packages and Functions            |
   |41-239   | Scrape Major Japanese Vendor Tokyomtg    |
   |241-278  | Analyze against US Markets for Arbitrage |

### Scryfall_Acquisition.R & mtgjson_acquisition.R
   + Obtains base level card information for connecting scrapes between all websites. mtgjson vastly outperforms scryfall in terms of time to complete, but the option is yours.

### Bigquery_upload.R
   + Transfer data from CSV trees to Bigquery Database
### Decklist_Collector.R
   + Review daily outputs from Wizards of the Coast to gauge player demand.
  
