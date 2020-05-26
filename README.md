# MTG_Ecommerce

Data Collection and Analysis for "Magic, the Gathering" (MTG) singles on the secondary market. Major trading card game websites such as TCGPlayer.com and Cardkingdom.com both present pricing and buylist information on a daily basis for all MTG cards. In these scripts we are scraping this pricing information, uploading to BigQuery, and analyzing the time series data that results to forecast retail, buylist, and inventory movements.

Global Markets also present price discrepancies. Different markets value different cards at varying degrees. By reviewing these discrepancies on a daily basis we are able to capitalize on opportuniteis between markets and profit just by recognizing them as they occur.

# Usage
All of these scripts are written in R (Rstudio), and most will require some familiarization with Docker containers to recreate.
For my Selenium servers, you will not be able to use my IP4's, you'll have to set up your own Docker containers and port into them.
If Unfamiliar this tutorial can give you the basics for Docker and RSelenium as used.

All "GMAIL" elements will need to be replaced with your personal gmail credentials

# Script Rundown
1) Updated_Cron_Scrape.R 
Will provide all base data sets that others will utilize. Scrapes the desired sites and returns the pricing, buylist, and inventory levels.

2) Market_Values.R
Demonstrates the Average Card Value in all major formats.

3) Arima_Attempt.R
Forecasting algorithms, Arima, Holts-Winter, and Feed Forward Neural Network for forecasting Buylist offers a week in advance. You will need the supplamental CSV provided altered to the start date of your data collection.

4) CK_Closed_System.R
Deep dive into Cardkingdom Deconstructing their throughput algorithm

5) Tokyo.R
Review Eastern Markets for global arbitrage opportunities

6) Scryfall_Acquisition.R & mtgjson_acquisition.R
Obtains base level card information for connecting scrapes between all websites. mtgjson vastly outperforms scryfall in terms of time to complete, but the option is yours.

7) Bigquery_upload.R
Transfer data from CSV trees to Bigquery Database

8) Decklist_Collector.R
Review daily outputs from Wizards of the Coast to gauge player demand.
