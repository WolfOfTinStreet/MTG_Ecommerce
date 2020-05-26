# MTG_Ecommerce

Data Collection and Analysis for "Magic, the Gathering" (MTG) singles on the secondary market. Major trading card game websites such as TCGPlayer.com and Cardkingdom.com both present pricing and buylist information on a daily basis for all MTG cards. In these scripts we are scraping this pricing information, uploading to BigQuery, and analyzing the time series data that results to forecast retail, buylist, and inventory movements.

Global Markets also present price discrepancies. Different markets value different cards at varying degrees. By reviewing these discrepancies on a daily basis we are able to capitalize on opportuniteis between markets and profit just by recognizing them as they occur.

# Usage
All of these scripts are written in R (Rstudio), and most will require some familiarization with Docker containers to recreate.
For my Selenium servers, you will not be able to use my IP4's, you'll have to set up your own Docker containers and port into them.
If Unfamiliar this tutorial can give you the basics for Docker and RSelenium as used.

All "GMAIL" elements will need to be replaced with your personal gmail credentials



