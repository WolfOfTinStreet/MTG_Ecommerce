# MTG_Ecommerce

Data Collection and Analysis for "Magic, the Gathering" (MTG) singles on the secondary market. Major trading card game websites such as TCGPlayer.com and Cardkingdom.com both present pricing and buylist information on a daily basis for all MTG cards. In these scripts we are scraping this pricing information, uploading to BigQuery, and analyzing the time series data that results to forecast retail, buylist, and inventory movements.

# Usage
Let's start with the order of usage and some overarching guidelines:

All "GMAIL" elements will need to be replaced with your personal gmail credentials
For my Selenium servers, you will not be able to use my IP4's, you'll have to set up your own Docker containers and port into them.
In the interest of making it easy for you, check out this tutorial video:
https://www.youtube.com/watch?v=OxbvFiYxEzI&t=4777s
And start watching at 1:06:00
Does a great job of breaking down RSelenium (Rvest too, if you're completely unfamiliar (a BeautifulSoup comparable)) as well as showing how to set up a docker instance (Ubuntu! If you're on windows it gets a little more complicated) without really needing to understand what it is you're actually doing.

Now! Starting with Updated_Cron_Scrape.R
This gathers Cardkingdom buylist info, as well as best selling on TCG and CK.
Despite changing their website to remove the ability to actually sort by best selling, TCG has a glaring "whoopsie" wherein you can still recreate a json element to query their API underneath to obtain that sales rate. Not included in these scripts, but this methodology also unlocks a world of other opportunities on other sites and buylists (hint hint).

After collecting all of the information you'll see my growth analsysis which uses what I personally have found to be the best indicators of growth.

You'll also notice there's a "Funny Money" section which illustrates the best areas for investing store credit, in an effort to maximize that extra 30% Cardkingdom offers on trade ins.

All Elements of these data scrapes have their own CSV trees saved across 6 of my Droplets, but also, utilizing the Bigquery_upload.R, you can loop them up to a SQL db in the cloud. Also, highly recommend cron jobing backups to github repos for those pesky DDoS attacks and well, accidental deletions...

Metric aggregation pulls the key metric that I'll be running all my time series analysis on into consolidated files, and also allows for quick pulls on price histories.

Lastly, I have a little bit for analyzing CK buy list offers and finding cards listed at retail below that (for rookies, this is sheer arbitrage) and my last bit of code puts out offers on these, and in another script I aggregate the cards sent to me and ship off the buylist order without ever having to see what I bought. (Automation is super cool, aye?)

Next! Market Values.R
This is one that could be done a lot* faster if I just kept past histories into a single file and update daily instead of recreating from scrath every day, but honestly, I don't find it that useful. Folks LOVE the visuals from it though so I begrudgingly keep it around.

Next! We get our crystal balls out with the Arima_Attempt.R
This started as an Arima attempt and the name stuck, but it's actually Arima, Holts Winter and a FFNN time series forecast. This one is solely based of buylist values, but hey if you look at how metric aggregation is working in the above script, you should realize that you can apply this to all* those metrics. Maybe even integrate them... into an overall forecast... but that's crazy talk.

You will also need the supplamental CSV provided to plan out the dates indicating what you'd like to forecast for. Obviously if you begin tracking this data yourself, you will need to alter these dates, but this templating should serve as a solid foundation for you.

CK_Closed_System.R
Well, if they're fancy enough to use a "throughput algo" to determine their offers, we're smart enough to discover their sales velocity! Even if they do try and hide their inventory levels, it's a fairly easy back calculation, but nobody tell them that.

mtgjson_acquisition.R & Scryfall_Acquisiton.R
These both perform a similar task, but you'll definitely need some avenue for building an initial Key to connect sites by. I prefer mtgjson, but who am I to judge, scryfal there as well. I will note, scryfall will take 8 hours to pull in unless you're into parallel processing or threading. Personally, mtgjson takes 10 minutes, so I'm all for occam's razor.

Tokyo.R
Fortunately, being me, I have friends who have the EU pretty well mapped out already. So I get to focus my efforts on the Eastern Markets! Obvious global price discrepancies as different markets value different cards (to fully go into this I would need a masterclass... alas... goals)

Decklists.R
Wizards of the Coast releases daily decklists results which a large number of players review and often build paper decks around. It often helps explain the "Why?" as to card price movements when it seems very confusing otherwise. Don't forget people actually play this game. Crazy, I know. I would also suggest, after a few months of gathering this data (or back tracking, take 20 minutes to get the prior 3 months history worth of decklists, and if you wanted to go back even further....) you can actually, albeit from these curated* results, define meta's in a way that I don't think anyone else has done besides myself. 

But there you have it! 

A starter's guide to breaking into a secondary trading card game market that no one seems to really care to do. I've heard from the outset that you can't make money in this game by speculating on singles but uh... I think utilizing the above you may discover differently.
