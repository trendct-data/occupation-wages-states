
# Median annual wage variance state to state

This repository contains data and methodlogy behind the Trend CT story:

*   [Would your job make you better or worse off in another state?](http://occupation.trendct.org/)

# Walkthroughs

* Adjusting income for inflation and cost of living
 - [[HTML](http://trendct-data.github.io/occupation-wages-states/)] [[RMarkdown](https://github.com/trendct-data/occupation-wages-states/blob/master/index.Rmd)] 
* Sample story generator (specifically Texas & Editors) - [[HTML](http://trendct-data.github.io/occupation-wages-states/story.html)] [[RMarkdown](https://github.com/trendct-data/occupation-wages-states/blob/master/story.Rmd)] 

### The Data

* The annual median wage data comes from the Bureau of Labor Statistic's Employment and Wages from Occupational Employment Statistics (OES) [survey](http://stats.bls.gov/oes/). The data [goes back](http://www.bls.gov/oes/tables.htm) to 1988 but occupation codes changed around 1999, so started there to keep things as "apples to apples"" as possible.
* The inflation data is also from the Bureau of Labor Statistics' Consumer Price [index](http://www.bls.gov/cpi/).
* State-to-state cost of living adjustments was made by determining the relative value of $100 based on Bureau of Economic Analysis' Regional Price Parities for the [years available](http://www.bea.gov/newsreleases/regional/rpp/rpp_newsrelease.htm): 2008-2014.

### What's in this repo

* `data` **folder** - Folder with raw BLS data and formated data used in this analysis
* `jobs-wages` **folder** - Folder of experimental Shiny app
* `stories` **folder** - Folder with programatically generated stories in markdown for every state
* `compiler.R` - Cleans and consolidates all the different BLS Excel sheets.
* `data_prep` - Just preps and cleans the 2015 data
* `story_generator.R` - Progromatically generates stories in markdown for every state and exports it to the stories folder

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. [Read more](http://www.trendct.org/data).

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct-data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.

This walkthrough explains the methodology behind adjusting income in the Trend CT story [Would your job make you better or worse off in another state?](http://occupation.trendct.org/)


