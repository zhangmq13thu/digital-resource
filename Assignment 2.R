# Assignment 2.1.
# New York Times provides free API services for historical news articles (back to 1851).
# Compare the changing popularities of (“China”, “Japan”, and “India”) in the 20th century.
# Please select your own keywords (>=2) for comparison and make a brief story based on your data.

# from the NYT API dashboard, obtain the API key 
api_key = "01eGc6c53t05vNFDVay96FqVgaGoQPrR"

# 1. identify API urls 
  # api example: "https://api.nytimes.com/svc/search/v2/articlesearch.json?q=election&api-key=yourkey"
  # we should also specify : (1) publication year: 20th cenury 
                          #  (2) keywords
                          #  (3) sort creteria: oldest first 

# filtered search in NYT website: https://www.nytimes.com/search?dropmab=true&endDate=19991231&query=china&sort=oldest&startDate=19000101&types=article

# construct API urls we like to use (with reference with the doc of NYT api):
api_url_china_1900 = "https://api.nytimes.com/svc/search/v2/articlesearch.json?q=china&fq=pub_year:(1900)&sort=oldest&types=article&api-key=01eGc6c53t05vNFDVay96FqVgaGoQPrR"

# copy the api_url_china_1900 to web browser; copy & paste the json code to online JSON editor to see how it looks like 

# check if the interpreted content by JSON editor is the same: YES  

# our aim value: the total number of related articles each year in the 20th century 
# so for api_url_china_1900: find out how many china related articles in 1900 
# check the website: Showing 3,675 results for china, so we need to find where to get the value [3,675]

# check the json editor, we can find that there is a meta element and the value for hits is [3675] 
# double check if the value of hits in the meta element is equal to the numbers of search results in the website
# YES!

# so the aim element is [hits] in [meta]

install.packages("RCurl")
library(RCurl)

page_China_1900 = getURL(api_url_china_1900)

install.packages("rjson")
library(rjson)
Frequency_China_1900=fromJSON(page_China_1900)$response$meta$hits
Frequency_China_1900

# write a loop to go through all years in 20th century 
# see how api_url is contructed 
url_part_1 = "https://api.nytimes.com/svc/search/v2/articlesearch.json?q=" 
# url_part_2 = china (country name)
url_part_3 ="&fq=pub_year:("
# url_part_4 = years 
url_part_5 = ")&sort=oldest&types=article&api-key=01eGc6c53t05vNFDVay96FqVgaGoQPrR"

# so api_url_china_1900 == paste0(url_part_1,"china",url_part_3,"1900",url_part_5)

# to get the yearly frequency of china japan & india in the 20th century, write a double loop 
df_all_country_all_year = data.frame()
for (country in c("China","Japan","India")) {
  for (year in 1900:1904) {
    api_url_country_year = paste0(url_part_1,country,url_part_3,year,url_part_5)
    page_country_year = getURL(api_url_country_year)
    freq_country_year = fromJSON(page_country_year)$response$meta$hits
    df_country_year = data.frame(country,year,freq_country_year)
    df_all_country_all_year = rbind(df_country_year,df_all_country_all_year)
    print(paste0("Frequency of ", country, " in year ",year," is ", freq_country_year))
  }
}

# we can see every 10 times, there will be an error message 
# that is because NYT has API limit
# so we need to set the loop on sleep for 60s every 10 times 

df_all_country_all_year = data.frame()
count = 0
for (country in c("China","Japan","India")) {
  for (year in 1900:1999) {
    api_url_country_year = paste0(url_part_1,country,url_part_3,year,url_part_5)
    page_country_year = getURL(api_url_country_year)
    freq_country_year = fromJSON(page_country_year)$response$meta$hits
    df_country_year = data.frame(country,year,freq_country_year)
    df_all_country_all_year = rbind(df_country_year,df_all_country_all_year)
    print(paste0("Frequency of ", country, " in year ",year," is ", freq_country_year))
    count = count + 1 
    if (count%%10 == 0) {
      Sys.sleep(60)
    }
  }
}

View(df_all_country_all_year)
# NOTE: this will take approximately 30 mins since we need to set the sleep time 



