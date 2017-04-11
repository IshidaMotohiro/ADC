## 第16章 携帯電話のデータを集める
### 16.1 ページの探索
library(stringr)
library(XML)
library(RCurl)

# useragetがないとAmazonからRobot認定される
userAgent <- "eddie@r-datacollection.com"

#refに対するパラメータが変更されているため修正
#baseURL <- "https://www.amazon.com/s/ref=nb_sb_noss_2?url=node%3D2407749011&field-keywords="
baseURL <- "https://www.amazon.com/s/ref=sr_nr_n_12?url=node%3D2407749011&field-keywords="
keyword <- "Apple"
url <- str_c(baseURL, keyword)
firstSearchPage <- getURL(url, useragent=userAgent)
parsedFirstSearchPage <- htmlParse(firstSearchPage)
xpath <- str_c('//span[@class="refinementLink" and text()="', keyword, '"]/../@href')

restSearchPageLink <- xpathApply(parsedFirstSearchPage, xpath)
restSearchPageLink <- unlist(as.character(restSearchPageLink))
restSearchPageLink <- str_c("https://www.amazon.com", restSearchPageLink)
restSearchPageLink <- str_c(restSearchPageLink, "&sort=date-desc-rank")
restrictedSearchPage <- getURL(restSearchPageLink, useragent=userAgent)
SearchPages <- list()
SearchPages[[1]] <- restrictedSearchPage
xpath <- "//a[@class='pagnNext']/@href"
for (i in 2:5) {
  nextPageLink <- xpathApply(htmlParse(SearchPages[[i - 1]]), xpath)
  nextPageLink <- unlist(nextPageLink)
  nextPageLink <- str_c("https://www.amazon.com", nextPageLink)
  SearchPages[[i]] <- getURL(nextPageLink, useragent=userAgent)
}

# 16.1.2
#XPath変更にともない修正
extractTitle <- function(x) {
  #unlist(xpathApply(htmlParse(x), "//h3/a/span", xmlValue))
  unlist(xpathApply(htmlParse(x), "//a/h2", xmlValue))
}
titles <- unlist(lapply(SearchPages, extractTitle))
titles[1:3]


#XPath変更にともない修正
extractLink <- function(x) {
  #unlist(xpathApply(htmlParse(x), "//h3/a", xmlAttrs))
  unlist(xpathApply(htmlParse(x), "//a[@class='a-link-normal s-access-detail-page  a-text-normal']", xmlGetAttr, "href"))
}
links <- unlist(lapply(SearchPages, extractLink))
links[1:3]

chunk <- function(x, n) split(x, ceiling(seq_along(x)/n))
Links <- chunk(links, 10)
curl <- getCurlHandle()
ProductPages <- list()
for (i in 1:length(Links)) {
  ProductPages <- c(ProductPages, getURL(Links[[i]], useragent=userAgent))
  Sys.sleep(2)
}
ParsedProductPages <- lapply(ProductPages, htmlParse)


#XPath変更にともない修正
#修正を軽微なものとするため、下記のように直したが、セール商品についてはこれだけでは正しい価格が取得できない
extractPrice <- function(x) {
  #x <- xpathApply(x, "//span[@id=\"actualPriceValue\"]", xmlValue)
  x <- xpathApply(x, "//span[@id='priceblock_ourprice']", xmlValue)
  x <- unlist(x)
  x <- str_extract(x, "[[:digit:]]*\\.[[:digit:]]*")
  if (length(x) == 0)
    x <- NA
  return(as.numeric(x))
}
prices <- unlist(lapply(ParsedProductPages, extractPrice))
names(prices) <- NULL
prices[1:10]

extractStar <- function(x)
{
  x <- xpathApply(x, "//span[contains(@title, 'out of 5 stars')]", xmlValue)
  if (length(x) == 0) {
    x <- NA
  } else {
    x <- x[[1]]
    x <- str_extract(x, "[[:digit:]]\\.?[[:digit:]]?")
  }
  return(as.numeric(x))
}
stars <- unlist(lapply(ParsedProductPages, extractStar))
names(stars) <- NULL
stars[1:10]

extractRank <- function(x) {
  #x <- unlist(xpathApply(x, "//li[@id='SalesRank']", xmlValue))
  x <- unlist(xpathApply(x, "//span[contains(text(), 'in Cell Phones')]", xmlValue))
  x <- str_extract(x, "#.*?in")
  x <- str_replace_all(x, "[, in#]", "")
  if (length(x) == 0)
    x <- NA
  return(as.numeric(x))
}

ranks <- unlist(lapply(ParsedProductPages, extractRank))
names(ranks) <- NULL

# ASINを抜く前のXPathを修正
extractASIN <- function(x) {
  #x <- xpathApply(x, "//li/b[contains(text(), 'ASIN')]/../text()", xmlValue)
  x <- xpathApply(x, "//th[contains(text(), 'ASIN')]/../td", xmlValue)
  x <- str_trim(unlist(x))
  if (length(x) == 0)
    x <- NA
  return(x)
}
asins <- unlist(lapply(ParsedProductPages, extractASIN))
names(asins) <- NULL
asins[1:5]

extractModel <- function(x) {
  #xpath <- "//li/b[contains(text(), 'Item model number')]/../text()"
  xpath <- "//th[contains(text(), 'Item model number')]/../td"
  x <- xpathApply(x, xpath, xmlValue)
  x <- str_trim(unlist(x))
  if (length(x) == 0)
    x <- NA
  return(x)
}
models <- unlist(lapply(ParsedProductPages, extractModel))
# 見やすさのために訳者追記
names(models) <- NULL
models[1:5]

# 16.2
# 本文中で言及されているファイルは存在しない
# 代わりに 下記のファイルが使用できる
# https://github.com/crubba/Wiley-ADCR/blob/master/ch-16-amazon/ch-16-amazonScraper%202%20Functions.r
# が、このファイルもふるいため正しく動作しない
# 修正のため、下記のコードを実行すれば可能
# source("amazonScraperFunctions.r")
# source("ch-16-amazonScraper 2 Functions.r")

extractTitles <- function(SearchPages){
  titles <- unlist(lapply(SearchPages, extractTitle))
  names(titles) <- NULL
  titles
}

extractLinks <- function(SearchPages){
  links <- unlist(lapply(SearchPages, extractLink))
  names(links) <- NULL
  links
}

# 注意：ローカルにある場合はローカルから読むなどを機能は簡略化のため削除している
getProductPages <- function(links, brands){
  curl <- getCurlHandle()
  ProductPages <- list()
  for (i in 1:length(links)) {
    ProductPage <- getURL(links[[i]], useragent=userAgent)
    ProductPages <- c(ProductPages, ProductPage)
    fname <- str_c(brands[[i]], " ProductPage ", i, ".html")  
    writeLines(unlist(ProductPage), fname)
    Sys.sleep(2)
  }
  lapply(ProductPages, htmlParse)
}

extractStars <- function(ParsedProductPages){
  stars <- unlist(lapply(ParsedProductPages, extractStar))
  names(stars) <- NULL
  stars
}

extractASINs <- function(ParsedProductPages){
  unlist(lapply(ParsedProductPages, extractASIN))
}

extractModels <- function(ParsedProductPages){
  unlist(lapply(ParsedProductPages, extractModel))
}

extractRanks <- function(ParsedProductPages){
  unlist(lapply(ParsedProductPages, extractRank))
}

extractPrices <- function(ParsedProductPages){
  unlist(lapply(ParsedProductPages, extractPrice))
}

getSearchPages <- function(keyword, n)
{
  baseURL <- "https://www.amazon.com/s/ref=sr_nr_n_12?url=node%3D2407749011&field-keywords="
  url <- str_c(baseURL, keyword)
  firstSearchPage <- getURL(url, useragent=userAgent)
  parsedFirstSearchPage <- htmlParse(firstSearchPage)
  xpath <- str_c('//span[@class="refinementLink" and text()="', keyword, '"]/../@href')
  restSearchPageLink <- xpathApply(parsedFirstSearchPage, xpath)
  restSearchPageLink <- unlist(as.character(restSearchPageLink))
  restSearchPageLink <- str_c("https://www.amazon.com", restSearchPageLink)
  restSearchPageLink <- str_c(restSearchPageLink, "&sort=date-desc-rank")
  restrictedSearchPage <- getURL(restSearchPageLink, useragent=userAgent)
  SearchPages <- list()
  SearchPages[[1]] <- restrictedSearchPage
  xpath <- "//a[@class='pagnNext']/@href"
  for (i in 2:n) {
    nextPageLink <- xpathApply(htmlParse(SearchPages[[i - 1]]), xpath)
    nextPageLink <- unlist(nextPageLink)
    nextPageLink <- str_c("https://www.amazon.com", nextPageLink)
    SearchPages[[i]] <- getURL(nextPageLink, useragent=userAgent)
  }
  names(SearchPages) <- rep(keyword, length(SearchPages))
  SearchPages
}

### 16.2 スクレイピングの実施手順
forceDownload <- FALSE
KeyWords <- c("Apple", "BlackBerry", "HTC", "LG", "Motorola", "Nokia", "Samsung")
n <- 5

SearchPageList <- NULL
for (i in seq_along(KeyWords)) {
  message(KeyWords[i])
  SearchPageList <- c(SearchPageList, getSearchPages(KeyWords[i], n))
}
titles <- extractTitles(SearchPageList)
links <- extractLinks(SearchPageList)

# ブランド名がうまく取れないケースがあるので、元のコードを無視して修正
extractBrands <- function(SearchPages) {
  extractBrand <- function(x, brand){
    size <- length(unlist(xpathApply(htmlParse(x), "//a/h2", xmlValue)))
    rep(brand, size)
  }
  result <- unlist(mapply(extractBrand, SearchPages, names(SearchPages)))
  names(result) <- NULL
  result
}
#brands <- rep(KeyWords, each = n * 24)
brands <- extractBrands(SearchPageList)

productPages <- getProductPages(links, brands)
stars <- extractStars(productPages)
asins <- extractASINs(productPages)
models <- extractModels(productPages)
ranks <- extractRanks(productPages)
prices <- extractPrices(productPages)

fnames <- str_c(brands, " ProductPage ", seq_along(brands), ".html")
phones <- data.frame(brands, prices, stars, ranks, asins, models, titles, links, fnames,
                     timestamp = file.info(str_c("dataFull/", fnames))$ctime, stringsAsFactors = FALSE)
phones <- phones[complete.cases(phones), ]
phones <- phones[!duplicated(phones$asins), ]

### 16.3 グラフィカル分析
plotResults <- function(X, title="") {
  Prices <- X$prices
  Stars <- X$stars
  Ranks <- X$ranks
  plot(Prices, Stars, pch = 20, cex=10, col = "white",
       ylim = c(1,5), xlim = c(0, 1000), main = title,
       cex.main = 2, cex.axis = 2, xaxt = "n")
  axis(1, at=c(0, 500, 1000), labels = c(0, 500, 1000), cex.axis=2)
  # add guides
  abline(v=seq(0, 1000, 100), col = "grey")
  abline(h=seq(0, 5, 1), col = "grey")
  # adding data
  points(Prices, Stars, col=rgb(0, 0, 0, 0.2), pch = 20, cex = 7)
  # mark 5 highest values
  index <- order(Ranks)[1:5]
  abline(v = Prices[index], col="black")
  abline(h = Stars[index], col="black")
  points(Prices[index], Stars[index], col = rgb(1, 1,1,1), pch = 20)
}

#本文にはないので、Githubのコードを参照した
# define margins
par(mfrow=c(2,4))
par(mar=c(3,2,2,1))
par(oma=c(3, 4, 0, 1))
# do plotting
plotResults(phones, title="all")
for(i in seq_along(KeyWords)){
  plotResults( phones[phones$brands==KeyWords[i], ], title=KeyWords[i] )
}
# add common x and y-axis
mtext("costumer ratings\n",2,outer=T,cex=1.5)
mtext("prices",1,outer=T,cex=1.5)
#dev.off()

### 16.4 データの蓄積
library(RSQLite)
library(stringr)
# データベースを作成するに必要なコードはGithubから適宜補完している
# https://github.com/crubba/Wiley-ADCR/blob/master/ch-16-amazon/ch-16-amazonScraper%204%20Database.r
sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, "amazonProductInfo.db")

names(phones)
phones[1:3, 1:7]

createPhones <- function(con){
  if(!dbExistsTable(con,"phones")){
    sql <- "CREATE TABLE phones (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    asin CHAR,
    UNIQUE(asin) ON CONFLICT IGNORE);"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

createProducers <- function(con){
  if(!dbExistsTable(con, "producers")){
    sql <- "CREATE TABLE producers (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
    producer CHAR,
    UNIQUE(producer) ON CONFLICT IGNORE);"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

# function for creating a ... table within the database
createModels <- function(con){
  if(!dbExistsTable(con, "models")){
    sql <- "CREATE TABLE models (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
    model CHAR,
    UNIQUE(model) ON CONFLICT IGNORE);"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

# function for creating a ... table within the database
createLinks <- function(con){
  if(!dbExistsTable(con, "links")){
    sql <- "CREATE TABLE links (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
    link        TEXT);"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}



createItems <- function(con){
  if(!dbExistsTable(con,"items")){
    sql <- "CREATE TABLE items (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    price REAL,
    stars REAL,
    rank INTEGER,
    title TEXT,
    fname TEXT,
    time TEXT,
    phones_id INTEGER NOT NULL REFERENCES phones(id)
    ON UPDATE CASCADE,
    producers_id INTEGER NOT NULL REFERENCES producers(id)
    ON UPDATE CASCADE,
    models_id INTEGER NOT NULL REFERENCES models(id)
    ON UPDATE CASCADE,
    links_id INTEGER NOT NULL REFERENCES links(id)
    ON UPDATE CASCADE,
    UNIQUE(
    price, stars, rank, time,
    phones_id, producers_id, models_id, links_id
    ) ON CONFLICT IGNORE);"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

createReviews <- function(con){
  if(!dbExistsTable(con,"reviews")){
    sql <- "CREATE TABLE reviews (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    asin TEXT,
    stars INTEGER,
    helpfulyes INTEGER,
    helpfulno INTEGER,
    helpfulsum INTEGER,
    date TEXT,
    title TEXT,
    text TEXT,
    UNIQUE(asin, stars, date, title, text) ON CONFLICT IGNORE);"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

createReviewsMeta <- function(con){
  if(!dbExistsTable(con,"reviewsMeta")){
    sql <- "CREATE TABLE reviewsMeta (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    asin TEXT,
    one INTEGER,
    two INTEGER,
    three INTEGER,
    four INTEGER,
    five INTEGER,
    UNIQUE(asin) ON CONFLICT REPLACE);"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

createViewItemData <- function(con){
  if(!dbExistsTable(con,"ItemData")){
    sql <- "CREATE VIEW ItemData AS
    SELECT items.id as itemid, price as itemprice,
    stars as itemstars, rank as itemrank,
    title as itemtitle, model, phones.asin as asin,
    producer from items
    JOIN producers on producers_id = producers.id
    JOIN models on models_id = models.id
    Join phones on phones_id = phones.id;"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

createViewReviewData <- function(con){
  if(!dbExistsTable(con,"ReviewData")){
    sql <- "CREATE VIEW ReviewData AS
    SELECT phones.asin, reviews.id as reviewid,
    stars as reviewstars, one as allrev_onestar,
    two as allrev_twostar, three as allrev_threestar,
    four as allrev_fourstar, five as allrev_fivestar,
    helpfulyes, helpfulno, helpfulsum, date as reviewdate,
    title as reviewtitle, text as reviewtext
    FROM phones
    JOIN reviews on phones.asin=reviews.asin
    JOIN reviewsMeta on phones.asin=reviewsMeta.asin;"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

createViewAllData <- function(con){
  if(!dbExistsTable(con,"AllData")){
    sql <- "CREATE VIEW AllData AS
    SELECT * FROM ItemData
    JOIN ReviewData on ItemData.asin = ReviewData.asin"
    dbGetQuery(con, sql)
  }else{
    message("table already exists")
  }
}

defineDatabase <- function(con){
  createPhones(con)
  createProducers(con)
  createModels(con)
  createLinks(con)
  createItems(con)
  createReviews(con)
  createReviewsMeta(con)
  createViewItemData(con)
  createViewReviewData(con)
  createViewAllData(con)
}

dropAll <- function(con){
  sql <- "select 'drop table ' || name || ';' from sqlite_master where type = 'table';"
  tmp <- grep("sqlite_sequence",unlist(dbGetQuery(con,sql)), value=T,invert=T)
  for(i in seq_along(tmp)) dbGetQuery(con,tmp[i])
  sql <- "select 'drop view ' || name || ';' from sqlite_master where type = 'view';"
  tmp <- grep("sqlite_sequence",unlist(dbGetQuery(con,sql)), value=T,invert=T)
  for(i in seq_along(tmp)) dbGetQuery(con,tmp[i])
}

addASINs <- function(x, con){
  message("adding phones ...")
  asinsInDB <- unlist(dbReadTable(con,"phones")["asin"])
  asinsToAdd <- unique(x$asins[!(x$asins %in% asinsInDB)])
  for(i in seq_along(asinsToAdd)){
    sql <- str_c("INSERT INTO phones (asin) VALUES ('", asinsToAdd[i], "') ;")
    dbGetQuery(con, sql)
  }
}

# add producers not already in DB
addProducers <- function(x, con){
  message("adding producers ...")
  producersInDB  <- unlist(dbReadTable(con,"producers")["producer"])
  producersToAdd <- unique(x$brands[!(x$brands %in% producersInDB)])
  for(i in seq_along(producersToAdd)){
    sql <- str_c("INSERT INTO producers (producer) VALUES ('",
                 producersToAdd[i], "') ;")
    dbGetQuery(con, sql)
  }
}

# add models not already in DB
addModels <- function(x, con){
  message("adding models ...")
  modelsInDB  <- unlist(dbReadTable(con,"models")["model"])
  modelsToAdd <- unique(x$model[!(x$model %in% modelsInDB)])
  for(i in seq_along(modelsToAdd)){
    sql <- str_c("INSERT INTO models (model) VALUES ('",
                 str_replace_all(modelsToAdd[i], "'", "''"),
                 "') ;")
    dbGetQuery(con, sql)
  }
}

# add links not already in DB
addLinks <- function(x, con){
  message("adding links ...")
  linksInDB  <- unlist(dbReadTable(con,"links")["link"])
  linksToAdd <- unique(x$link[!(x$link %in% linksInDB)])
  for(i in seq_along(linksToAdd)){
    sql <- str_c("INSERT INTO links (link) VALUES ('", linksToAdd[i], "') ;")
    dbGetQuery(con, sql)
  }
}



addItems <- function(x, con){
  message("adding items ... ")
  # get fresh infos from db
  Phones <- dbReadTable(con, "phones")
  Producers <- dbReadTable(con, "producers")
  Models <- dbReadTable(con, "models")
  Links <- dbReadTable(con, "links")
  for(i in seq_along(x[,1])){
    priceDB <- x$price[i]
    starsDB <- x$stars[i]
    rankDB <- x$rank[i]
    titleDB <- str_replace(x$titles[i], "'", "''")
    fnameDB <- x$fname[i]
    timeDB <- x$timestamp[i]
    phonesDB <- Phones$id[Phones$asin %in% x$asin[i]]
    producersDB <- Producers$id[Producers$producer %in% x$brand[i]]
    modelsDB <- Models$id[Models$model %in% x$model[i]]
    linksDB <- Links$id[Links$link %in% x$link[i]]
    sql <- str_c(" INSERT INTO items
                 (price, stars, rank, title, fname, time,
                 phones_id, producers_id, models_id, links_id)
                 VALUES
                 ('",
                 str_c( priceDB, starsDB, rankDB,
                        titleDB, fnameDB, timeDB,
                        phonesDB, producersDB, modelsDB, linksDB,
                        sep="', '"),
                 "'); ")
    dbGetQuery(con, sql)
  }
}

saveInDatabase <- function(x, DBname){
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite, DBname)
  defineDatabase(con)
  addASINs(x, con)
  addProducers(x, con)
  addModels(x, con)
  addLinks(x, con)
  addItems(x, con)
  dbDisconnect(con)
}

saveInDatabase(na.omit(phones), "amazonProductInfo.db")
sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, "amazonProductInfo.db")
res <- dbReadTable(con, "phones")
dim(res)
res[1:3, ]
