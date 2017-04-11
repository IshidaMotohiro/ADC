# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 9 章 Web からのスクレイピング

### 9.1 収集のシナリオ
library(RCurl)
library(XML)
library(stringr)

### 9.1.1 すでに整形されたデータのダウンロード
#### 9.1.1.1 CSV フォーマットによる選挙データ
url <- "http://www.elections.state.md.us/elections/2012/election_data/index.html"
page_parse <- htmlParse(url, encoding = "utf-8")
links <- getHTMLLinks(url)
filenames <- links[str_detect(links, "_General.csv")]
filenames_list <- as.list(filenames)
filenames_list[1:3]

##### ダウンロード用の関数を定義
downloadCSV <- function(filename, baseurl, folder) {
  dir.create(folder, showWarnings = FALSE)
  fileurl <- str_c(baseurl, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    download.file(fileurl, destfile = str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}

###### 関数の実行
l_ply(filenames_list, downloadCSV,
      baseurl = "http://www.elections.state.md.us/elections/2012/election_data/",
      folder = "elec12_maryland")


length(list.files("./elec12_maryland"))
list.files("./elec12_maryland")[1:5]

#### 9.1.1.2 PDF で保存された選挙区地図
url <- "http://planning.maryland.gov/Redistricting/2010/legiDist.shtml"
links <- getHTMLLinks(url)
filenames <- links[str_detect(links, "2010maps/Leg/Districts_")]
filenames_list <- str_extract_all(filenames, "Districts.+pdf")

##### RCurl によるダウンロード
downloadPDF <- function(filename, baseurl, folder, handle) {
  dir.create(folder, showWarnings = FALSE)
  fileurl <- str_c(baseurl, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    pdf_file <- getBinaryURL(fileurl, curl = handle)
    writeBin(pdf_file, str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}

handle <- getCurlHandle(useragent = str_c(R.version$platform,
                                          R.version$version.string, sep=", "),
                        httpheader = c(from = "eddie@datacollection.com"))

l_ply(filenames_list, downloadPDF,
      baseurl = "planning.maryland.gov/PDF/Redistricting/2010maps/Leg/",
      folder = "elec12_maryland_maps",
      handle = handle)

length(list.files("./elec12_maryland_maps"))
list.files("./elec12_maryland_maps")[1:3]

### 9.1.2 FTP インデックスからの複数のファイルのダウンロード
##### FTP ディレクトリの読み取り
ftp <- 'ftp://cran.r-project.org/pub/R/web/views/'
ftp_files <- getURL(ftp, dirlistonly = TRUE)
ftp_files

##### ファイル名の抽出
filenames <- strsplit(ftp_files, "\n")[[1]]
filenames_html <- unlist(str_extract_all(filenames, ".+(.html)"))
filenames_html[1:3]

filenames_html <- getURL(ftp, customrequest = "NLST *.html")
filenames_html = strsplit(filenames_html, "\\n")[[1]]

##### ファイルのダウンロード
downloadFTP <- function(filename, folder, handle) {
  dir.create(folder, showWarnings = FALSE)
  fileurl <- str_c(ftp, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    datafile <- try(getURL(fileurl, curl = handle))
    write(datafile, str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}

handle <- getCurlHandle(ftp.use.epsv = FALSE)
l_ply(filenames_html, downloadFTP, folder = "cran_tasks", handle = handle)

length(list.files("./cran_tasks"))
list.files("./cran_tasks")[1:3]

ftpUpload(what = "example.txt", to = "ftp://example.com", userpwd = "username:password")

### 9.1.3 複数のページにアクセスするために URL を操作する
##### URL 操作
baseurl <- htmlParse("http://www.transparency.org/news/pressreleases/year/2010")
xpath <- "//div[@id='Page']/strong[2]"
total_pages <- as.numeric(xpathSApply(baseurl, xpath, xmlValue))
total_pages

max_url <- (total_pages - 1) * 10
add_url <- str_c("/P", seq(10, max_url, 10))
add_url

getPageURLs <- function(url){
  baseurl <- htmlParse(url)
  total_pages <- as.numeric(xpathSApply(baseurl, "//div[@id='Page']/strong[2]", xmlValue))
  max_url <- (total_pages - 1) * 10
  add_url <- str_c("/P", seq(10, max_url, 10))
  urls_list <- as.list(str_c(url, add_url))
  urls_list[length(urls_list) + 1] <- url
  return(urls_list)
}

url <- "http://www.transparency.org/news/pressreleases/year/2010"
urls_list <- getPageURLs(url)
urls_list[1:3]

##### リストされたページのダウンロード
dlPages <- function(pageurl, folder ,handle) {
  dir.create(folder, showWarnings = FALSE)
  page_name <- str_c(str_extract(pageurl, "/P.+"), ".html")
  if (is.na(page_name)) { page_name <- "/base.html" }
  if (!file.exists(str_c(folder, "/", page_name))) {
    content <- try(getURL(pageurl, curl = handle))
    write(content, str_c(folder, "/", page_name))
    Sys.sleep(1)
  }
}

handle <- getCurlHandle()
l_ply(urls_list, dlPages, folder = "tp_index_2010", handle = handle)

list.files("tp_index_2010")[1:3]

getPressURLs <- function(folder) {
  pages_parsed <- lapply(str_c(folder, "/", dir(folder)), htmlParse)
  urls <- unlist(llply(pages_parsed, getHTMLLinks))
  press_urls <- urls[str_detect(urls, "http.+/pressrelease/")]
  press_urls_list <- as.list(press_urls)
  return(press_urls_list)
}

press_urls_list <- getPressURLs(folder = "tp_index_2010")
length(press_urls_list)

##### プレスリリースのダウンロード
# dlPress function
dlPress <- function(press_url, folder, handle) {
  dir.create(folder, showWarnings = FALSE)
  press_filename <- str_c(str_extract(press_url,
                                      "[^//][[:alnum:]_.]+$"),
                          ".html")
  if (!file.exists(str_c(folder, "/", press_filename))) {
    content <- try(getURL(press_url, curl = handle))
    write(content, str_c(folder, "/", press_filename))
    Sys.sleep(1)
  }
}

handle <- getCurlHandle()
l_ply(press_urls_list, dlPress, folder = "tp_press_2010", handle = handle)
length(list.files("tp_press_2010"))					 

### 9.1.4 リンクやリスト，評を HTML ドキュメントから取得する便利な関数
##### リンクの抽出
mac_url <- "https://en.wikipedia.org/wiki/Machiavelli"
mac_source <- readLines(mac_url, encoding = "UTF-8")
mac_parsed <- htmlParse(mac_source, encoding = "UTF-8")
mac_node   <- mac_parsed["//p"][[1]]

getHTMLLinks(mac_source)[1:3]
getHTMLLinks(mac_parsed)[1:3]
getHTMLLinks(mac_node)[1:3]

getHTMLLinks(mac_source, xpQuery = "//a[@class='extiw']/@href")[1:3]

xpath <- "//img[contains(@src, 'Machiavelli')]/@src"
getHTMLExternalFiles(mac_source, xpQuery = xpath)[1:3]

##### リスト抽出
readHTMLList(mac_source)[[10]][1:3]

##### 表の抽出
tbls <- readHTMLTable(mac_source, stringsAsFactors = F)
names(tbls)
tbls[[1]][2:6, 1:2]

##### elFun 引数の指定
tbls[[1]][9:10, 1]
[1] "Influences\n\nPlato, Aristotle, Socrates, St. Augustine, St. Thomas Aquinas, Dante Alighieri, Xenophon, Plutarch, Tacitus, Polybius, Cicero, Sallust, Livy, Thucydides"                                                                                                               
[2] "Influenced\n\nPolitical Realism, Bacon, Hobbes, Harrington, Rousseau, Vico, Edward Gibbon, David Hume, John Adams, Vincenzo Cuoco, Nietzsche, Pareto, Gramsci, Althusser, T. Schelling, Negri, Waltz, Baruch de Spinoza, Denis Diderot, Carl Schmitt, Giulio Andreotti, Philip Pettit"

influential <- readHTMLTable(mac_source,  elFun = getHTMLLinks, stringsAsFactors = FALSE)[[1]][9, ]
as.character(influential)[1:3]
influenced <- readHTMLTable(mac_source, elFun = getHTMLLinks, stringsAsFactors = FALSE)[[1]][10,]
as.character(influenced)[1:3]

### 9.1.5 HTML フォームを処理する
##### 準備
setwd("scenario-forms")

# setup
info   <- debugGatherer()
handle <- getCurlHandle(cookiejar = "", 
                        followlocation = TRUE, 
                        autoreferer = TRUE,
                        debugfunc = info$update,
                        verbose = TRUE,
                        httpheader = list(
                             from  = "Eddie@r-datacollection.com",
                             'user-agent' = str_c(R.version$version.string, ", ", R.version$platform)
                          )
                        )

xmlAttrsToDF <- function(parsedHTML, xpath){
  x <- xpathApply(parsedHTML, xpath, xmlAttrs)
  x <- lapply(x, function(x) as.data.frame(t(x)))
  do.call(rbind.fill, x)
}

#### 9.1.5.1 フォームから GET を行う
url <- "http://wordnetweb.princeton.edu/perl/webwn"
html_form <- getURL(url, curl = handle)
parsed_form <- htmlParse(html_form)

xmlAttrsToDF(parsed_form, "//form")
xmlAttrsToDF(parsed_form, "//form[1]/input")

html_form_res   <- getForm(uri = url, curl = handle, s = "data")
parsed_form_res <- htmlParse(html_form_res)
xpathApply(parsed_form_res,"//li", xmlValue)

cat(str_split(info$value()["headerOut"], "\r")[[1]])
info$reset()

url <- "http://wordnetweb.princeton.edu/perl/webwn?s=data"
html_form_res <- getURL(url = url, curl = handle)

# query <- "data"
# wordnetFun <- function(query){
#   url <- sprintf("http://wordnetweb.princeton.edu/perl/webwn?s=%s&sub=Search+WordNet", query)
#   getURL(url)
# }

#### 9.1.5.2 POST を行うフォーム
url <- "http://www.webpagefx.com/tools/read-able/"
form <- htmlParse(getURL(url = url, curl = handle))

##### POST フォームの確認
xmlAttrsToDF(form, "//form")  
xmlAttrsToDF(form, "//form[2]//input")
xpathApply(form, "//form")[2]

sentence <- '"It is a capital mistake to theorize before one has data. Insensibly one begins to twist facts to suit theories, instead of theories to suit facts." - Arthur Conan Doyle, Sherlock Holmes'

##### R で POST フォームを指定する
res <- postForm(uri=str_c(url, "check.php"), 
                curl=handle,
                style="POST",
                directInput=sentence)

readHTMLTable(res)

cat(str_split(info$value()["headerOut"],"\r")[[1]])
cat(str_split(info$value()["dataOut"],"\r")[[1]])
info$reset()

##### マルチパートエンコードされた POST
url <- "r-datacollection.com/materials/http/sky.png"
sky <- getBinaryURL(url=url)
writeBin(sky, "sky.png")

url   <- "http://www.fixpicture.org/"
form  <- htmlParse(getURL(url = url, curl = handle))

# get form specs
xmlAttrsToDF(form, "//form")
xmlAttrsToDF(form, "//input")[1:2 ,c("name","type","class","value")]
xmlAttrsToDF(form, "//select")
xmlAttrsToDF(form, "//select/option")

# send picture
res <- postForm(uri = "http://www.fixpicture.org/resize.php?LANG=en", 
                 image = fileUpload(filename = "sky.png", 
                                    contentType = "image/png"),
                 format = "pdf",
                 curl = handle)

doc  <- htmlParse(res)
link <- str_c(url, xpathApply(doc,"//a/@href", as.character)[[1]])

# store image
resImage <- getBinaryURL(link, curl=handle)
writeBin(resImage,"sky.pdf",useBytes=T)

cat(str_split(info$value()["dataOut"], "\r")[[1]])
info$reset()

#### 9.1.5.3 フォームの操作を自動化する RTHMLForms パッケージ
install.packages("RHTMLForms", repos ="http://www.omegahat.net/R",  type  ="source")
library(RHTMLForms)

url <- "http://wordnetweb.princeton.edu/perl/webwn"
forms <- getHTMLFormDescription(url)
formFunction <- createFunction(forms[[1]])

html_form_res   <- formFunction(s = "data", .curl = handle)
parsed_form_res <- htmlParse(html_form_res)
xpathApply(parsed_form_res,"//li", xmlValue)

args(formFunction)

### 9.1.6 HTTP 認証
url <- "www.r-datacollection.com/materials/solutions"
cat(getURL(url, userpwd = "teacher:sesame", followlocation = TRUE))

##### パスワードの保存
options(RDataCollectionLogin = "teacher:sesame")
cat(getURL(url, userpwd = getOption("RDataCollectionLogin"), followlocation = TRUE))


### 9.1.7 HTTPS による接続
u <- "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/search"
u_action <- "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/variables?"

signatures = system.file("CurlSSL", "cacert.pem", package = "RCurl")
res <- getURL(u, cainfo = signatures)

res <- getURL(u, ssl.verifypeer = FALSE)

u_action <- "https://www.icpsr.umich.edu/icpsrweb/ICPSR/ssvd/variables?"
handle <- getCurlHandle(cainfo = signatures)
res <- getForm(u_action, variableLabel="climate+change", questionText="", categoryLabel="", curl = handle)
str_extract(res, "Your query returned [[:digit:]]+ variables")

### 9.1.8 Cookies の利用
info   <- debugGatherer()
handle <- getCurlHandle(cookiejar      = "", 
                           followlocation = TRUE, 
                           autoreferer    = TRUE,
                           debugfunc      = info$update,
                           verbose        = TRUE,
                           httpheader     = list(
                             from         = "eddie@r-datacollection.com",
                             'user-agent' = str_c(R.version$version.string, 
                                              ", ", R.version$platform)
                           ))

#### 9.1.8.1 オンラインショッピングのカートに投入する
search_url <- "www.biblio.com/search.php?keyisbn=data"
cart_url   <- "www.biblio.com/cart.php"

search_page <- htmlParse(getURL(url = search_url, curl = handle))

xpathApply(search_page, "//form[@class='ob-add-form']")

xpath <- "//form[@class='ob-add-form']/input[@name='bid']/@value"
bids <- unlist(xpathApply(search_page, xpath, as.numeric))

##### Cookie 情報のあるリクエスト
for(i in seq_along(bids)) {
	res <- getForm(uri = cart_url, 
	                 curl = handle, 
	                 bid = bids[i], 
	                 add = 1, 
	                 int = "keyword_search")
}

cart  <- htmlParse(getURL(url=cart_url, curl=handle))
clean <- function(x)  str_replace_all(xmlValue(x),"(\t)|(\n\n)","")
cat(xpathSApply(cart, "//h3", clean))

cat(str_split(info$value()["headerOut"],"\r")[[1]][1:13])

cat(str_split(info$value()["headerIn"],"\r")[[1]][1:14])

cart <- htmlParse(getURL(url = cart_url, curl = handle, cookielist = "ALL"))
clean <- function(x)  str_replace_all(xmlValue(x),"(\t)|(\n\n)","")
cat(xpathSApply(cart, "//h3", clean))

#### 9.1.8.2 Cookie の交換を工夫する
handle <- getCurlHandle(cookiejar = "cookies.txt")
getURL("http://httpbin.org/cookies/set?k1=v1&k2=v2", curl = handle)
handle <- curlSetOpt(cookielist = "FLUSH", curl = handle)
readLines("cookies.txt")

new_handle <- getCurlHandle(cookiejar = "cookies.txt", cookiefile = "cookies.txt")
getURL("http://httpbin.org/cookies", curl = new_handle, cookielist = "ALL")
getURL("http://httpbin.org/cookies", cookie = "name=Eddie;age=32")

### 9.1.9 Selenium と Rwebdriver を用いた AJAX による Web ページからのデータスクレイピイング
#### 9.1.9.2 Selenium と Rwebdriver パッケージ
library(devtools)
install_github(repo = "crubba/Rwebdriver")

##### Selenium Webdriver を使ってみる
library(Rwebdriver)
library(XML)

start_session(root = "http://localhost:4444/wd/hub/", browser = "firefox")

##### Web ページへのアクセス
post.url(url = "http://www.r-datacollection.com/materials/selenium/intro.html")

##### 現在の URL の取得
get.url()

##### ページタイトルの取得
page_title()

##### クリック動作
buttonID <- element_xpath_find(value = "/html/body/div/div[2]/form/input")

element_click(ID = buttonID)

##### ウィンドウ操作
allHandles <- window_handles()

##### 要素の識別
window_change(allHandles[1])

yearID <- element_xpath_find(value = '//*[@id="yearSelect"]')
monthID <- element_xpath_find(value = '//*[@id="monthSelect"]')
recipID <- element_xpath_find(value = '//*[@id="recipientSelect"]')
                                 
element_click(yearID)

##### キーボード操作
keys("2013")

element_click(monthID)
keys("January")
element_click(recipID)
keys("barack Obama")

submitID <- element_xpath_find(value = '//*[@id="yearForm"]/div/button')
element_click(submitID)

##### ソースコードへのアクセス
pageSource <- page_source()
moneyTab <- readHTMLTable(pageSource, which = 1)

colnames(moneyTab) <- c("year", "name", "party", "contributor", "state", "amount")
moneyTab <- moneyTab[-1, ]
head(moneyTab)


### 9.1.10 API からのデータ収集
##### 天気予報 API
install.packages("jsonlite")
library(jsonlite)
api_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
weather_url <- "http://api.openweathermap.org/data/2.5/weather"
city <- "chiyoda-ku,jp"
now.weather <- fromJSON(getForm(weather_url, q = city, appid = api_key, units = "metric"))

now.weather$weather
now.weather$main$temp
now.weather$main$humidity

forecast_url <- "http://api.openweathermap.org/data/2.5/forecast"
city <- "chiyoda-ku,jp"
forecasts <- fromJSON(getForm(forecast_url, q = city, appid = api_key, units = "metric"))
names(forecasts)
names(forecasts$list)
head(forecasts$list$weather)

head(forecasts$list$main$temp)

head(forecasts$list$dt_txt)
head(as.POSIXct(forecasts$list$dt, origin = "1970-1-1"))
tail(as.POSIXct(forecasts$list$dt, origin = "1970-1-1"))

### 9.1.11 OAuth による認証
library(httr)

facebook <- oauth_endpoint(
	authorize = "https://www.facebook.com/dialog/oauth",
	access = "https://graph.facebook.com/oauth/access_token")

Sys.setenv(FACEBOOK_CONSUMER_SECRET = appSec)
fb_app <- oauth_app("facebook", appID)
 
permissions <- "user_birthday,user_hometown,user_location,user_status,read_insights"

fb_oauth <- fbOAuth(app_id=appID, app_secret= appSec,extended_permissions = FALSE)
x <- getUsers("me", fb_oauth, private_info = FALSE)
x

## 9.3 Web スクレイピイング : グッドプラクティス
### 9.3.2 robots.txt とは何か
robotsCheck <- function(robotstxt = "", useragent = "*", dirs = "disallowed"){
  # パッケージ読み込み
  require(stringr)
  require(RCurl)

  # robots.txt の読込み
  bots <- getURL(robotstxt, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"), useragent = "*")
  write(bots, file = "robots.txt")
  bots <- readLines("robots.txt")

  # 定義された bot がリストにあるか確認
  useragent <- ifelse(useragent == "*", "\\*", useragent)
  args_ua_lines <- which(str_detect(bots, str_c("[Uu]ser-[Aa]gent:[ ]{0,}", useragent, "$")))
  is_args_ua_listed <- ifelse(length(args_ua_lines) > 0, TRUE, FALSE)

  # bot が含まれていたらルールを読み込んでいく
  disallow_dirs <- NULL
  allow_dirs <- NULL
  is_excluded <- 0

  if(is_args_ua_listed){
    rule_indicies <- c()
    for(start_pos in args_ua_lines){
      range <- (start_pos + 1):length(bots)
      rule_indicies <- append(rule_indicies, range[cumprod(str_detect(bots[range], "^(([Dd]isa)|([Aa]))llow")) > 0])
    }
    bot_rules <- bots[rule_indicies]

    # 禁止されているディレクトリの抽出
    disallow_rules <- bot_rules[str_detect(bot_rules, "^[Dd]isallow")]
    disallow_dirs <- unlist(str_extract_all(disallow_rules, "/.{0,}"))

    # 許可されているディレクトリの抽出
    allow_rules <- bot_rules[str_detect(bot_rules, "^[Aa]llow")]
    allow_dirs <- unlist(str_extract_all(allow_rules, "/.{0,}"))
    # 完全に許可されていないか
    is_excluded <- any(disallow_dirs == "/")
  }

  # 結果を返す
  if(is_excluded){ message("This bot is blocked from the site.")}
  if(dirs == "disallowed" & !is_excluded){ return(disallow_dirs) }
  if(dirs == "allowed" & !is_excluded){ return(allow_dirs) }
}

facebook_robotstxt <- "https://www.facebook.com/robots.txt"
robotsCheck(robotstxt = facebook_robotstxt, useragent = "*", dirs = "disallowed")
robotsCheck(robotstxt = facebook_robotstxt, useragent = "Yeti", dirs = "disallowed")
robotsCheck(robotstxt = facebook_robotstxt, useragent = "Yeti", dirs = "allowed")
robotsCheck(robotstxt = "http://www.google.com/robots.txt", useragent = "*", dirs = "allowed")

### 9.3.3 スクレイピイングは有効的に
# load packages
library(RCurl)
library(XML)

url <- "http://www.imdb.com/chart/top"
top <- getURL(url)
parsed_top <- htmlParse(top, encoding = "UTF-8")
top_table <- readHTMLTable(parsed_top)[[1]]
head(top_table[, 1:3])

getURL(url, useragent = str_c(R.version$platform, R.version$version.string, sep = ", "),
       httpheader = c(from = "eddie@datacollection.com"))

info <- debugGatherer()
httpheader <- list(from = "Eddie@r-datacollection.com", 'user-agent' = str_c(R.version$version.string, ", ", R.version$platform))
handle <- getCurlHandle(debugfunc = info$update, verbose = TRUE)

getBest <- function(doc) readHTMLTable(doc)[[1]][, 1:3]

url <- "http://www.imdb.com/chart/top"
best_doc <- getURL(url)
best_vec <- getBest(best_doc)
if (!file.exists("bestFilms.Rdata")) {
  save(best_vec, file = "bestFilms.Rdata")
}
head(best_vec)

httpheader$"If-Modified-Since" <- "Tue, 04 Mar 2016 10:00:00 GMT" # define If-Modified-Since header
best_doc <- getURL(url, curl = handle, httpheader = httpheader)

##### この関数を httpdate.r の名前で保存してください
httpDate <- function(time="now", type="rfc1123"){
  if(class(time)[1] == "character") {
    tmp <- as.POSIXlt(Sys.time(), tz="GMT")
  }else{
    tmp <- as.POSIXlt(as.POSIXct(time, origin = "1970-01-01"), tz="GMT")
  }

  nday <- c("Sun", "Mon" , "Tue" , 
                 "Wed", "Thu" , "Fri" , 
                 "Sat")[tmp$wday+1]
  month  <- tmp$mon+1
  nmonth <- c("Jan" , "Feb" , "Mar" , 
              "Apr", "May" , "Jun" , 
              "Jul" , "Aug", "Sep" , 
              "Oct" , "Nov" , "Dec")[month]
  mday <- formatC(tmp$mday, width=2, flag="0")
  hour <- formatC(tmp$hour, width=2, flag="0")
  min <- formatC(tmp$min , width=2, flag="0")
  sec <- formatC(round(tmp$sec) , width=2, flag="0")
  if(type=="rfc1123"){
    return(paste0(nday, ", ", 
                  mday," ", nmonth, " ", tmp$year+1900, " ", 
                  hour, ":", min, ":", sec, " GMT") )
  }else{
    stop("Not implemented")
  }
}

file.date <- function(filename, timezone=as.POSIXlt(Sys.time())$zone) {
  as.POSIXlt(min(unlist( file.info(filename)[4:6] )),
             origin = "1970-01-01", 
             tz = timezone)
}

file.date("httpdate.r")

# usage: 
# httpDate()
# httpDate( file.date("WorstFilms.Rdata") ) 
# httpDate("2001-01-02")
# httpDate("2001-01-02 18:00")
# httpDate("2001-01-02 18:00:01")
# httpDate(60*60*24*30.43827161*12*54+60*60*24*32)
# httpDate(-10*24*60*60,origin="2014-02-01")


source("httpdate.r")
last_mod <- file.date("bestFilms.Rdata")
last_mod

httpheader$"If-Modified-Since" <- httpDate(last_mod)
best_doc <- getURL(url, curl = handle, httpheader = httpheader)

getCurlInfo(handle)$response.code

if (getCurlInfo(handle)$response.code == 200) {
  best_list <- getBest(best_doc)
  save(best_list, file = "bestFilms.Rdata")
}

for(i in 1:length(filenames)){
  if(!file.exists(filenames[i])){
    download.file(str_c(url, filenames[i]), destfile = filenames[i])
  }
}

existing_files <- list.files("directory_of_scraped_files")
online_files <- vector_of_online_files
new_files <- setdiff(online_files, existing_files)

for(i in 1:length(urls)){
  scrape_function(urls[i])
  Sys.sleep(1)
}
