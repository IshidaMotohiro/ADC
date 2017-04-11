## 第17章 商品レビューのセンチメント分析}
### 17.1 データ収集
library(stringr)
library(XML)
library(RCurl)
library(RSQLite)

# 同様にスクレイピングしたデータはすでに下記URLにあるので、こちらを活用
# http://www.r-datacollection.com/materials/ch-16-amazon/
# 17章で使用するこれらのデータを拾ってくるコード
# DBに関してはGithub版と若干のかい離あり
# https://github.com/crubba/Wiley-ADCR/blob/master/ch-16-amazon/amazonProductInfo.db
##############################################################################################
setwd("C://temp/ch16")
library("rvest")
library("stringr")
download_content <- function(path)
{
  if(path != "" && !file.exists(path)){dir.create(path)}
  url <- paste0("http://www.r-datacollection.com/materials/ch-16-amazon/", path)
  x <- read_html(url)  
  links <- x %>% html_nodes("a") %>% html_attr("href")
  links <- links[str_detect(links, "(html|db|Rdata)$")]
  for(link in links)
  {
    download.file(paste0(url, link), paste0(path, link))
  }
}
download_content("")
download_content("dataFull/")
download_content("dataReviews/")


##############################################################################################

sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, "amazonProductInfo.db")

sql <- "SELECT phones.asin, items.fname FROM phones JOIN items ON phones.id=phones_id;"
phonesData <- dbGetQuery(con,sql)
head(phonesData)

#サポートサイトよりダウンロードしたHTMLのファイル名はURLエンコーディングにより
#半角スペースが%20になっている場合があるため、ファイル名の置換を行う
phonesData$fname <- str_replace_all(phonesData$fname, " ", "%20")

if(!file.exists("dataReviews")) dir.create("dataReviews")
setwd("dataReviews")

productPageFiles <- str_c("../dataFull/", phonesData$fname)
productPages <- lapply(productPageFiles, htmlParse)

extractReviewLinks <- function(x){
  x <- xpathApply(x, "//a[contains(text(), 'customer review')]/@href", as.character)[[1]]
  if(length(x) == 0) x <- NA
  if(str_detect(x, "create-review") & !is.na(x)) x <- NA
  names(x) <- NULL
  x
}

reviewLinks <- unlist(lapply(productPages, extractReviewLinks))


noLink <- NULL
for(i in seq_along(reviewLinks)){
  if(is.na(reviewLinks[i])){
    noLink <- rbind(noLink, productPageFiles[i])
  }
}
noLink[1:3]

#httpsへの変更
#reviewLinks <- str_replace(reviewLinks, "http://www.amazon.com", "")
#reviewLinks <- ifelse(is.na(reviewLinks), NA, str_c("http://www.amazon.com", reviewLinks))
reviewLinks <- str_replace(reviewLinks, "https://www.amazon.com", "")
reviewLinks <- ifelse(is.na(reviewLinks), NA, str_c("https://www.amazon.com", reviewLinks))

N <- length(reviewLinks)
for(i in seq_along(reviewLinks)){
  # file name
  fname <- str_c(phonesData[i, "asin"], "_0001.html")
  # download
  if(!file.exists(fname) & !is.na(reviewLinks[i])){
    message("downloading")
    try(download.file(reviewLinks[i], fname))
    # sleep
    sleep <- abs(rnorm(1)) + runif(1, 0, .25)
    message("I have done ", i, " of ", N, " - gonna sleep ", round(sleep, 2), " seconds.")
    Sys.sleep(sleep)
  }
  # size of file info
  message(i, " size: ", file.info(fname)$size/1000, " KB")
}

firstPages <- list.files(pattern = "001.html")
file.remove(firstPages[file.info(firstPages)$size == 0])
firstPages <- list.files(pattern = "001.html")

HTML <- lapply(firstPages, htmlParse)

for(i in seq_along(HTML)){
  # extract link
  link <- xpathApply(HTML[[i]], "//a[contains(text(), 'Next')]/@href", as.character)[[1]]
  #link <- paste0("http://www.amazon.com", link)
  link <- paste0("https://www.amazon.com", link)
  # set k to 2
  k <- 2
  while(length(link) > 0 & k <= 5){
    # gen filename
    fname <- str_replace(firstPages[i], "[[:digit:]]{4}.html", str_c(str_pad(k, 4, side = "left", pad = "0"), ".html"))
    message(i, ":", k, "... :", fname)
    # download file
    if(!file.exists(fname) & length(link) > 0){
      download.file(link, fname, quiet = T)
      message(" download to file name: ", fname)
      Sys.sleep(abs(rnorm(1)) + runif(1, 0, .25))
    }
    htmlNext <- htmlParse(fname)
    # extract link for next file
    link <- tryCatch(
      xpathApply(htmlNext, "//a[contains(text(), 'Next')]/@href", as.character)[[1]], 
      error = function(e){
        message("xpath error")
        NULL
      }
    )
    #link <- paste0("http://www.amazon.com", link)
    link <- paste0("https://www.amazon.com", link)
    # k + 1
    k <- k + 1
  }
}

tmp <- list.files(pattern = ".html")
file.remove(tmp[file.info(tmp)$size < 50000])

getNumbers <- function(node){
  val <- xmlValue(node)
  x <- str_extract(val, "[[:digit:]]{1,6}")
  x
}


FPAsins <- list.files(pattern="html$")
FPAsins <- unique(str_replace(FPAsins, "_.+", ""))
reviewsMeta <- data.frame(asin = FPAsins, one = NA, two = NA, three = NA, four = NA, five = NA, stringsAsFactors = F)

for(i in seq_along(HTML)){
  #tmp <- as.numeric(
  #  readHTMLTable(
  #    HTML[[i]],
  #    elFun = getNumbers,
  #    stringsAsFactors = F
  #    )$productSummary$V3
  #  )
  tmp <- readHTMLTable(HTML[[i]], header=FALSE, stringsAsFactors = FALSE)$histogramTable
  tmp <- as.numeric(str_replace_all(tmp$V3, "%", ""))
  print(tmp)
  reviewsMeta[i, c("one", "two", "three", "four", "five")] <- tmp[1:5]
}

reviewsMeta$sum <- apply(reviewsMeta[, c("one", "two", "three", "four", "five")], 1, sum)
reviewsMeta$mean <- (reviewsMeta$one +
                          reviewsMeta$two * 2 +
                          reviewsMeta$three * 3 +
                          reviewsMeta$four * 4 +
                          reviewsMeta$five * 5
                        ) / reviewsMeta$sum

reviews <- data.frame(asin = NA, stars = 0, helpfulyes = 0,
                         helpfulno = 0, helpfulsum = 0, date = "", title = "", text = "",
                         stringsAsFactors = F)

for(i in seq_along(FPAsins)){
  # gather file names for ASIN
  files <- list.files(pattern = FPAsins[i])
  asin <- FPAsins[i]
  message(i, " / ", length(FPAsins), " ... doing: ", asin)
  # loop through files with same asin
  for(k in seq_along(files)){
    # parsing one file
    html <- htmlParse(files[k])
    # base path for each review : "//div[@style='margin-left:0.5em;']"
    # reviewValue <- unlist( xpathApply(html, "//div[@style='margin-left:0.5em;']", xmlValue))
    # helpful
    # helpful <- str_extract(reviewValue, "[[:digit:]]{1,5}.*?[[:digit:]]{1,5} people")
    # helpful <- str_extract_all(helpful,"[[:digit:]]{1,5}")
    # helpfulyes <- as.numeric(unlist(lapply(helpful,'[',1)))
    # helpfulno <- as.numeric(unlist(lapply(helpful,'[',2))) - helpfulyes
    # helpfulsum <- helpfulyes + helpfulno
    # # stars
    # stars <- str_extract(reviewValue, "[[:digit:]]\\.[[:digit:]] out of 5 stars")
    # stars <- as.numeric(str_extract(stars, "[[:digit:]]"))
    # # text
    # text <- unlist(xpathApply(html, "//div[@style='margin-left:0.5em;']/div[@class='reviewText']", xmlValue))
    # text <- str_replace_all(text, "'", "''")
    # # title
    # title <- unlist(xpathApply(html, "//div[@style='margin-left:0.5em;']/div/span[@style='vertical-align:middle;']/b", xmlValue))
    # title <- str_replace_all(title, "'", "''")
    # # date
    # date <- unlist(xpathApply(html, "//div[@style='margin-left:0.5em;']/div/span[@style='vertical-align:middle;']/nobr", xmlValue))
    # # putting it together
    # tmp <- cbind(asin, stars, helpfulyes, helpfulno, helpfulsum, date, title, text)
    # reviews <- rbind(reviews, tmp)

    # 「役に立った？に対する回答からNoがなくなった」＆「表記・XPathが変更された」ための対応
    # レビューサイトが存在しない状態になっている場合ははじく(!is.null(reviewValue))
    reviewValue <- unlist(xpathApply(html, "//*[@id='cm_cr-review_list']//span[@class='cr-vote-buttons']", xmlValue))
    if(!is.null(reviewValue)){
      str_extract_all_with_replace <- function(string, pattern){
        sapply(str_extract_all(string, pattern), function(x){if(length(x) != 0){x} else{0}})
      }
      one <- str_extract_all_with_replace(reviewValue, "One person")
      one <- ifelse(one=="One person", 1, 0)
      many <- str_extract_all_with_replace(reviewValue, "[[:digit:]]{1,5} people")
      many <- as.numeric(str_extract_all(many, "[[:digit:]]{1,5}"))
      helpful <- one + many
      helpfulyes <- helpful 
      helpfulno <- rep(NA, length(helpfulyes))
      helpfulsum <- helpfulyes
      # stars
      stars <- paste(xpathApply(html, "//*[@id='cm_cr-review_list']//*[@class='a-icon-alt']", xmlValue), collapse=" ")
      stars <- str_extract_all(str_extract_all(stars, "[[:digit:]]\\.[[:digit:]] out of"), "[[:digit:]]\\.[[:digit:]]")
      stars <- as.numeric(unlist(stars))
      # text
      text <- unlist(xpathApply(html, "//*[@id='cm_cr-review_list']//*[@class='a-row review-data']", xmlValue))
      # title
      title <- unlist(xpathApply(html, "//*[@id='cm_cr-review_list']//*[@class='a-size-base a-link-normal review-title a-color-base a-text-bold']", xmlValue))
      # date
      date <- unlist(xpathApply(html, "//*[@id='cm_cr-review_list']//*[@class='a-size-base a-color-secondary review-date']", xmlValue))
      # putting it together
      tmp <- data.frame(asin, stars, helpfulyes, helpfulno, helpfulsum, date, title, text)
      reviews <- rbind(reviews, tmp)
    }
  }
}

reviews <- reviews[!is.na(reviews$asin),]

SQL <- str_c(" INSERT INTO reviewsMeta(asin, one, two, three, four, five)
  VALUES
  ('",
  str_c(
   reviewsMeta[, "asin"],
   reviewsMeta[, "one"],
   reviewsMeta[, "two"],
   reviewsMeta[, "three"],
   reviewsMeta[, "four"],
   reviewsMeta[, "five"],
   sep="', '")
  ,"'); ")

cat(SQL[1])
cat(SQL[2])

for(i in seq_along(SQL)) dbGetQuery(con, SQL[i])

SQL <- str_c(" INSERT INTO reviews (asin, stars, helpfulyes, helpfulno, helpfulsum, date, title, text)
  VALUES
  ('",
  str_c(
   reviews[, "asin"],
   reviews[, "stars"],
   reviews[, "helpfulyes"],
   reviews[, "helpfulno"],
   reviews[, "helpfulsum"],
   reviews[, "date"],
   reviews[, "title"],
   reviews[, "text"],
   sep="', '")
  ,"'); ")


cat(SQL[1])

# 17.2 データの分析
sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, "amazonProductInfo.db")
dbListTables(con)

allData <- dbReadTable(con, "AllData")
names(allData)

dim(allData)

library(tm)
library(textcat)
library(RTextTools)
library(vioplot)
# 著者の環境ではSnowballC, slamのインストールも別途必要であった

allData$language <- textcat(allData$reviewtext)
dim(allData)
allData <- allData[allData$language == "english",]
allData <- allData[!is.na(allData$reviewtext),]
dim(allData)


reviews <- Corpus(VectorSource(allData$reviewtext))

reviews <- tm_map(reviews, removeNumbers)
reviews <- tm_map(reviews, str_replace_all, pattern = "[[:punct:]]", replacement = " ")
reviews <- tm_map(reviews, removeWords, words = stopwords("en"))
#reviews <- tm_map(reviews, content_transformer(tolower))
reviews <- tm_map(reviews, tolower)
reviews <- tm_map(reviews, stemDocument, language = "english")
reviews

# 本文とアドレスが違う＆ダウンロード後にrarの解凍が必要
download.file("http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar", "opinion-lexicon-English.rar")

pos <- readLines("opinion-lexicon-English/positive-words.txt")
pos <- pos[!str_detect(pos, "ˆ;")]
pos <- pos[2:length(pos)]
neg <- readLines("opinion-lexicon-English/negative-words.txt")
neg <- neg[!str_detect(neg, "ˆ;")]
neg <- neg[2:length(neg)]

pos <- stemDocument(pos, language = "english")
pos <- pos[!duplicated(pos)]
neg <- stemDocument(neg, language = "english")
neg <- neg[!duplicated(neg)]

set.seed(123)
sample(pos, 10)

sample(neg, 10)

# tdm.reviews.bin <- TermDocumentMatrix(reviews, control = list(weighting = weightBin))
tdm.reviews.bin <- TermDocumentMatrix(tm_map(reviews, PlainTextDocument), control = list(weighting = weightBin))
tdm.reviews.bin <- removeSparseTerms(tdm.reviews.bin, 1-(5/length(reviews)))
tdm.reviews.bin

pos.mat <- tdm.reviews.bin[rownames(tdm.reviews.bin) %in% pos, ]
neg.mat <- tdm.reviews.bin[rownames(tdm.reviews.bin) %in% neg, ]
pos.out <- apply(pos.mat, 2, sum)
neg.out <- apply(neg.mat, 2, sum)
senti.diff <- pos.out - neg.out
senti.diff[senti.diff == 0] <- NA

summary(senti.diff)

#To prepend multibyte problem
Sys.setlocale("LC_ALL", "C")

range(nchar(allData$reviewtext))

plot.dat <- data.frame(
  sentiment = senti.diff/nchar(allData$reviewtext),
  stars = allData$reviewstars)
plot.dat <- plot.dat[!is.na(plot.dat$sentiment),]

vioplot(
  plot.dat$sentiment[plot.dat$stars == 1],
  plot.dat$sentiment[plot.dat$stars == 2],
  plot.dat$sentiment[plot.dat$stars == 3],
  plot.dat$sentiment[plot.dat$stars == 4],
  plot.dat$sentiment[plot.dat$stars == 5],
  horizontal = T,
  col = "grey")
axis(2, at = 3, labels = "Stars in review", line = 1, tick = FALSE)
axis(1, at = 0.01, labels = "Estimated sentiment by number of characters", line = 1, tick = FALSE)

# Set up the corpus of titles
titles <- Corpus(VectorSource(allData$reviewtitle))
titles
# Perform data preparation
titles <- tm_map(titles, removeNumbers)
titles <- tm_map(titles, str_replace_all, pattern = "[[:punct:]]", replacement = " ")
titles <- tm_map(titles, removeWords, words = stopwords("en"))
titles <- tm_map(titles, tolower)
titles <- tm_map(titles, stemDocument, language = "english")
# Set up term-document matrix
#tdm.titles <- TermDocumentMatrix(titles)
tdm.titles <- TermDocumentMatrix(tm_map(titles, PlainTextDocument))
tdm.titles <- removeSparseTerms(tdm.titles, 1-(5/length(titles)))
tdm.titles

# Calculate the sentiment
pos.mat.tit <- tdm.titles[rownames(tdm.titles) %in% pos, ]
neg.mat.tit <- tdm.titles[rownames(tdm.titles) %in% neg, ]
pos.out.tit <- apply(pos.mat.tit, 2, sum)
neg.out.tit <- apply(neg.mat.tit, 2, sum)
senti.diff.tit <- pos.out.tit - neg.out.tit
senti.diff.tit[senti.diff.tit == 0] <- NA

plot(jitter(senti.diff.tit), jitter(allData$reviewstars),
        col = rgb(0, 0, 0, 0.4),
        ylab = "Stars in Review",
        xlab = "Estimated sentiment"
        )
abline(v = 0, lty = 3)

#dtm.reviews <- DocumentTermMatrix(reviews)
dtm.reviews <- DocumentTermMatrix(tm_map(reviews, PlainTextDocument))
dtm.reviews <- removeSparseTerms(dtm.reviews, 1-(5/length(reviews)))
N <- length(reviews)
container <- create_container(
  dtm.reviews,
  labels = allData$reviewstars,
  trainSize = 1:2000,
  testSize = 2001:N,
  virgin = F
  )
dtm.reviews

maxent.model <- train_model(container, "MAXENT")
svm.model <- train_model(container, "SVM")
maxent.out <- classify_model(container, maxent.model)
svm.out <- classify_model(container, svm.model)

labels.out <- data.frame(
  correct.label = as.numeric(allData$reviewstars[2001:N]),
  maxent = as.numeric(maxent.out[,1]),
  svm = as.numeric(svm.out[,1])
)

plot(jitter(labels.out[,2]), jitter(labels.out[,1]),
  xlab = "Estimated stars",
  ylab = "True stars"
)
plot(jitter(labels.out[,3]), jitter(labels.out[,1]), 
  xlab = "Estimated stars",
  ylab = "True stars"
)

