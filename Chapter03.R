# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 3 章 XML と JSON

### 3.5.1 XML の解析

library(XML)
parsed_stocks <- xmlParse(file = "stocks/technology.xml")


### XML の検証(バリデーション)

library(XML)
parsed_stocks <- xmlParse(file = "stocks/technology.xml", validate = TRUE)

library(XML)
stocks <- xmlParse(file = "stocks/technology-manip.xml", validate = TRUE)

### 3.5.2 XMLドキュメント上の基本的な操作

bond <- xmlParse("bond.xml")
class(bond)

root <- xmlRoot(bond)
xmlName(root)
xmlSize(root)   


### ナビゲーション

root[[1]]
root[[1]][[1]]
root[["movie"]]
root["movie"]
attr(root,"class")
root[["movie"]][[1]][[1]]

### XML ベースのドキュメントへのアクセス

xmlParse("rsscode.rss")

### XML からデータフレームやリストへ

xmlSApply(root[[1]], xmlValue)
xmlSApply(root, xmlAttrs)
xmlSApply(root, xmlGetAttr, "id")

(movie.df <- xmlToDataFrame(root))
(movie.list <- xmlToList(bond))


### 3.5.4 イベントドリブン型解析
### イベントドリブン/SAXパーサ

branchFun <- function(){
  container_close <- numeric()
  container_date <- numeric()
  
  "Apple" <- function(node,...) {
    date <- xmlValue(xmlChildren(node)[[c("date")]])
    container_date <<- c(container_date, date)
    close <- xmlValue(xmlChildren(node)[[c("close")]])
    container_close <<- c(container_close, close)
    #print(c(close, date));Sys.sleep(0.5)
  }
  getContainer <- function() data.frame(date=container_date, close=container_close)
  list(Apple=Apple, getStore=getContainer)
}

(h5 <- branchFun())
invisible(xmlEventParse(file = "stocks/technology.xml", branches = h5, handlers = list()))

apple.stock <- h5$getStore()
head(apple.stock, 5)


### 3.8 JSONとRの練習

library(RJSONIO)
isValidJSON("indy.json")
indy <- fromJSON("indy.json") 


### fromJSON()

class(indy)


### 完璧なXML/JSON-to-R関数は存在しない

library(stringr)
indy.vec <- unlist(indy, recursive = TRUE, use.names = TRUE)
indy.vec[str_detect(names(indy.vec), "name")]
sapply(indy[[1]], '[[', "year")


library(plyr)
indy.unlist <- sapply(indy[[1]], unlist)
indy.df <- do.call("rbind.fill", lapply(lapply(indy.unlist, t), data.frame, stringsAsFactors = FALSE))
names(indy.df)


peanuts.json <- fromJSON("peanuts.json", nullValue=NA, simplify = FALSE)
peanuts.df <- do.call("rbind", lapply(peanuts.json, data.frame, stringsAsFactors = FALSE))
peanuts.df

### toJSON
peanuts.out.json <- toJSON(peanuts.df, pretty = TRUE)
file.output <- file("peanuts_out.json")
writeLines(peanuts.out.json, file.output)
close(file.output)


### jsonliteでより適したマッピングを行う

library(jsonlite)

x <- '[1, 2, true, false]'
fromJSON(x)

x <- '["foo", true, false]'
fromJSON(x)

x <- '[1, "foo", null, false]'
fromJSON(x)

(peanuts.json <- fromJSON("peanuts.json"))

(indy <- fromJSON("data/indy.json"))
indy.df <- indy$`indy movies`
indy.df$name





