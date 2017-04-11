# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 4 章 XPATH

### 4.1 Xpath - Webドキュメント用のクエリ言語
### 最初のポイント：解析

library(XML)
parsed_doc <- htmlParse(file = "fortunes.html")
print(parsed_doc)

### 4.2 XPathによるノードセットの識別
### 4.2.1 XPathクエリの基本構造


### 絶対パス
xpathSApply(doc = parsed_doc, path = "/html/body/div/p/i")

### 相対パス
xpathSApply(parsed_doc, "//body//p/i")
xpathSApply(parsed_doc, "//p/i")

### ワイルドカード演算子
xpathSApply(parsed_doc, "/html/body/div/*/i")

### 要素の選択式
xpathSApply(parsed_doc, "//title/..")

### 複数パス
xpathSApply(parsed_doc, "//address | //title")

twoQueries <- c(address = "//address", title = "//title")
xpathSApply(parsed_doc, twoQueries)


### 4.2.2 ノードの関係性

### 家系図との類似性
xpathSApply(parsed_doc, "//a/ancestor::div")
xpathSApply(parsed_doc, "//a/ancestor::div//i")
xpathSApply(parsed_doc, "//p/preceding-sibling::h1")
xpathSApply(parsed_doc, "//title/parent::*")


### 4.2.3 XPath述語

### 4.2.3.1 数値述語
### 暗黙の数値プロパティ
xpathSApply(parsed_doc, "//div/p[position()=1]")
xpathSApply(parsed_doc, "//div/p[last()]")
xpathSApply(parsed_doc, "//div/p[last()-1]")
xpathSApply(parsed_doc, "//div[count(.//a)>0]")
xpathSApply(parsed_doc, "//div[count(./@*)>2]")
xpathSApply(parsed_doc, "//*[string-length(text())>50]")

### ブール関数
xpathSApply(parsed_doc, "//div[not(count(./@*)>2)]")

### 4.2.3.2 テキスト述語
xpathSApply(parsed_doc, "//div[@date='October/2011']")
xpathSApply(parsed_doc, "//*[contains(text(), 'magic')]")
xpathSApply(parsed_doc, "//div[starts-with(./@id, 'R')]")

### テキスト内文字列の前処理
xpathSApply(parsed_doc, "//div[substring-after(./@date, '/')='2003']//i")


### 4.3 ノード要素の抽出

xpathSApply(parsed_doc, "//title", fun = xmlValue)

### 抽出関数群
xpathSApply(parsed_doc, "//div", xmlAttrs)   
xpathSApply(parsed_doc, "//div", xmlGetAttr, "lang")


### 4.3.1 fun引数の拡張

### xpathSApply()のためのカスタム関数
lowerCaseFun <- function(x) {
	x <- tolower(xmlValue(x))
	x
	}

xpathSApply(parsed_doc, "//div//i", fun = lowerCaseFun)

dateFun <- function(x) {
    require(stringr)
    date <- xmlGetAttr(node = x, name = "date")
    year <- str_extract(date, "[0-9]{4}")
    year
    }

xpathSApply(parsed_doc, "//div", dateFun)

idFun <- function(x) {
	id <- xmlGetAttr(x, "id")
	id <- ifelse(is.null(id), "not specified", id)
	return(id)
}

xpathSApply(parsed_doc, "//div", idFun)


### 4.3.1.1 XPath式による変数の使用

parsed_stocks <- xmlParse(file = "technology.xml")
companies <- c("Apple", "IBM", "Google")

(expQuery <- sprintf("//%s/close", companies))

getClose <- function(node) {
	value <- xmlValue(node)
	company <- xmlName(xmlParent(node))
	mat <- c(company = company, value = value)
}

stocks<-as.data.frame(t(xpathSApply(parsed_stocks, expQuery, getClose))) 
stocks$value <- as.numeric(as.character(stocks$value))
head(stocks,3)


### 4.3.2 XML名前空間

parsed_xml <- xmlParse("titles.xml")
parsed_xml

xpathSApply(parsed_xml, "//title", fun = xmlValue)

### 名前空間の回避
xpathSApply(parsed_xml, "//*[local-name()='title']", xmlValue)

xpathSApply(parsed_xml, "//x:title", namespaces = c(x = "http://funnybooknames.com/crockford"), fun = xmlValue)
xpathSApply(parsed_xml, "//x:title", namespaces = c(x = "http://www.w3.org/1999/xhtml"), fun = xmlValue)

nsDefs <- xmlNamespaceDefinitions(parsed_xml)[[2]]
ns <- nsDefs$uri
ns 

xpathSApply(parsed_xml, "//x:title", namespaces = c(x = ns), xmlValue)
