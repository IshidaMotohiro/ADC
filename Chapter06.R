# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 6 章 AJAX

### 6.1.2 DOM 操作

library(XML)

(fortunes1 <- htmlParse("fortunes1.html"))

### 6.2.1 外部 HTML/XML ドキュメントの読み込み

library(XML)

(fortunes2 <- htmlParse("fortunes2.html"))

### 6.3.3 Network パネル

library(RCurl)

(fortunes_xhr <- getURL("r-datacollection.com/materials/ajax/quotes/quotes.html"))
