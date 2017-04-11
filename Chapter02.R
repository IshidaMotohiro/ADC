# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 5 章 HTML

### 2.4.1  DOM

library(XML)

parsed_fortunes <- htmlParse(file = url)

print(parsed_fortunes)

### 2.4.2 ハンドラ関数の指

h1 <- list("body" = function(x){NULL})

parsed_fortunes <- htmlTreeParse(url, handlers = h1, asTree = TRUE)

parsed_fortunes$children

### 2.4.2 総称ハンドラ

h2 <- list(
    startElement = function(node, ...){
	name <- xmlName(node)
	if(name %in% c("div", "title")){NULL}else{node}
    },
    comment = function(node){NULL}
)

parsed_fortunes <- htmlTreeParse(file = url, handlers = h2, asTree = TRUE)

### 2.4.3 クロージャーとしてのハンドラ関数

getItalics = function() {
       i_container = character()
       list(i = function(node, ...) {
              i_container <<- c(i_container, xmlValue(node))
       },
       returnI = function() i_container)
}


h3 <- getItalics()

invisible(htmlTreeParse(url, handlers = h3))
# コード部分での注釈は # 1 個で良い
h3$returnI()
