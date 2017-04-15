# 『Rによるデータ自動収集』
##　第 1 章　導入


### 1.1 ケーススタディ

ibrary(stringr)
library(XML)
library(maps)

### 原著者のサイトから執筆時のWikiページをダウンロードする
# http://www.r-datacollection.com/ の Book  materials リンク
# Chapter 1: Introduction

heritage_parsed <- htmlParse("http://www.r-datacollection.com/materials/ch-1-introduction/worldheritagedanger.htm")

### あるいは RCurl::getURLを使えば HTTPS プロトコルを利用できる
### library(RCurl)
### heritage_parsed <- htmlParse(getURL("https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"), encoding = "UTF-8")

tables <- readHTMLTable(heritage_parsed, stringsAsFactors = FALSE)


library(RCurl)
heritage_parsed <- htmlParse(getURL("https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"), encoding = "UTF-8")

