# 『Rによるデータ自動収集』
##　第 1 章　導入


### 1.1 ケーススタディ
install.packages(c("stringr", "XML", "maps"), dependencies = TRUE)

library(stringr)
library(XML)
library(maps)

### 原著者のサイトから執筆時のWikiページをダウンロードする
# http://www.r-datacollection.com/ の Book  materials リンク
# Chapter 1: Introduction

heritage_parsed <- htmlParse("data/worldheritagedanger.htm")

###heritage_parsed <- htmlParse("http://www.r-datacollection.com/materials/ch-1-introduction/worldheritagedanger.htm")
### あるいは RCurl::getURLを使えば HTTPS プロトコルを利用できる
###library(RCurl)
###  heritage_parsed <- htmlParse(getURL("https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"), encoding = "UTF-8")

tables <- readHTMLTable(heritage_parsed, stringsAsFactors = FALSE)

danger_table <- tables[[2]]

names(danger_table)

danger_table <- danger_table[ , c(1,3,4,6,7)]

colnames(danger_table) <- c("name","locn","crit","yins","yend")

danger_table$name[1:3]

danger_table$crit <- ifelse(str_detect(danger_table$crit, "Natural")==T, "nat", "cult")

danger_table$yins <- as.numeric(danger_table$yins)

danger_table$yins[1:3]

yend_clean <- unlist(str_extract_all(danger_table$yend, "[[:digit:]]{4}$"))

danger_table$yend <- as.numeric(yend_clean)

danger_table$yend[1:3]

danger_table$locn[c(1,3,5)]

reg_y <- "[/][ -]*[[:digit:]]*[.]*[[:digit:]]*[;]"

reg_x <- "[;][ -]*[[:digit:]]*[.]*[[:digit:]]*"

y_coords <- str_extract(danger_table$locn, reg_y)

y_coords <- as.numeric(str_sub(y_coords, 3, -2))

danger_table$y_coords <- y_coords

x_coords <- str_extract(danger_table$locn, reg_x)

x_coords <- as.numeric(str_sub(x_coords, 3, -1))

danger_table$x_coords <- x_coords

danger_table$locn <- NULL

### 正規表現

round(danger_table$y_coords,2)[1:3]

round(danger_table$x_coords,2)[1:3]

dim (danger_table)

head (danger_table)


### データの概要

par(oma=c(0,0,0,0))

par(mar=c(0,0,0,0))

pch <- ifelse(danger_table$crit == "nat", 19, 2)


map("world", col = "darkgrey", lwd = .5, mar = c(0.1,0.1,0.1,0.1))


points(danger_table$x_coords, danger_table$y_coords, pch = pch, col = "black", cex = .8)

box()

table(danger_table$crit)

### UNESCO の政治的メッセージ

hist(danger_table$yend,  
     freq = TRUE, ylab = "", 
     xlab = "危機遺産リストに登録された年", 
     main = "")

# cairo_pdf(file = "Figure1-2")
hist(danger_table$yend,  freq = TRUE, ylab = "", xlab = "危機遺産リストに登録された年", main = "")
# dev.off()

# cairo_ps(file = "Figure1-2.eps")
hist(danger_table$yend,  freq = TRUE, ylab = "", xlab = "危機遺産リストに登録された年", main = "")
# dev.off()


duration <- danger_table$yend - danger_table$yins

hist(duration,
        freq=TRUE, ylab = "", 
        xlab="危機遺産リストに登録されるまでの経過年数",
        main="")



# cairo_pdf(file = "Figure1-3")
hist(duration, freq=TRUE, ylab = "",  xlab="危機遺産リストに登録されるまでの経過年数", main = "")
# dev.off()

#cairo_ps(file = "Figure1-3.eps")
hist(duration, freq=TRUE, ylab = "",  xlab="危機遺産リストに登録されるまでの経過年数", main = "")
#dev.off()



