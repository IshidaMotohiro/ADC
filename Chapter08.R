# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 8 章 正規表現と重要な文字列関数

raw.data <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson,Homer5553642Dr. Julius Hibbert"

library(stringr)

name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name

phone <- unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phone

data.frame(name = name, phone = phone)


### 8.1 正規表現

example.obj <- "1. A small sentence. - 2. Another tiny sentence."


### 8.1.1 マッチした文字列の抽出

str_extract(example.obj, "small")

str_extract(example.obj, "banana")

unlist(str_extract_all(example.obj, "sentence"))

out <- str_extract_all(c("text", "manipulation", "basics"), "a")
out

str_extract(example.obj, "small")

str_extract(example.obj, "SMALL")

str_extract(example.obj, ignore.case("SMALL"))

unlist(str_extract_all(example.obj, "en"))

str_extract(example.obj, "mall sent")

str_extract(example.obj, "2")

str_extract(example.obj, "^2")

unlist(str_extract_all(example.obj, "sentence$"))

unlist(str_extract_all(example.obj, "tiny|sentence"))


### 8.1.2 正規表現の一般化

str_extract(example.obj, "sm.ll")

str_extract(example.obj, "sm[abc]ll")

str_extract(example.obj, "sm[a-p]ll")

unlist(str_extract_all(example.obj, "[uvw. ]"))

unlist(str_extract_all(example.obj, "[[:punct:]]"))

unlist(str_extract_all(example.obj, "[:punct:]"))

unlist(str_extract_all(example.obj, "[AAAAAA]"))

str_extract("François Hollande", "Fran[a-z]ois")

str_extract("François Hollande", "Fran[[:alpha:]]ois")

unlist(str_extract_all(example.obj, "[[:punct:]ABC]"))

unlist(str_extract_all(example.obj, "[^[:alnum:]]"))

str_extract(example.obj, "s[[:alpha:]][[:alpha:]][[:alpha:]]l")

str_extract(example.obj, "s[[:alpha:]]{3}l")

str_extract(example.obj, "A.+sentence")

str_extract(example.obj, "A.+?sentence")

unlist(str_extract_all(example.obj, "(.en){1,5}"))

unlist(str_extract_all(example.obj, ".en{1,5}"))

unlist(str_extract_all(example.obj, "\\."))

unlist(str_extract_all(example.obj, fixed(".")))

unlist(str_extract_all(example.obj, "\\w+"))

unlist(str_extract_all(example.obj, "e\\b"))

str_extract(example.obj, "([[:alpha:]]).+?\\1")

str_extract(example.obj, "(\\b[b-z]+\\b).+?\\1")


### 8.1.3 シンプソンズ再び

raw.data

name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name

phone <- unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phone


### 8.2.1 stringr パッケージ

str_extract(example.obj, "tiny")

str_extract_all(example.obj, "[[:digit:]]")

str_locate(example.obj, "tiny")

str_sub(example.obj, start = 35, end = 38)

str_sub(example.obj, 35, 38) <- "huge"
example.obj

str_replace(example.obj, pattern = "huge", replacement = "giant")

unlist(str_split(example.obj, "-"))

as.character(str_split_fixed(example.obj, "[[:blank:]]", 5))

example.obj

char.vec <- c("this", "and this", "and that")

str_detect(char.vec, "this")

str_count(char.vec, "this")

str_count(char.vec, "\\w+")

dup.obj <- str_dup(char.vec, 3)
dup.obj

length.char.vec <- str_length(char.vec)
length.char.vec

char.vec <- str_pad(char.vec, width = max(length.char.vec), side = "both", pad = " ")
char.vec

char.vec <- str_trim(char.vec)
char.vec

cat(str_c(char.vec, collapse = "\n"))

str_c("text", "manipulation", sep = " ")

str_c("text", c("manipulation", "basics"), sep = " ")


### 8.2.2 いくつかの便利な関数

agrep("Barack Obama", "Barack H. Obama", max.distance = list(all = 3))

agrep("Barack Obama", "Michelle Obama", max.distance = list(all = 3))

pmatch(c("and this", "and that", "and these", "and those"), char.vec)

make.unique(c("a", "b", "a", "c", "b", "a"))

library(XML)
# ファイルのダウンロード
if(!file.exists("listOfSimpsonsEpisodes.html")){
  link <- "http://en.wikipedia.org/wiki/List_of_The_Simpsons_episodes_(seasons_1%E2%80%9320)"
  download.file(link, "listOfSimpsonsEpisodes.html", mode="wb")
}
# テーブルの取得
tables <- readHTMLTable("listOfSimpsonsEpisodes.html",
                        header=T, stringsAsFactors=F)
tmpcols <- names(tables[[3]])
for(i in 3:10){
  tmpcols <- intersect(tmpcols, names(tables[[i]]))
}
episodes <- NULL
for(i in 3:10){
  episodes <- rbind(episodes[,tmpcols],tables[[i]][,tmpcols])
}
for(i in 1:dim(episodes)[2]){
  Encoding(episodes[,i]) <- "UTF-8"
}
names(episodes) <- c("pnr", "nr", "title", "directedby",
                     "Writtenby", "airdate", "productioncode")
save(episodes,file="episodes.Rdata")

load("episodes.Rdata")

grep("Homer", episodes$title[1:10], value=T)

grepl("Homer", episodes$title[1:10])

iffer1 <- grepl("Homer", episodes$title)
iffer2 <- grepl("Lisa", episodes$title)
iffer <- iffer1 & iffer2
episodes$title[iffer]

grepall <- function(pattern, x,
                    ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE,
                    value=FALSE, logic=FALSE){
  # エラーと例外のハンドリング
  if(length(pattern)==0 | length(x)==0){
    warning("Length of pattern or data equals zero.")
    return(NULL)
  }
  # grepl() とall() の適用
  indicies <- sapply(pattern, grepl, x,
                     ignore.case, perl, fixed, useBytes)
  index <- apply(indicies, 1, all)
  # 結果の変換と返却
  if(logic==T) return(index)
  if(value==F) return((1:length(x))[index])
  if(value==T) return(x[index])
}
grepall(c("Lisa","Homer"), episodes$title)
grepall(c("Lisa","Homer"), episodes$title, value=T)


### 8.3 文字エンコーディング

Sys.getlocale()

small.frogs <- "Små grodorna, små grodorna är lustiga att se."
small.frogs

small.frogs.utf8 <- iconv(small.frogs, from = "windows-1252",
                          to = "UTF-8")
Encoding(small.frogs.utf8)

small.frogs.utf8

Encoding(small.frogs.utf8) <- "windows-1252"
small.frogs.utf8

sample(iconvlist(), 10)

library(RCurl)
enc.test <- getURL("http://www.sciencemag.org/")
unlist(str_extract_all(enc.test, "<meta.+?>"))

library(tau)
is.locale(small.frogs)
is.ascii(small.frogs)

