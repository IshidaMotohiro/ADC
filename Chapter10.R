# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 10 章 統計的テキスト処理
### 10.1 例 : 英国政府のプレスリリースを分類する
library(RCurl)
library(XML)
library(stringr)

all_links <- character()
new_results <- 'government/announcements?keywords=&announcement_type_option=press-releases&topics[]=all&departments[]=all&world_locations[]=all&from_date=&to_date=01%2F07%2F2010'
signatures = system.file("CurlSSL", cainfo = "cacert.pem", package = "RCurl")
while(length(new_results) > 0){
  new_results <- str_c("https://www.gov.uk/", new_results)
  results <- getURL(new_results, cainfo = signatures)
  results_tree <- htmlParse(results)
  all_links <- c(all_links, xpathSApply(results_tree, "//li[@id]//a", xmlGetAttr, "href"))
  new_results <- xpathSApply(results_tree, "//nav[@id='show-more-documents']//li[@class='next']//a", xmlGetAttr, "href")
}

all_links[1]
length(all_links)

dir.create("Press_Releases/")
for(i in 1:length(all_links)){
  url <- str_c("https://www.gov.uk", all_links[i])
  tmp <- getURL(url, cainfo = signatures)
  write(tmp, str_c("Press_Releases/", i, ".html"))
}

length(list.files("Press_Releases"))
list.files("Press_Releases")[1:3]

### 10.2 テキストデータの処理
### 10.2.1 tm パッケージによる大規模なテキスト操作

tmp <- readLines("Press_Releases/1.html")
tmp <- str_c(tmp, collapse = "")
tmp <- htmlParse(tmp)
release <- xpathSApply(tmp, "//div[@class='block-4']", xmlValue)

organisation <- xpathSApply(tmp, "//dd[@class='from']/a[@class='organisation-link']", xmlValue)
organisation
publication <- xpathSApply(tmp, "//dd/time[@class='date']", xmlValue)[1]
publication

library(tm)
release_corpus <- Corpus(VectorSource(release))

meta(release_corpus[[1]], "organisation") <- organisation[1]
meta(release_corpus[[1]], "publication") <- publication
meta(release_corpus[[1]])

n <- 1
for(i in 2:length(list.files("Press_Releases/"))){
  tmp <- readLines(str_c("Press_Releases/", i, ".html"))
  tmp <- str_c(tmp, collapse = "")
  tmp <- htmlParse(tmp)
  release <- xpathSApply(tmp, "//div[@class='block-4']", xmlValue)
  organisation <- xpathSApply(tmp, "//dd[@class='from']/a[@class='organisation-link']", xmlValue)
  publication <- xpathSApply(tmp, "//dd/time[@class='date']", xmlValue)[1]
  if(length(release) != 0){
    n <- n + 1
    tmp_corpus <- Corpus(VectorSource(release))
    release_corpus <- c(release_corpus, tmp_corpus)
    meta(release_corpus[[n]], "organisation") <- organisation[1]
    meta(release_corpus[[n]], "publication") <- publication
  }
}
release_corpus

meta.data <- data.frame(unlist(meta(release_corpus, "organisation")),
                        unlist(meta(release_corpus, "publication")))
colnames(meta.data) <- c("organisation", "publication")
head(meta.data)

table(as.character(meta(release_corpus, "organisation")))

idx <- meta(release_corpus, "organisation")  == "Department for Business, Innovation & Skills" |
  meta(release_corpus, "organisation")  == "Department for Communities and Local Government" |
  meta(release_corpus, "organisation")  == "Department for Environment, Food & Rural Affairs" |
  meta(release_corpus, "organisation")  == "Foreign & Commonwealth Office" |
  meta(release_corpus, "organisation")  == "Ministry of Defence" |
  meta(release_corpus, "organisation")  == "Wales Office"

release_corpus <- release_corpus[idx]
release_corpus

##### tm.filter() によるコーパスのフィルタリング
afgh <- tm_filter(release_corpus, FUN = function(x) any(str_detect(x, "Afghanistan")))
afgh

### 10.2.2 単語文書行列の構築
tdm <- TermDocumentMatrix(release_corpus)
tdm

### 10.2.3 データクレンジング
#### 10.2.3.1 単語の除去
##### 数字の削除
release_corpus <- tm_map(release_corpus, removeNumbers)

##### 句読点の削除
release_corpus <- tm_map(release_corpus, content_transformer(function(x){str_replace_all(x, pattern = "[[:punct:]]", replacement = " ")}))

##### 大文字を小文字に変換
release_corpus <- tm_map(release_corpus, content_transformer(tolower))

##### ストップワードの除去
length(stopwords("en"))
stopwords("en")[1:10]
release_corpus <- tm_map(release_corpus, removeWords, words = stopwords("en"))

#### 10.2.3.2 ステミング
release_corpus <- tm_map(release_corpus, stemDocument)

### 10.2.4 スパース性と n-gram
tdm <- TermDocumentMatrix(release_corpus)
tdm

##### 低頻度な単語
tdm <- removeSparseTerms(tdm, 1-(10/length(release_corpus)))
tdm

##### Bigram
BigramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

tdm_bigram <- TermDocumentMatrix(release_corpus, control = list(tokenize = BigramTokenizer))
tdm_bigram

findAssocs(tdm, "nuclear", .7)


### 10.3 教師あり学習の手法
### --------------------------------------------------------------

### 10.3.5 応用 : 政府によるプレスリリース
##### 文書単語行列の作成
dtm <- DocumentTermMatrix(release_corpus)
dtm <- removeSparseTerms(dtm, 1-(10/length(release_corpus)))
dtm

org_labels <- unlist(meta(release_corpus, "organisation"))
org_labels[1:3]

library(RTexttools)
N <- length(org_labels)
container <- create_container(
    dtm,
    labels = org_labels,
    trainSize = 1:400,
    testSize = 401:N,
    virgin = F
)

slotNames(container)

##### 推定
svm_model <- train_model(container, "SVM")
tree_model <- train_model(container, "TREE")
maxent_model <- train_model(container, "MAXENT")

svm_out <- classify_model(container, svm_model)
tree_out <- classify_model(container, tree_model)
maxent_out <- classify_model(container, maxent_model)

##### 評価
head(svm_out)
head(tree_out)
head(maxent_out)

labels_out <- data.frame(
    correct_label = org_labels[401:N],
    svm = as.character(svm_out[,1]),
    tree = as.character(tree_out[,1]),
    maxent = as.character(maxent_out[,1]),
    stringsAsFactors = F
)

# SVM の結果
table(labels_out[,1] == labels_out[,2])
prop.table(table(labels_out[,1] == labels_out[,2]))

# ランダムフォレストの結果
table(labels_out[,1] == labels_out[,3])
prop.table(table(labels_out[,1] == labels_out[,3]))

# 最大エントロピー分類器の結果
table(labels_out[,1] == labels_out[,4])
prop.table(table(labels_out[,1] == labels_out[,4]))

### 10.4 教師なし学習の手法
### 10.4.2 応用 : 政府のプレスリリース
##### ミニコーパスの作成
short_corpus_size <- 20
corpus_indeces <- c(1:length(release_corpus))
short_corpus <- release_corpus[c(corpus_indeces[meta(release_corpus, "organisation")  == "Ministry of Defence"][1:short_corpus_size],
                                 corpus_indeces[meta(release_corpus, "organisation")  == "Wales Office"][1:short_corpus_size],
                                 corpus_indeces[meta(release_corpus, "organisation")  == "Department for Environment, Food & Rural Affairs"][1:short_corpus_size])]

short_corpus

table(unlist(meta(short_corpus, "organisation")))

short_dtm <- DocumentTermMatrix(short_corpus)
short_dtm <- removeSparseTerms(short_dtm, 1-(5/length(short_corpus)))
rownames(short_dtm) <- c(rep("Defence", 20), rep("Wales", 20), rep("Environment", 20))

dist_dtm <- dist(short_dtm)
out <- hclust(dist_dtm, method = "ward.D")
plot(out)

##### LDA の推定
library(topicmodels)
lda_out <- LDA(dtm, 6)

posterior_lda <- posterior(lda_out)
lda_topics <- data.frame(t(posterior_lda$topics))
mean_topic_matrix <- matrix(
    NA,
    nrow = 6,
    ncol = 6,
    dimnames = list(
        names(table(org_labels)),
        str_c("Topic_", 1:6)
    )
)

for(i in 1:6){
  mean_topic_matrix[i,] <- apply(lda_topics[, which(org_labels == rownames(mean_topic_matrix)[i])], 1, mean)
}

round(mean_topic_matrix, 2)

##### トピックと関連する単語
terms(lda_out, 10)

##### CTM の推定
ctm_out <- CTM(dtm, 6)
terms(ctm_out, 10)

##### 事後確率の推定
posterior_ctm <- posterior(ctm_out)
ctm_topics <- data.frame(t(posterior_ctm$topics))

par(mfrow = c(2,3), cex.main = .8, pty = "s", mar = c(5, 5, 1, 1))
for(topic in 1:2){
  for(orga in names(table(org_labels))){
    tmp.data <- ctm_topics[topic, org_labels == orga]
    plot(1:ncol(tmp.data),
         sort(as.numeric(tmp.data)),
         type = "l",
         ylim = c(0, 1),
         xlab = "Press releases",
         ylab = str_c("Posterior probability, topic ", topic),
         main = str_replace(orga, "Department for", "")
         )
  }
}
