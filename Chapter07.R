# 『Rによるデータ自動収集 -- Webスクレイピングとテキストマイニングの実践ガイド --』
## 第 7 章 SQL とリレーショナルデータベース

### 7.4.2 DBI ベースのパッケージを介したR とSQL の対話

# パッケージの読み込み
library(RSQLite)

# 接続の確立
sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, "birthdays.db")

# 「そのままの」SQL
sql <- "SELECT * FROM birthdays"
res <- dbGetQuery(con, sql)
res

res <- dbSendQuery(con, sql)
fetch(res)

# 一般情報
dbGetInfo(con)[1]

# テーブルのリスト
dbListTables(con)

# テーブルの読み込み
res <- dbReadTable(con, "birthdays")
res

# テーブルの書き出し
dbWriteTable(con, "test", res)

# テーブルが存在するか？
dbExistsTable(con, "test")

# テーブルの削除
dbRemoveTable(con, "test")

# データ型の確認
dbDataType(con, res$nameid)
dbDataType(con, res$firstname)
dbDataType(con, res$birthday)

# トランザクション管理
dbBegin(con)
dbRollback(con)
dbBegin(con)
dbCommit(con)

# 接続を閉じる
dbDisconnect(con)


### 7.4.3 RODBC を介したR とSQL の会話

# パッケージの読み込み
require(RODBC)

# 接続の確立
con <- odbcConnect("db1")

# 「そのままの」SQL
sql <- "SELECT * FROM birthdays ;"
res <- sqlQuery(con, sql)
res

# 一般情報
odbcGetInfo(con)[3]

# テーブルのリスト
sqlTables(con)[, 3:5]

odbcDataSources()

res <- sqlFetch(con, "birthdays")
res

# テーブルの書き出し
test <- data.frame(x = 1:3, y = letters[7:9])
sqlSave(con, test, "test")
sqlFetch(con, "test")

# テーブルを空にする
sqlClear(con, "test")
sqlFetch(con, "test")

# テーブルの削除
sqlDrop(con, "test")
