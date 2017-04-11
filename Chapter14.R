## 第14章Twitter による2014年アカデミー賞予測
### 14.1 Twitter API: 概要
library("twitteR")
library("streamR")

# データは下記から取得可能
# https://depot.uni-konstanz.de/cgi-bin/exchange.pl?g=drw7p7frm6
# Twitterの規約により原著で使用しているファイルそのものを提供できないため、このような措置となったとのこと
# また、原文のミスにより変数名がdatに変更されているのでそれにあうようにする
# filterStream("tweets_oscars.json", track = c("Oscars", "Oscars2014"), timeout = 10800, oauth = twitCred)
# tweets <- parseTweets("tweets_oscars.json", simplify = TRUE)
load("tweets_all_red.RData")
dat <- tweets_all

### 14.2 Twitter ベースでの2014 年度アカデミー賞予測
library("lubridate")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
Sys.setlocale("LC_TIME", "English")
dat$created_at[1]
dat$time <- as.POSIXct(dat$created_at, tz = "UTC", format = "%a %b %d %H:%M:%S %z %Y")

dat$round_hour <- round_date(dat$time, unit = "hour")
plot_time <- as.data.frame(table(dat$round_hour))
plot_time <- plot_time[-nrow(plot_time),]

plot(plot_time[,2], type = "l", xaxt = "n", xlab = "Hour", ylab = "Frequency")
axis(1, at = c(1, 20, 40, 60), labels = plot_time[c(1, 20, 40, 60), 1])

library("stringr")
library("plyr")

unlist(dat[1234,])

# 原文ではもともと入っているデータが存在しないので、当方にて作成
dat$lotext <- str_to_lower(dat$text)

actor <- c("matthew mcconaughey", "christian bale", "bruce dern", "leonardo dicaprio", "chiwetel ejiofor")
actress <- c("cate blanchett", "amy adams", "sandra bullock", "judi dench", "meryl streep")
film <- c("(12|twelve) years a slave", "american hustle", "captain phillips", "dallas buyers club", "gravity", "nebraska", "philomena", "(the )?wolf of wall street")

tmp_actor <- lapply(dat$lotext, str_detect, actor)
dat_actor <- ldply(tmp_actor)
colnames(dat_actor) <- c("mcconaughey", "bale", "dern", "dicaprio", "ejiofor")

tmp_actress <- lapply(dat$lotext, str_detect, actress)
dat_actress <- ldply(tmp_actress)
colnames(dat_actress) <- c("blanchett", "adams", "bullock", "dench", "streep")
tmp_film <- lapply(dat$lotext, str_detect, film)
dat_film <- ldply(tmp_film)
colnames(dat_film) <- c("twelve_years", "american_hustle", "capt_phillips", "dallas_buyers", "gravity", "nebraska", "philomena", "wolf_wallstreet")

apply(dat_actor, 2, sum)
apply(dat_actress, 2, sum)
apply(dat_film, 2, sum)

tmp_actor2 <- lapply(actor, agrep, dat$lotext)
length_actor <- unlist(lapply(tmp_actor2, length))
names(length_actor) <- c("mcconaughey", "bale", "dern", "dicaprio", "ejiofor")
tmp_actress2 <- lapply(actress, agrep, dat$lotext)
length_actress <- unlist(lapply(tmp_actress2, length))
names(length_actress) <- c("blanchett", "adams", "bullock", "dench", "streep")
length_actor
length_actress
