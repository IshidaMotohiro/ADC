## 第13章半構造化されたドキュメントから情報を抜き出す
### 13.1 FTPサーバからデータをダウンロードする
library(RCurl)
library(stringr)
dir.create("Data")

#Original Code
#ftp <- "ftp://ftp.wcc.nrcs.usda.gov/data/climate/table/temperature/history/california/"
#filelist <- getURL(ftp, dirlistonly = TRUE)
#str_sub(filelist, 1, 119)
#filelist <- unlist(str_split(filelist, "\r\n"))
#filelist <- filelist[!filelist == ""]
#filelist[1:3]

#Modified URL
ftp <- "http://www.wcc.nrcs.usda.gov/ftpref/data/climate/table/temperature/history/california/"
library(rvest)
page <- read_html(ftp)
filelist <- page %>% html_nodes("a") %>% html_attr("href")
filelist <- filelist[-(1:5)]
filelist[1:3]

filesavg <- str_detect(filelist, "tavg")
filesavg <- filelist[filesavg]
filesavg[1:3]
urlsavg <- str_c(ftp, filesavg)
length(urlsavg)
urlsavg[1]

for (i in seq_along(urlsavg)) {
  fname <- str_c("Data/", filesavg[i])
  if (!file.exists(fname)) {
    download.file(urlsavg[i], fname)
    Sys.sleep(1)
  }
}

length(list.files("Data"))
list.files("Data")[1:3]

### 13.2 半構造化されたテキストデータをパースする
txt <- character()
for (i in 1:length(filesavg)) {
  txt <- c(txt, readLines(str_c("Data/", filesavg[i])))
}

txt <- str_c(txt, collapse = "\n")
txtparts <- unlist(str_split(txt, "----------\n"))

str_sub(txtparts[28:30], 1, 134)
cat(str_sub(txtparts[28], 1, 604))

txtparts <- str_replace(txtparts,"\n\\*\\*\\*This data is provisional and subject to change.", "")
txtparts <- str_replace(txtparts,"ˆ\n", "")
txtparts <- txtparts[txtparts!=""]

year <- str_extract(txtparts, "[[:digit:]]{2}  Average Air Temperature")
year[1:4]
year <- str_extract(year, "[[:digit:]]{2}")
year <- ifelse(year < 20, str_c(20, year), str_c(19, year))
year <- as.numeric(year)
year[5:15]

station <- str_extract(txtparts, "Station : .+?\n")
station[1:2]

station <- str_replace_all(station, "(Station : )|(\n)", "")
station[1:2]

station <- str_split(station, ", ")
station[1]

id <- sapply(station, "[", 1)
name <- sapply(station, "[", 2)
id[1:3]
name[1:3]

temperatures <- str_extract(txtparts, regex("day.*", dotall = TRUE))
tf <- tempfile()
writeLines(temperatures[5], tf)

temptable <- read.fwf(tf, width=c(3, 7, rep(6, 11)), stringsAsFactors = F)
temptable[c(1:5,32:38), 1:10]

temptable <- temptable[3:33, -1]
temptable <- as.numeric(unlist(temptable))

day <- rep(1:31, 12)
month <- rep(c(10:12, 1:9), each = 31)

temptable <- data.frame(avgtemp = temptable, day = day, month = month, year = year[5], name = name[5], id = id[5])
head(temptable, 3)

parseTemp <- function(filename)
{
  # Added by Shinichi Takayanagi
  # get text
  txt <- paste( readLines(filename), collapse="\n")
  # split text into year tables
  txtparts <- unlist(str_split(txt, "----------\n"))
  # cleansing
  txtparts <- str_replace(txtparts, "\n\\*\\*\\*This data is provisional and subject to change.", "")
  txtparts <- str_replace(txtparts,"ˆ\n","")
  txtparts <- txtparts[txtparts!=""]
  # get the year
  #year <- str_extract(txtparts,"[[:digit:]]{2} Average Air Temperature")
  year <- str_extract(txtparts, "[[:digit:]]{2}  Average Air Temperature")
  year <- str_extract(year,"[[:digit:]]{2}")
  year <- ifelse(year < 20, str_c(20,year), str_c(19,year))
  year <- as.numeric(year)
  # get station and name
  station <- str_extract(txtparts, "Station : .+?\n")
  station <- str_replace_all(station, "(Station : )|(\n)", "")
  station <- str_split(station,", ")
  id <- sapply(station, '[', 1)
  name <- sapply(station, '[', 2)
  # extract part of the sections that contains daily temperatures
  #temperatures <- str_extract(txtparts, "day.*")
  temperatures <- str_extract(txtparts, regex("day.*", dotall = TRUE))
  # prepare object to store temperature data
  tempData <- data.frame(avgtemp = NA, day = NA, month = NA, year = NA, id = "", name = "")
  # generate day and month patterns matching the order of temperatures
  day <- rep(1:31, 12)
  month <- rep( c(10:12,1:9), each=31 )
  # helper function
  doTemp <- function(temperatures, year, name, id){
    # write fixed width table into temporary file
    tf <- tempfile()
    writeLines(temperatures, tf)
    # read in data and transform to data frame
    temptable <- read.fwf(tf, width = c(3,7,6,6,6,6,6,6,6,6,6,6,6), stringsAsFactors = F)
    # keep only those lines and rows entailing day-temperatures
    temptable <- temptable[3:33, -1]
    # transform data frame of strings to vector of type numeric
    temptable <- suppressWarnings(as.numeric(unlist(temptable)))
    # combine data
    temptable <- data.frame(avgtemp = temptable, day = day, month = month, year = year, name = name, id = id)
    # add data to tempData
    tempData <<- rbind(tempData, temptable)
  }
  mapply(doTemp, temperatures, year, name, id)
  tempData <- tempData[!is.na(tempData$avgtemp),]
  return(tempData)
}

tempData1 <- parseTemp(str_c("Data/", filesavg[1]))
dim(tempData1)
tempData1[500:502, ]

parseTemps <- function(filenames) {
  tmp <- lapply(filenames, parseTemp)
  tempData <- NULL
  for (i in seq_along(tmp)) tempData <- rbind(tempData, tmp[[i]])
  return(tempData)
}

tempData <- parseTemps(str_c("Data/", filesavg))
dim(tempData)

### 13.3 測候所と気温データの可視化
dir.create("Data_CA")
#Original: download.file("ftp://ftp.wcc.nrcs.usda.gov/states/ca/jchen/CA_sites.dat", "Data_CA/CA_sites.dat")
download.file("http://www.wcc.nrcs.usda.gov/ftpref/states/ca/jchen/CA_sites.dat", "Data_CA/CA_sites.dat")
stationData <- read.csv("Data_CA/CA_sites.dat", header = F, sep="|")[,-c(1,2,7:9)]
names(stationData) <- c("name","lat","lon","alt")

head(stationData,2)

stationData$lon <- stationData$lon * -1
stationData[, c("lat", "lon")] <- stationData[, c("lat", "lon")]/100
stationData$alt <- stationData$alt/3.2808399
stationData <- stationData[order(stationData$lat), ]
head(stationData, 2)

#Original code
#library("RgoogleMaps")
#Does not work...
#map <- GetMap.OSM(latR = c(37.5, 42), lonR = c(-125, -115), scale = 5000000, destfile = "map.png", GRAYSCALE = TRUE, NEWMAP = TRUE)
#png("stationmap.png", width = dim(readPNG("map.png"))[2], height = dim(readPNG("map.png"))[1])
#PlotOnStaticMap(map, lat = stationData$lat, lon = stationData$lon, cex = 2, pch = 19, col = rgb(0, 0, 0, 0.5), add = FALSE)
#Modified
library("ggmap")
location = c(-125, 37.5, -115, 42)
map <- get_map(location = location, source = "osm")
ggmap(map) + geom_point(data = stationData, aes(x = lon, y = lat), alpha = 0.5, size = 5)
ggsave("stationmap.png")


monthlyTemp <- aggregate(x = tempData$avgtemp, by = list(name = tempData$name, month = tempData$month), FUN = mean)

stationNames <- c("ADIN MTN", "INDEPENDENCE CAMP", "SQUAW VALLEY G.C.", "SPRATT CREEK", "LEAVITT MEADOWS","POISON FLAT")
stationAlt <- stationData[match(stationNames, stationData$name), ]$alt
stationLat <- stationData[match(stationNames, stationData$name), ]$lat
stationLon <- stationData[match(stationNames, stationData$name), ]$lon

plotTemps <- function(i)
{
  iffer <- monthlyTemp$name == stationNames[i]
  plot(monthlyTemp[iffer, c("month", "x")],
       type = "b",
       main = str_c(stationNames[i],"(",
       round(stationAlt[i]), "m)", "\n Lat.= ", stationLat[i], " Lon.= ", stationLon[i]),
       ylim = c(-15, 25), ylab = "average temperature")
  abline(h = 0,lty = 2)
  iffer2 <- tempData$name == stationNames[i]
  points(tempData$month[iffer2] + tempData$day[iffer2] *0.032, jitter(tempData$avgtemp[iffer2], 3), col = rgb(0.2, 0.2, 0.2, 0.1), pch = ".")
}

par(mfrow = c(2, 3))
for (i in seq_along(stationNames)) plotTemps(i)
