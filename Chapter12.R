## 第12 章アメリカ上院議員間のコラボレーション・ネットワーク
### 12.1 法案に関する情報
library(RCurl)
library(stringr)
library(XML)
library(igraph)

# Iterate over all 4059 pieces of legislation
for(i in 1:4059){
  # Generate the unique URL for each piece of legislation
  url <- str_c("http://thomas.loc.gov/cgi-bin/bdquery/D?d111:",
               i, ":./list/bss/d111SN.lst:@@@P")
  # Download the page
  bill_result <- getURL(url,
                        useragent = R.version$version.string,
                        httpheader = c(from = "i@datacollection.com"))
  # Write the page to local hard drive
  write(bill_result, str_c("Bills_111/Bill_111_S", i, ".html"))
  # Print progress of download
  cat(i, "\n")
}

sponsor_regex <- "FLD003\\+@4\\(\\(@1\\([[:alpha:]+.]+"
cosponsor_regex <- "FLD004\\+@4\\(\\(@1\\([[:alpha:]+.]+"

html_source <- readLines("Bills_111/Bill_111_S1.html")
sponsor <- str_extract(html_source, sponsor_regex)
(sponsor <- sponsor[!is.na(sponsor)])
cosponsors <- unlist(str_extract_all(html_source, cosponsor_regex))
cosponsors[1:3]
length(cosponsors)


cleanUp <- function(x){
  name <- str_extract(x, "[[:alpha:]+.]+$")
  name <- str_replace_all(name, fixed("++"), ", ")
  name <- str_replace_all(name, fixed("+"), " ")
  name <- str_trim(str_replace(name, "Sen", ""))
  return(name)
}

cleanUp(sponsor)
cleanUp(cosponsors)

length(error_collection)

for(i in 1:length(error_collection)){
  bill_number <- as.numeric(error_collection[[i]][1])
  html_source <- readLines(str_c("Bills_111/Bill_111_S", bill_number, ".html"))
  count_withdrawn <- unlist(
    str_extract_all(
      html_source,
      "\\(withdrawn - [[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}\\)"
    )
  )
  sponsor_list[[str_c("S.", bill_number)]]$cosponsors <-
    sponsor_list[[str_c("S.", bill_number)]]$cosponsors[1:(length(
      sponsor_list[[str_c("S.", bill_number)]]$cosponsors) - length(
        count_withdrawn))]
}

all_senators <- unlist(sponsor_list)
all_senators <- unique(all_senators)
all_senators <- sort(all_senators)
head(all_senators)

#Figure 12.1
error_collection <- list()
sponsor_list <- list()
# Iterate over all 4059 pieces of legislation
for(i in 1:4059){
# Read the ith result
  html_source <- readLines(str_c("Bills_111/Bill_111_S", i, ".html"))
  # Extract and clean the sponsor
  sponsor <- unlist(str_extract_all(html_source, sponsor_regex))
  sponsor <- sponsor[!is.na(sponsor)]
  sponsor <- cleanUp(sponsor)
  # Extract and clean the cosponsors
  cosponsors <- unlist(str_extract_all(html_source, cosponsor_regex))
  cosponsors <- cleanUp(cosponsors)
  # Input the results into the sponsor list
  sponsor_list[[str_c("S.", i)]] <- list(sponsor = sponsor, cosponsors = cosponsors)
  # Collect potential points of error / number of cosponsors
  fail_safe <- str_extract(html_source,
  "COSPONSORS?\\(([[:digit:]]{1,3}|S)\\)")
  fail_safe <- fail_safe[!is.na(fail_safe)]
  # Error - no cosponsor string
  if(length(fail_safe) == 0){
    error_collection[[length(error_collection) + 1]] <- c(i, "String - COSPONSOR - not found")
  }
  # Error - found more cosponsors than possible
  if(fail_safe == "COSPONSOR(S)"){
    if(length(cosponsors) > 0){
      error_collection[[length(error_collection) + 1]] <- c(i, "Found cosponsors where there should be none")
    }
  } else{
    right_number <- str_extract(fail_safe, "[[:digit:]]+")
    # Error - Found wrong number of cosponsors
    if(length(cosponsors) != right_number){
      error_collection[[length(error_collection) + 1]] <- c(i, "Did not find the right number of cosponsors")
    }
  }
  # Error - Found no sponsors
  if(is.na(sponsor)){
    error_collection[[length(error_collection) + 1]] <- c(i, "No sponsors")
  }
  # Error - Found too many sponsors
  if(length(sponsor) > 1){
    error_collection[[length(error_collection) + 1]] <- c(i, "More than one sponsor")
  }
}

sponsor_matrix <- matrix(NA, nrow = 4059, ncol = length(all_senators))
colnames(sponsor_matrix) <- all_senators
rownames(sponsor_matrix) <- paste("S.", seq(1, 4059), sep ="")

for(i in 1:length(sponsor_list)){
  sponsor_matrix[i, which(all_senators == sponsor_list[[i]]$sponsor)] <-
    "Sponsor"
  if(length(sponsor_list[[i]]$cosponsors) > 0){
    for(j in 1:length(sponsor_list[[i]]$cosponsors)){
      sponsor_matrix[i, which(all_senators == sponsor_list[[i]]
                              $cosponsors[j])] <- "Cosponsor"
    }
  }
}
sponsor_matrix[30:35,31:34]

### 12.2 上院議員の情報
url <- "http://bioguide.congress.gov/biosearch/biosearch.asp"
form_page <- getURL(url)
write(form_page, "form_page.html")

form_page <- str_c(readLines("form_page.html"), collapse = "")
destination <- str_extract(form_page, "<form.+?>")
cat(destination)

form <- str_extract(form_page, "<form.+?</form>")
cat(str_c(unlist(str_extract_all(form, "<INPUT.+?>")), collapse = "\n"))

cat(str_c(unlist(str_extract_all(form, "<SELECT.+?>")), collapse = "\n"))

senator_site <- postForm(uri = "http://bioguide.congress.gov/biosearch/biosearch1.asp",
                         lastname = "",
                          firstname = "",
                          position = "Senator",
                          state = "",
                          party = "",
                          congress = "111",
                          style = 'POST')
write(senator_site, "senators.html")

senator_site <- readLines("senators.html", encoding = "UTF-8")
senator_site <- str_c(senator_site, collapse = "")
senators <- readHTMLTable(senator_site, encoding="UTF-8")[[2]]
senators <- as.data.frame(sapply(senators, as.character), stringsAsFactors = F)
names(senators)[names(senators)=="Birth-Death"] <- "BiDe"
head(senators, 3)

senators$match_names <- senators[,1]
senators$match_names <- tolower(senators$match_names)
senators$match_names <- str_extract(senators$match_names, "[[:alpha:]]+")
all_senators_dat <- data.frame(all_senators)
all_senators_dat$match_names <- str_extract(all_senators_dat$all_senators, "[[:alpha:]]+")
all_senators_dat$match_names <- tolower(all_senators_dat$match_names)
senators <- merge(all_senators_dat, senators, by = "match_names")
senators[,2] <- as.character(senators[,2])
senators[,3] <- as.character(senators[,3])
senators[,2] <- tolower(senators[,2])
senators[,3] <- tolower(senators[,3])

allDup <- function(x){
  duplicated(x) | duplicated(x, fromLast = TRUE)
}
dup_senators <- senators[allDup(senators[,1]),]
senators <- senators[rownames(senators) %in% rownames(dup_senators) == F,]
dup_senators[str_detect(dup_senators[,3], "\\("), 3] <- str_replace_all(dup_senators[str_detect(dup_senators[,3], "\\("), 3], ", .+?\\(", ", ")
dup_senators[str_detect(dup_senators[,3], "\\("), 3] <- str_replace_all(dup_senators[str_detect(dup_senators[,3], "$"), 3], "$", "")

#Remove Bracket(Add by Shinichi Takayanagi)
dup_senators[,3] <- str_replace_all(dup_senators[,3], "\\)", "")

for(i in nrow(dup_senators):1){
  if(str_detect(dup_senators[i, 2], str_extract(dup_senators[i, 3], "[ˆ,][[:alpha:] .]+?$")) == F){
    dup_senators <- dup_senators[-i,]
  }
}

senators <- rbind(senators, dup_senators)
senators$rownames <- as.numeric(rownames(senators))
senators <- senators[order(senators$rownames),]
dim(senators)

colnames(sponsor_matrix) <- senators$all_senators

all_senators <- unlist(sponsor_list)
all_senators <- unique(all_senators)

### 12.3 ネットワーク構造の解析
edgelist_sponsors <- matrix(NA, nrow = 0, ncol = 2)
for(i in 1:nrow(sponsor_matrix)){
  if(length(which(!is.na(sponsor_matrix[i,]))) > 1){
    edgelist_sponsors <- rbind(
      edgelist_sponsors,
      t(combn(colnames(sponsor_matrix)[which(!is.na
                                                (sponsor_matrix[i,]))], 2))
      )
    }
  }
dim(edgelist_sponsors)

sponsor_network <- graph.edgelist(edgelist_sponsors, directed = F)

result <- matrix(
  NA,
  ncol = ncol(sponsor_matrix),
  nrow = 2,
  dimnames = list(
    c("Sponsor", "Cosponsor"),
    colnames(sponsor_matrix)
  )
)
for(i in 1:ncol(sponsor_matrix)){
  result[1, i] <- sum(sponsor_matrix[, i] == "Cosponsor", na.rm = T)
  result[2, i] <- sum(sponsor_matrix[, i] == "Sponsor", na.rm = T)
}
result <- t(result)

adj_sponsor <- get.adjacency(sponsor_network)
adj_sponsor[lower.tri(adj_sponsor)] <- 0

s10 <- min(sort(as.matrix(adj_sponsor), decreasing = T)[1:10])
max_indices <- which(as.matrix(adj_sponsor) >= s10, arr.ind = T)
export_names <- matrix(NA, ncol = 2, nrow = 10)
for(i in 1:nrow(max_indices)){
  export_names[i, 1] <- rownames(adj_sponsor)[max_indices[i,1]]
  export_names[i, 2] <- colnames(adj_sponsor)[max_indices[i,2]]
}

E(sponsor_network)$weight <- 1
sponsor_network_weighted <- simplify(sponsor_network)
sponsor_network_weighted
head(E(sponsor_network_weighted)$weight)

plot_sponsor <- sponsor_network_weighted
plot_sponsor <- delete.edges(plot_sponsor, which(E(plot_sponsor)$weight <
                                                   (mean(E(plot_sponsor)$weight) + sd(E(plot_sponsor)$weight))))
plot(plot_sponsor, edge.color = "lightgray", vertex.size = 0, vertex.frame.color = NA, vertex.color = "white", vertex.label.color = "black")
