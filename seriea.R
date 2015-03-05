library(plyr)
library(dplyr)
library(rvest)


# SCRAPE LINKS ------------------------------------------------------------
url <- "http://football-data.co.uk/italym.php"
h <- html(url)


src <- data.frame(links = h %>% html_nodes(xpath="//a") %>% html_attr("href"),
                     text = h %>% html_nodes(xpath="//a") %>% html_text())

src <- src[grep("serie a",src$text,ignore.case=T),]
season <- h %>% html_nodes(xpath="//i") %>% html_text()
src$season <- season[grep("season",season,ignore.case=T)]
src$links <- paste0("http://football-data.co.uk/",src$links)



# READ FILES --------------------------------------------------------------
data <- llply(src$links,read.csv,header = T,stringsAsFactor = F)
names(data) <- gsub("[^a-z0-9]","",tolower(src$season))

full <- tbl_df(rbind.fill(data))
full <- full[,-grep("^X.",colnames(full))]
tmp <- llply(data,function(x) dim(x)[1])
full$season <- unname(unlist(mapply(rep,names(tmp),tmp,SIMPLIFY=T)))
full <- full %>% filter(FTR != "")
full <- full %>% mutate(FTR = factor(FTR))
levels(full$FTR) <- c("Away","Draw","Home")
full$dowloadtimestamp <- Sys.time()
save(full, file = paste0("./input/full_",Sys.Date(),".rda"))




# INSPECT BETTER ----------------------------------------------------------
better <- 7
ix <- (23+(3*(better-1))):(23+3*(better-1)+2)
colnames(full)[ix]
tmp <- apply(full[,ix],1,which.min)
lengths <- sapply(tmp,length)
tmp[lengths == 0] <- NA

results <- sapply(tmp,function(x) x[[1]])

results <- factor(results,labels=c("Home","Draw","Away"))

results <- relevel(results,c("Draw"))
results <- relevel(results,c("Away"))

mosaicplot(full$FTR~results,shade=T)
