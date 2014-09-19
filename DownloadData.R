library(XML)
library(reshape2)
library(lubridate)

url <- "http://www.legaseriea.it/it/"
html <- htmlTreeParse(url,useInternalNodes=T)
teamlinks <- xpathSApply(html,path="//div//dl[@class='serie_a']//dd//a",xmlGetAttr,"href")
teams <- xpathSApply(html,path="//div//dl[@class='serie_a']//dd//a",xmlGetAttr,"title")
teamlinks <- paste0("http://www.legaseriea.it",teamlinks)



#### enter for loops
for(i in 1:20){

        team <- teams[i]
        teamlink <- teamlinks[i]
        html <- htmlTreeParse(teamlink,useInternalNodes=T)
        
        #get links
        historylink <- xpathSApply(html,path="//ul[@class='menu_tab']//li//a",xmlGetAttr,"href")[1]
        historylink <- paste0("http://www.legaseriea.it/",historylink)
        statslink <- xpathSApply(html,path="//ul[@class='menu_tab']//li//a",xmlGetAttr,"href")[5]
        statslink <- paste0("http://www.legaseriea.it/",statslink)
        playersstatslink <- paste0(statslink,"/giocatori")

        #### get history
        histtmp <- readHTMLTable(historylink,header = T)[[1]]
        histtmp$squadra <- team
        
        if (i == 1) {
                history <- histtmp
        } else {
                history <- rbind(history,histtmp)
        }
        
        
        #### get stats
        #get tempi gol
        tempigoltmp <- expand.grid(c("Primo tempo","Secondo tempo"),c("Fatti","Subiti"))
        colnames(tempigoltmp) <- c("Tempo","Gol")
        
        tmp <- rbind(readHTMLTable(statslink)[[1]][c(2,4),2:4],readHTMLTable(statslink)[[2]][c(2,4),2:4])
        colnames(tmp) <- c("0_15","16_30","31_45")
        tempigoltmp <- cbind(tempigoltmp,tmp)
        tempigoltmp <- melt(tempigoltmp,id.vars = c("Tempo","Gol"),value.name = "valore")
        tempigoltmp$variabile <- do.call(paste, tempigoltmp[,1:3])
        tempigoltmp <- tempigoltmp[,c("variabile","valore")]

        #get team stats
        statstmp <- readHTMLTable(statslink)[[3]]
        colnames(statstmp) <- c("variabile","valore")
        
        statstmp <- statstmp[-1,]
        statstmp <- rbind(statstmp,tempigoltmp)
        statstmp$squadra <- team
        
        if (i == 1) {
                stats <- statstmp
        } else {
                stats <- rbind(stats,statstmp)
        }
        
        
        ####get players stats
        playersstatstmp <- readHTMLTable(playersstatslink)[[5]]
        colnames(playersstatstmp) <- c("nome","presenze","minutigiocati","goal","ammonizioni","2ammonizioni","espulsioni","pallerecuperate","passaggiriusciti","tiri")
        
        #stats <- stats[-1,]
        playersstatstmp$squadra <- team
        
        if (i == 1) {
                playersstats <- playersstatstmp
        } else {
                playersstats <- rbind(playersstats,playersstatstmp)
        }
            
}
rm(histtmp,playersstatstmp,statstmp,tmp,statslink,teamlinks,html,team,teamlink,
   tempigoltmp,historylink,i,playersstatslink)



classifica <- readHTMLTable("http://www.legaseriea.it/it/serie-a-tim/classifica-estesa/classifica")[[1]]
classifica <- classifica[,c(-2,-18)]
colnames(classifica) <- c("squadra","punti","giocate","vinte","pareggiate","perse","giocate - in casa","vinte - in casa","pareggiate - in casa","perse - in casa","giocate - fuori casa","vinte - fuori casa","pareggiate - fuori casa","perse - fuori casa","reti fatte","reti subite")


#clean stats
stats <- dcast(stats,squadra~variabile,value.var = "valore")
colnames(stats) <- gsub(" ","",tolower(colnames(stats)))
colnames(stats) <- gsub("%","perc_",tolower(colnames(stats)))
colnames(stats) <- gsub("à","a",tolower(colnames(stats)))
stats$possessopalla <- ms(stats$possessopalla)
stats$supremaziaterritoriale <- ms(stats$supremaziaterritoriale)
stats[,15:26] <- sapply(15:26,function(x) gsub("\\(%\\)","\\(0%\\)",stats[,x]))
stats[,15:26] <- sapply(15:26,function(x) as.numeric(gsub("^[0-9]|[^0-9]","",stats[,x])))


save(classifica,history,playersstats,stats,file = paste0("./Rdata/",as.character(Sys.Date()),".rda"))


