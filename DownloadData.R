library(XML)

url <- "http://www.legaseriea.it/it/"
html <- htmlTreeParse(url,useInternalNodes=T)
teamlinks <- xpathSApply(html,path="//div//dl[@class='serie_a']//dd//a",xmlGetAttr,"href")
teams <- xpathSApply(html,path="//div//dl[@class='serie_a']//dd//a",xmlGetAttr,"title")
teamlinks <- paste0("http://www.legaseriea.it",teamlinks)

i <- 1

#### enter for loops
for(i in 1:20){

        team <- teams[i]
        teamlink <- teamlinks[i]
        html <- htmlTreeParse(teamlink,useInternalNodes=T)
        
        #get links
        playerslink <- xpathSApply(html,path="//ul[@class='menu_tab']//li//a",xmlGetAttr,"href")[3]
        playerslink <- paste0("http://www.legaseriea.it/",playerslink)
        statslink <- xpathSApply(html,path="//ul[@class='menu_tab']//li//a",xmlGetAttr,"href")[5]
        statslink <- paste0("http://www.legaseriea.it/",statslink)
        playersstatslink <- paste0(statslink,"/giocatori")
        
        
        #get players
        #html <- htmlTreeParse(playerslink,useInternalNodes=T)
        players <- readHTMLTable(playerslink)[[1]]
        colnames(players) <- c("numero","nome","datadinascita","ruolo","nazionalita","partitegiocate","reti","cartellinigialli","cartellinigiallirossi","cartellinirossi")
        
        players <- players[-1,]
        players$squadra <- team
        
        if (i == 1) {
                totalplayers <- players
        } else {
                totalplayers <- rbind(totalplayers,players)
        }


        #get team stats
        #html <- htmlTreeParse(stats,useInternalNodes=T)
        stats <- readHTMLTable(statslink)[[3]]
        colnames(stats) <- c("variabile","valore")
        
        stats <- stats[-1,]
        stats$squadra <- team
        
        if (i == 1) {
                totalstats <- stats
        } else {
                totalstats <- rbind(totalstats,stats)
        }
        
        
        #get players stats
        #html <- htmlTreeParse(stats,useInternalNodes=T)
        playersstats <- readHTMLTable(playersstatslink)[[5]]
        colnames(playersstats) <- c("nome","presenze","minutigiocati","goal","ammonizioni","2ammonizioni","espulsioni","pallerecuperate","passaggiriusciti","tiri")
        
        #stats <- stats[-1,]
        playersstats$squadra <- team
        
        if (i == 1) {
                totalplayersstats <- playersstats
        } else {
                totalplayersstats <- rbind(totalplayersstats,playersstats)
        }
        
        
        
 }



