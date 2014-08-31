library(XML)

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
        players <- xpathSApply(html,path="//ul[@class='menu_tab']//li//a",xmlGetAttr,"href")[3]
        players <- paste0("http://www.legaseriea.it/",players)
        
        html <- htmlTreeParse(players,useInternalNodes=T)
        players <- readHTMLTable(players)[[1]]
        colnames(players) <- c("numero","nome","datadinascita","ruolo","nazionalita","partitegiocate","reti","cartellinigialli","cartellinirossi")
        
        players <- players[-1,]
        players$squadra <- team
        
        if (i == 1) {total <- players}
        else{total <- rbind(total,players)}
        
        
}

total$squadra <- factor(total$squadra)

