setwd("~/GitHub/SerieA")
library(XML)
library(reshape2)
library(lubridate)
library(dplyr)

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


#format columns
classifica <- tbl_df(classifica)
colnames(classifica) <- gsub("[^a-z0-9]","",tolower(colnames(classifica)))
classifica <- mutate(classifica,
       punti = as.numeric(as.character(punti)),                            
       giocate = as.numeric(as.character(giocate)),                        
       vinte = as.numeric(as.character(vinte)),                            
       pareggiate = as.numeric(as.character(pareggiate)),                  
       perse = as.numeric(as.character(perse)),                            
       giocateincasa = as.numeric(as.character(giocateincasa)),            
       vinteincasa = as.numeric(as.character(vinteincasa)),                
       pareggiateincasa = as.numeric(as.character(pareggiateincasa)),      
       perseincasa = as.numeric(as.character(perseincasa)),                
       giocatefuoricasa = as.numeric(as.character(giocatefuoricasa)),      
       vintefuoricasa = as.numeric(as.character(vintefuoricasa)),          
       pareggiatefuoricasa = as.numeric(as.character(pareggiatefuoricasa)),
       persefuoricasa = as.numeric(as.character(persefuoricasa)),          
       retifatte = as.numeric(as.character(retifatte)),                    
       retisubite = as.numeric(as.character(retisubite))
       )



history <- tbl_df(history)
colnames(history) <- gsub("[^a-z0-9]","",tolower(colnames(history)))
history <- mutate(history,
        posizione = as.numeric(as.character(posizione)),
        punti = as.numeric(as.character(punti)),
        partitegiocate = as.numeric(as.character(partitegiocate)),
        vinte = as.numeric(as.character(vinte)),
        pareggiate = as.numeric(as.character(pareggiate)),
        perse = as.numeric(as.character(perse)),
        squadra = as.factor(squadra)
       )






playersstats <- tbl_df(playersstats)
colnames(playersstats) <- gsub("[^a-z0-9]","",tolower(colnames(playersstats)))
colnames(playersstats)[6] <- "doppieammonizioni"

playersstats <- mutate(playersstats,
        presenze = as.numeric(as.character(presenze)),                
        minutigiocati = as.numeric(as.character(minutigiocati)),      
        goal = as.numeric(as.character(goal)),                        
        ammonizioni = as.numeric(as.character(ammonizioni)),          
        doppieammonizioni = as.numeric(as.character(doppieammonizioni)),        
        espulsioni = as.numeric(as.character(espulsioni)),            
        pallerecuperate = as.numeric(as.character(pallerecuperate)),  
        passaggiriusciti = as.numeric(as.character(passaggiriusciti)),
        tiri = as.numeric(as.character(tiri)),
        squadra = as.factor(squadra)
        )





stats <- tbl_df(stats)
colnames(stats) <- gsub("[^a-z0-9]","",tolower(colnames(stats)))

stats <- mutate(stats,
        percattaccoallaporta = as.numeric(as.character(percattaccoallaporta)),    
        percpassaggiriusciti = as.numeric(as.character(percpassaggiriusciti)),    
        percpericolosita = as.numeric(as.character(percpericolosita)),            
        percprotezionearea = as.numeric(as.character(percprotezionearea)),        
        angoli = as.numeric(as.character(angoli)),                                
        fallicommessi = as.numeric(as.character(fallicommessi)),                  
        giocateutili = as.numeric(as.character(giocateutili)),                    
        goalfatti = as.numeric(as.character(goalfatti)),                          
        pallegiocate = as.numeric(as.character(pallegiocate)),                    
        tiri = as.numeric(as.character(tiri)),                                    
        tiridentro = as.numeric(as.character(tiridentro)),                        
        primotempofatti015 = as.numeric(as.character(primotempofatti015)),        
        secondotempofatti015 = as.numeric(as.character(secondotempofatti015)),    
        primotemposubiti015 = as.numeric(as.character(primotemposubiti015)),      
        secondotemposubiti015 = as.numeric(as.character(secondotemposubiti015)),  
        primotempofatti1630 = as.numeric(as.character(primotempofatti1630)),      
        secondotempofatti1630 = as.numeric(as.character(secondotempofatti1630)),  
        primotemposubiti1630 = as.numeric(as.character(primotemposubiti1630)),    
        secondotemposubiti1630 = as.numeric(as.character(secondotemposubiti1630)),
        primotempofatti3145 = as.numeric(as.character(primotempofatti3145)),      
        secondotempofatti3145 = as.numeric(as.character(secondotempofatti3145)),  
        primotemposubiti3145 = as.numeric(as.character(primotemposubiti3145)),    
        secondotemposubiti3145 = as.numeric(as.character(secondotemposubiti3145))
        )

##### SAVE
save(classifica,history,playersstats,stats,file = paste0("./Rdata/",as.character(Sys.Date()),".rda"))


