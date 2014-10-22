library(XML)
html <- htmlTreeParse("http://www.corrieredellosport.it/live/SerieA/calendario.shtml",useInternalNodes = T)

clubs <- xpathSApply(html,"//li[@class='club']",xmlValue)
giornata <- lapply(1:19, function(x) paste0("Giornata ",rep(x,10)))
giornata <- unlist(giornata)
casa <- clubs[seq(from = 1, to = 380,by = 2)]
fuoricasa <- clubs[seq(fr+om = 2, to = 380,by = 2)]
calendario <- data.frame(giornata = giornata, casa = casa, fuoricasa = fuoricasa)
calendario$girone <- "andata" 

tmp <- calendario
tmp$girone <- "ritorno"
tmp$casa <- calendario$fuoricasa
tmp$fuoricasa <- calendario$casa

calendario <- rbind(calendario,tmp)
calendario$girone <- factor(calendario$girone)




#andate
andate <- xpathSApply(html,"//div[@class='data_cal']",xmlValue)
andate <- substr(andate,2,7)
andate <- unlist(lapply(1:19, function(x) rep(andate[x],10)))

#andate
ritorni <- xpathSApply(html,"//div[@class='data_cal_r']",xmlValue)
ritorni <- substr(ritorni,2,7)
ritorni <- unlist(lapply(1:19, function(x) rep(ritorni[x],10)))

calendario$date <- c(andate,ritorni)

calendario2014 <- calendario
save(calendario2014,file = "calendario2014.Rdata")
