library(XML)

url <- "http://www.corrieredellosport.it/live/SerieA/probabili-formazioni.shtml"
html <- htmlTreeParse(url,useInternalNodes = T)

links <- paste0("http://www.corrieredellosport.it/live/SerieA/",xpathSApply(html,"//div//a[@class='home']",xmlGetAttr, "href"))

for(i in 1:length(links)){
        formazioni <- htmlTreeParse(links[i],useInternalNodes = T)
        
        squadracasa <- xpathSApply(formazioni,"//div[@class='team home']//ul//li[@class='top']",xmlValue)
        formazionecasa <- xpathSApply(formazioni,"//div[@class='team home']//ul//li[@class='odd' or @class='even']",xmlValue)
        
        squadrafuoricasa <- xpathSApply(formazioni,"//div[@class='team away']//ul//li[@class='top']",xmlValue)
        formazionefuoricasa <- xpathSApply(formazioni,"//div[@class='team away']//ul//li[@class='odd' or @class='even']",xmlValue)        
}


