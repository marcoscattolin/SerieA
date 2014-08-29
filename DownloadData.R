library(XML)

url <- "http://www.corrieredellosport.it/calcio/serie_a/statistiche_classifiche_seriea.shtml"
html <- htmlTreeParse(url)
links <- getHTMLLinks(url)
links <- links[grep("SerieA/statistiche",links)]
