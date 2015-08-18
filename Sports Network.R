#Gathering data on trades between major league baseball teams
#hypothesis - some teams may trade more than other teams, cliques or local structure may be observed
#stratified by team value or by geography

#Gather data from http://www.spotrac.com/transactions/more/index'/1/
#1 indicates MLB, 2 for NBA, 3 for NFL, 4 for MLS


library(XML)
library(igraph)

directory<-"/Users/josephelliott/Documents/Social Network Analysis/Assignments/Project"
cleanFilename<-'cleanTeams2013.rda'
rawFilename<-'tradeDataRaw2013.rda'
setwd(directory)
getwd()

loadData<-function() {
  load(cleanFilename)
  if(!exists('cleanTeams')){
    load(rawFilename)
    if(!exists('tradeData')){
      downloadData()
      loadData()      
    }
    else{
      cleanDataSet(tradeData)
      loadData()
    }
  }
  else {
    cleanTeams
  }
}


downloadData<-function() {
  #first gather the data from the pages and put it into a table
  #Action (Traded), team1, team2, date
  maxPages<-53
  n<-2
  tradeData<-data.frame()
  dates<-c()
  tradeFrame<-data.frame()
  dateDownloaded <- date()
  dateDownloaded
  
  while(n <= maxPages) {
    html<-htmlTreeParse(paste0("http://www.spotrac.com/transactions/more/",as.character(n),"/1/"),useInternalNodes=TRUE)
    scrapedDataEven<-xpathSApply(html, "//tr/td[@class='transaction  even']/span[@class='data']", xmlValue)
    scrapedDateEven<-xpathSApply(html, "//tr/td[@class='date  even']", xmlValue)
    scrapedDataOdd<-xpathSApply(html, "//tr/td[@class='transaction  ']/span[@class='data']", xmlValue)
    scrapedDateOdd<-xpathSApply(html, "//tr/td[@class='date  ']", xmlValue)
    evenFrame<-data.frame(date=scrapedDateEven,data=as.character(scrapedDataEven))
    oddFrame<-data.frame(date=scrapedDateOdd,data=as.character(scrapedDataOdd))
    fullFrame<-rbind(evenFrame,oddFrame)
    
    
    tradedStrings<-grep("Traded ",fullFrame$data)
    tradeDataTemp<-fullFrame[tradedStrings,]
    #create a master list of all the trade data. 
    tradeData<-rbind(tradeData,tradeDataTemp)
    n<-n+1
  }
  print(tradeData)
  print(str(tradeData))
  save(tradeData,dateDownloaded,file="tradeDataRaw2013.rda")
}

cleanDataSet<-function(tradeData){
  #use regex to break down the trade data into team1 and team2
  #use unlist and sapply to eliminate missing data points, rather than NA.
  team1<-unlist(sapply(regmatches(as.character(tradeData$data),regexec("Traded to (.*? \\([A-Z]{1,5}\\)) from",as.character(tradeData$data))),function(x) x[2]))
  team2<-unlist(sapply(regmatches(as.character(tradeData$data),regexec(".* from (.*?\\))",as.character(tradeData$data))),function(x) x[2]))
  
  
  #Build data frame of just teams
  teams<-data.frame(tradedTo=team1,tradedFrom=team2,date=as.Date(tradeData$date, "%b %d, %Y"))
  teams
  
  #get rid of rows with NA
  cleanTeams<-na.omit(teams)
  
  #for some reason, some teams have two abbreviation types or alternative capitalization:
  cleanTeamName<-function(x) {
    if(x == "Baltimore (BALT)") {
      as.factor("Baltimore (BAL)")
    }else if(x=="Tampa Bay (TAM)"){
      as.factor("Tampa Bay (TB)")
    }else if(x=="Florida (FLA)"){ 
      as.factor("Miami (MIA)")
    }else if(x=="Chicago (CHW)"){
      as.factor("Chicago (CWS)")
    }else if(x=="PIttsburgh (PIT)"){
      as.factor("Pittsburgh (PIT)")
    }else{x}
  }
  #create cleanTeams data frame
  cleanTeams<-data.frame(tradedTo=factor(sapply(cleanTeams$tradedTo,cleanTeamName)),tradedFrom=factor(sapply(cleanTeams$tradedFrom,cleanTeamName)), date=cleanTeams$date)
  
  #check to make sure we have 30 teams
  print(sort(unique(c(levels(cleanTeams[1,1]),levels(cleanTeams[1,2])))))
  

  
  #TODO: filter based on date and team names. 
  #cleanTeams<-apply(cleanTeams,c(1),function(x) if(x$date %in% cleanTeams$date){x})

  
  #save the cleaned data set. 
  save(cleanTeams,file="cleanTeams2013.rda")
}

#cleanTeams is the resulting edges...

exportGMLFile<-function(cleanTeams){

  #use edgelist to create graph.  Initialize weights to zero.
  g<-graph.edgelist(as.matrix(cleanTeams[,1:2]),directed=FALSE)
  E(g)$weight <- 1 
  
  #simplify the graph to eliminate duplicates and sum edge weights
  sg<-simplify(g)
  
  #node names
  print(V(sg)$name)
  
  #edges
  print(cbind( get.edgelist(sg) , round( E(sg)$weight, 3 )))
  
  #turn into a gml file.
  write.graph(sg,'mlb2013.gml',format='gml')
}