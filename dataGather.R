library(XML)
for(i in 1995:2016){
  url <- paste0("http://www.pro-football-reference.com/years/", i ,"/draft.htm")
  assign(paste0(i,"_nfldraft"), data.frame(readHTMLTable(url)[1], year = i, stringsAsFactors = FALSE))
}
require(dplyr)
l.df <- lapply(ls(pattern="nfldraft"), function(x){get(x)})
l.df <- lapply(l.df, function(x){
  x[,1] <- as.character(x[,1])
  x <- x[!(x[,1]=="Rnd"),]
  x$overall <- 1:nrow(x)
  colnames(x) <- gsub("drafts.","",colnames(x))
  x
})
df <- do.call("rbind", l.df)
colnames(df) <- gsub("drafts.","",colnames(df))
df$drafts.CarAV <- as.numeric(as.character(df$drafts.CarAV))
df <- as.data.frame(df)
for(i in 1:ncol(df)){
  if(class(df[[i]]) == "factor"){
    df[[i]] <- as.character(df[[i]])
  }
  if(grepl("[0-9]", df[[i]])==TRUE | df[[i]]==""){
    df[[i]] <- as.numeric(df[[i]])
  }
}
setwd("")
save(df, file = "/Users/Piotr/Dropbox/DraftData/data.RData")