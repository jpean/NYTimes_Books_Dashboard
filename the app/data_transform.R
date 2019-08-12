library(dplyr)
nyt_data<-read.csv("/var/host/media/removable/SD Card/NYTimes_Books_Dashboard/books_uniq_weeks.csv")

for(i in 1:ncol(nyt_data)){
  colnames(nyt_data)[i]<-gsub("_"," ",capwords(colnames(nyt_data)[i]))
}

prices<-round(runif(nrow(nyt_data),0,20),2)
ages<-sample(c("Childhood","Teen","Young Adult","Adult"),
             nrow(nyt_data),replace=TRUE,prob=c(0.1,0.2,0.4,0.3))

nyt_data<-nyt_data%>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Price=prices,
         `Age group`=ages,
         Title=str_replace_all(Title, "[^[:alnum:]]", " "))

