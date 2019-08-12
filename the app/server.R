library(shiny)
library(formattable)
library(dplyr)
library(gtools)
library(ggplot2)
library(stringr)
library(wordcloud)
library(tm)
library(rsconnect)
source("data_transform.R")
set.seed(1234)


function(input,output){
  nyt<-reactive({
    if(input$pub!="All"&input$auth!="All"){
      nyt_data%>%
        filter(Publisher%in%input$pub,
               between(Date,input$date[1],input$date[2]),
               Author%in%input$auth)
    }else if(input$pub!="All"&input$auth=="All"){
      nyt_data%>%
        filter(Publisher%in%input$pub,
               between(Date,input$date[1],input$date[2]))
    }else if(input$pub=="All"&input$auth!="All"){
      nyt_data%>%
        filter(between(Date,input$date[1],input$date[2]),
               Author%in%input$auth)
    }else{
      nyt_data%>%
        filter(between(Date,input$date[1],input$date[2]))
    }
  })
  cloudwords<-reactive({
    nyt_words<-nyt()
    words<-paste(nyt_words$Title,collapse = " ")
    docs <- Corpus(VectorSource(words))
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    d%>%filter(word!="the"&word!="and")
  })
  output$book_table<-renderTable({
    formattable(nyt()%>%
                  mutate(Price=paste0("$",round(Price,2)))%>%
                  select(Publisher,Author,Price,Title,`Age group`,`Weeks on list`))
  })
  output$line_graph<-renderPlot({
    nyt()%>%
      group_by(Date)%>%
      summarize("Average Price"=mean(Price))%>%
      ggplot(aes(x=Date,y=`Average Price`))+
      geom_line()
  })
  output$word_cloud<-renderPlot({
    wc<-cloudwords()
    wordcloud(words = wc$word, freq = wc$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(10, "Dark2"))
  })
}
