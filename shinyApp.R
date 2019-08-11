library(shiny)
library(formattable)
library(dplyr)
library(gtools)
library(ggplot2)
library(stringr)
library(wordcloud)
library(tm)
set.seed(1234)

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

ui<-fluidPage(
  h1("New York Times Bestsellers 2011-2018"),
  em("Prices are Age Group randomized"),
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId="pub",
            label="Publisher",
            choices=c("All",as.character(unique(nyt_data$Publisher))),
            selected="All",
            multiple = FALSE,
            selectize = FALSE,
            size=5
            ),
          sliderInput(
            inputId="date",
            label="Dates",
            min=as.Date("2011-01-01",format="%Y-%m-%d"),
            max=as.Date("2018-12-31",format="%Y-%m-%d"),
            value=c(as.Date("2011-01-01",format="%Y-%m-%d"),
                    as.Date("2018-12-31",format="%Y-%m-%d")),
            timeFormat="%Y-%m-%d"
          ),
          selectInput(
            inputId="auth",
            label="Author",
            choices=c("All",as.character(unique(nyt_data$Author))),
            selected="All",
            multiple = FALSE,
            selectize = FALSE,
            size=5
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Data Table",
                tableOutput("book_table")
            ),
            tabPanel(
              "Average Book Price Over Time",
                plotOutput("line_graph")
            ),
            tabPanel(
              "Word Cloud for Titles",
              plotOutput("word_cloud")
            )
          )
        )
    )
)

server<-function(input,output){
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

shinyApp(ui,server)
