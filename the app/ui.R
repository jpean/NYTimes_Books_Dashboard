library(shiny)
library(formattable)
library(dplyr)
library(gtools)
library(ggplot2)
library(stringr)
library(wordcloud)
library(tm)
library(rsconnect)
set.seed(1234)

nyt_data<-read.csv("NYT_Bestsellers.csv",stringsAsFactors = FALSE)

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