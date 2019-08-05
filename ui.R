library(shiny)
library(plotly)
shinyUI(fluidPage( 
        titlePanel("Staticker - getting the stats on your Criticker ratings"),
        sidebarLayout(
                sidebarPanel(
                        p("Did you already convert your raw Criticker ratings?
                          If not, do that first!"),
                        fileInput("convIn","Converted CSV Import"),
                        uiOutput("loading"),
                        conditionalPanel(
                                condition = "output.convLoaded",
                                br(), span(textOutput("loadMessage"), style="color:red"),
                                p("Data has been loaded."),br(),
                                radioButtons("plotType","Choose desired plot type",
                                             choiceNames = c("Y vs. X","By Decade"),
                                             choiceValues = c("xvy","decade"),
                                             selected = "")
                                ),
                        conditionalPanel(
                                condition = "input.plotType == 'xvy'",
                                selectInput("ySelect","Y-Axis",
                                            c("Criticker rating","IMDB rating","Metacritic rating",
                                              "Runtime","Year","IMDB votes")),
                                selectInput("xSelect","X-Axis",
                                            c("Criticker rating","IMDB rating","Metacritic rating",
                                              "Runtime","Year","IMDB votes"),
                                            selected = "IMDB rating"),
                                selectInput("cSelect","Color Axis",
                                            c("-","Criticker rating","IMDB rating","Metacritic rating",
                                              "Runtime","Year","IMDB votes")),
                                checkboxInput("displayFit","Display best linear fit",value = T),
                                checkboxInput("logScale","Log scale on color axis",value = F)
                        )
                        ),
                mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Output", br(),
                                             conditionalPanel(
                                                     condition = "input.plotType == 'xvy'",
                                                     plotlyOutput("plotxy")
                                             )
                                    ),
                                    tabPanel("Statistics",br(),
                                             verbatimTextOutput("stats"),
                                             br(),verbatimTextOutput("favs")
                                    ),
                                    tabPanel("Conversion", br(),
                                             fileInput("fIn","Converted CSV Import"),
                                             uiOutput("conversion"),
                                             textOutput("convMessage"),
                                             uiOutput("dlconverted")
                                    ),
                                    tabPanel("Documentation", br(), 
                                             h1("Documentation"),
                                             p("This app lets you... Enjoy!"),
                                             br(),p("Written by Patrick Simon, 
                                                    August 2019"))
                        )
                )
        )
))