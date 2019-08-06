library(shiny)
library(plotly)
if (!("nlme" %in% installed.packages()[,1])) install.packages("nlme")
library(nlme)
shinyUI(fluidPage( 
        titlePanel("Staticker - getting the stats on your Criticker ratings"),
        sidebarLayout(
                sidebarPanel(
                        conditionalPanel(
                                condition = "!output.convLoaded",
                                p("Did you already convert your raw Criticker ratings?
                                  If not, do that first! Click on the Conversion tab
                                  to do that."),
                                fileInput("convIn","Converted CSV Import"),
                                uiOutput("loading")
                        ),
                        conditionalPanel(
                                condition = "output.convLoaded",
                                splitLayout(
                                        p("Data loaded."),
                                        actionButton("reload","Load new data"),
                                        cellWidths = c("40%", "60%")
                                        ),
                                br(), span(textOutput("loadMessage"), style="color:red"),br(),
                                radioButtons("plotType","Choose desired plot type",
                                             choiceNames = c("Scatter Plot","Bar Charts","Pie Charts"),
                                             choiceValues = c("xvy","bar","pie"),
                                             selected = "")
                                ),
                        conditionalPanel(
                                condition = "output.convLoaded & input.plotType == 'xvy'",
                                selectInput("ySelect","Y-Axis",
                                            c("Criticker rating","IMDB rating","Metacritic",
                                              "Runtime","Year","IMDB votes")),
                                selectInput("xSelect","X-Axis",
                                            c("Criticker rating","IMDB rating","Metacritic",
                                              "Runtime","Year","IMDB votes"),
                                            selected = "IMDB rating"),
                                selectInput("cSelect","Color Axis",
                                            c("-","Criticker rating","IMDB rating","Metacritic",
                                              "Runtime","Year","IMDB votes")),
                                checkboxInput("displayFit","Display best linear fit",value = T),
                                checkboxInput("logScale","Log scale on color axis",value = F)
                        ),
                        conditionalPanel(
                                condition = "output.convLoaded & input.plotType == 'bar'",
                                selectInput("barYSelect","Y-Axis",
                                            choices = c("No. of movies","Average rating")),
                                selectInput("barSelect","Choose groups on X-Axis",
                                             choices = c("Genre","Decade/Year","Runtime","Rating")),
                                conditionalPanel(
                                        condition = "input.barSelect != 'Genre'",
                                        radioButtons("barBins","Number of bins",
                                                     choiceNames = c("Default","Maximum","Manual"),
                                                     choiceValues = c("def","max","man"),
                                                     selected = "def")
                                        ),
                                conditionalPanel(
                                        condition = "input.barSelect == 'Genre'",
                                        checkboxInput("barAlpha","Alphabetically",value = T)
                                ),
                                conditionalPanel(
                                        condition = "input.barBins == 'man' & input.barSelect != 'Genre'",
                                        sliderInput("barBinW","Bin width in %",min=1,max=50,value=10,step=1)
                                )
                        ),
                        conditionalPanel(
                                condition = "output.convLoaded & input.plotType == 'pie'",
                                radioButtons("pieSelect","Choose pie chart",
                                             choiceNames = c("Genre","Country of Origin","Language","Decade"),
                                             choiceValues = c("Genre","Country","Language","Decade")),
                                splitLayout(
                                        p(br(),strong("Group slices below")),
                                        numericInput("pieGroupThreshold", "", value = 1,
                                                     min=0,max=100,step=0.1,width="100px"),
                                        cellWidths = c("70%", "30%")
                                ),
                                splitLayout(
                                checkboxInput("pieLabels","Labels",value=T),
                                checkboxInput("piePercent","Percent",value=T),
                                checkboxInput("pieLegend","Legend",value=T)
                                )
                        )
                        ),
                mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Output", br(),
                                             conditionalPanel(
                                                     condition = "input.plotType == 'xvy'",
                                                     plotlyOutput("plotxy")
                                             ),
                                             conditionalPanel(
                                                     condition = "input.plotType == 'pie'",
                                                     plotlyOutput("plotpie")
                                             ),
                                             conditionalPanel(
                                                     condition = "input.plotType == 'bar'",
                                                     plotlyOutput("plotbar")
                                             )
                                    ),
                                    tabPanel("Statistics",br(),
                                             verbatimTextOutput("stats"),
                                             br(),verbatimTextOutput("favs")
                                    ),
                                    tabPanel("Conversion", br(),
                                             fileInput("fIn","Raw Criticker CSV Import"),
                                             uiOutput("conversion"),
                                             textOutput("convMessage"),
                                             uiOutput("dlconverted"),
                                             br(),p("This might take a few minutes, 
                                                    depending on how many ratings you have.")
                                    ),
                                    tabPanel("Documentation", br(), 
                                             h1("Documentation"),
                                             p("Under construction. Basic steps: 1) Log into your Criticker account.
                                               2) Export your ratings to CSV format with this ",
                                               a(href="https://www.criticker.com/resource/ratings/conv.php?type=csv","Link"),
                                               ". 3) Use the Conversion tab here to read more data on all your movies.
                                               4) Save that converted CSV file for later use.
                                               5) Open the file in the left sidebar.
                                               6) Have fun!"),
                                             br(),p("Written by Patrick Simon, 
                                                    August 2019"))
                        )
                )
        ),
        hr(),
        p("Copyright 2019, Patrick Simon. Licensed under the GNU General Public License v3.0. Source code available at ",
          a(href="https://github.com/psim3141/staticker","GitHub"),".") #,
        #p(a(href="https://www.themoviedb.org",
        #img(src = "https://www.themoviedb.org/assets/2/v4/logos/408x161-powered-by-rectangle-blue-10d3d41d2a0af9ebcb85f7fb62ffb6671c15ae8ea9bc82a2c6941f223143409e.png", width = "100px")),
        #"This product uses the TMDb API but is not endorsed or certified by TMDb.")
)
)