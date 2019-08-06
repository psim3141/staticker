library(shiny)
library(plotly)
if (!("nlme" %in% installed.packages()[,1])) install.packages("nlme")
library(nlme)
library(tidyr)
library(RColorBrewer)
library(dplyr)
library(jsonlite)

shinyServer(function(input, output) {
    convData <- reactiveVal(NULL)
    convMessage1 <- reactiveVal(NULL)
    convMessage2 <- reactiveVal(NULL)
    convMessage3 <- reactiveVal(NULL)
    
    observeEvent(input$convert, {
            
            ratings <- read.csv(input$fIn$datapath,stringsAsFactors = F)
            
            nas <- is.na(ratings$IMDB.ID)
            if (sum(nas) > 0) {
                    print(ratings[nas,]$Title)
                    convMessage1(paste(sum(nas),"removed due to unavailable IMDB ID. "))
                    ratings <- ratings[!nas,]
            }
            
            doubles <- duplicated(ratings$IMDB.ID)
            if (sum(doubles) > 0) {
                    print(ratings[doubles,]$Title)
                    convMessage2(paste(sum(doubles),"removed due to duplicate IMDB ID. "))
                    ratings <- ratings[!doubles,]
            }
            
            nCrit <- dim(ratings)[1]
            iYear <- numeric(nCrit)
            iRuntime <- numeric(nCrit)
            iGenre <- character(nCrit)
            iDirect <- character(nCrit)
            iWriter <- character(nCrit)
            iLanguage <- character(nCrit)
            iCountry <- character(nCrit)
            iPoster <- character(nCrit)
            iMeta <- numeric(nCrit)
            iIMDB <- numeric(nCrit)
            iVotes <- numeric(nCrit)
            iType <- character(nCrit)
            
            k <- read.csv("key.txt",header=F)
            
            withProgress(message = 'Loading OMDB data', value = 0, {
                    for (r in 1:nCrit) {
                            
                            jr <- try(fromJSON(paste("http://www.omdbapi.com/?i=",ratings[r,]$IMDB.ID,"&apikey=",k[[1]],sep="")),silent=T)
                            if(class(jr) == "try-error" | length(jr)==2) {
                                    #print("wat")
                                    #print(ratings[r,2])
                                    iYear[r] <- iRuntime[r] <-iDirect[r] <-iLanguage[r] <-
                                            iCountry[r] <- iPoster[r] <-  iMeta[r] <-  iIMDB[r] <- 
                                            iVotes[r] <- iType[r] <- iGenre[r] <- NA
                            } else{
                                    iYear[r] <- as.numeric(strsplit(jr$Year,"–")[[1]][[1]])
                                    iRuntime[r] <- as.numeric(strsplit(jr$Runtime," ")[[1]][[1]])
                                    iGenre[r] <- jr$Genre
                                    iDirect[r] <- jr$Director
                                    iWriter[r] <- jr$Writer
                                    iLanguage[r] <- jr$Language
                                    iCountry[r] <- jr$Country
                                    iPoster[r] <- jr$Poster
                                    iMeta[r] <- as.numeric(jr$Metascore)
                                    iIMDB[r] <- as.numeric(jr$imdbRating)
                                    iVotes[r] <- as.numeric(gsub(",","",jr$imdbVotes))
                                    iType[r] <- jr$Type
                                    
                                    #print(jr$Title)
                            }
                            
                            # Increment the progress bar, and update the detail text.
                            incProgress(1/nCrit, detail = paste("Rating", r))
                    }
            })
            
            ratings$Year <- iYear
            ratings$Runtime <- iRuntime
            ratings$Genre <- iGenre
            ratings$Director <- iDirect
            ratings$Writer <- iWriter
            ratings$Language <- iLanguage
            ratings$Country <- iCountry
            ratings$Poster <- iPoster
            ratings$Metacritic <- iMeta
            ratings$IMDB.Rating <- iIMDB
            ratings$IMDB.Votes <- iVotes
            ratings$Type <- iType
            
            ratings <- separate(ratings,Genre,c("G1","G2","G3"),extra="drop",fill="right",sep=", | \\([^()]*\\), | \\([^()]*\\)",remove=F)
            ratings <- separate(ratings,Director,c("D1","D2","D3"),extra="drop",fill="right",sep=", | \\([^()]*\\), | \\([^()]*\\) |\\([^()]*\\)",remove=F)
            ratings <- separate(ratings,Writer,c("W1","W2","W3"),extra="drop",fill="right",sep=", | \\([^()]*\\), | \\([^()]*\\)",remove=F)
            ratings <- separate(ratings,Language,c("L1","L2","L3"),extra="drop",fill="right",sep=", | \\([^()]*\\), | \\([^()]*\\)",remove=F)
            ratings <- separate(ratings,Country,c("C1","C2","C3"),extra="drop",fill="right",sep=", | \\([^()]*\\), | \\([^()]*\\)",remove=F)
            
            
            ratings$W3[!is.na(ratings$W3) & (ratings$W3 == "" | ratings$W3 == ratings$W2 | ratings$W3 == ratings$W1)] <- NA
            ratings$W2[!is.na(ratings$W2) & (ratings$W2 == "" | ratings$W2 == ratings$W1)] <- NA
            
            ratings$D3[!is.na(ratings$D3) & (ratings$D3 == "" | ratings$D3 == ratings$D2 | ratings$D3 == ratings$D1)] <- NA
            ratings$D2[!is.na(ratings$D2) & (ratings$D2 == "" | ratings$D2 == ratings$D1)] <- NA
            
            ratings$L3[!is.na(ratings$L3) & ratings$L3 == ""] <- NA
            ratings$L2[!is.na(ratings$L3) & ratings$L2 == ""] <- NA
            
            
            nas2 <- is.na(ratings$IMDB.Rating)
            if (sum(nas2) > 0) {
                    print(ratings[nas2,]$Title)
                    convMessage3(paste(sum(nas2),"removed due to unavailable OMDB data. "))
                    ratings <- ratings[!nas2,]
            }
            
            convData(ratings)
    })
    
    output$downloadConverted <- downloadHandler(
            filename = function() {
                    "ratings-converted.csv"
            },
            content = function(con) {
                    write.csv(convData(),con,row.names=F)
            }
            )
    
    
    output$conversion <- renderUI({
       if(!is.null(input$fIn)) {
         actionButton("convert","Convert ratings")
       }
    })
    
    output$dlconverted <- renderUI({
            if(!is.null(convData())) {
                    downloadButton("downloadConverted","Download converted ratings")
            }
    })
    
    output$convMessage <- renderText({
            paste(convMessage1(),convMessage2(),convMessage3())
    })
    
    
    
    
    
    
    
    
    
    
    
    loadData <- reactiveVal(NULL)
    loadMessage <- reactiveVal(NULL)
    favDirector <- reactiveVal("")
    
    observeEvent(input$ldata, {
            ratings <- read.csv(input$convIn$datapath,stringsAsFactors = F,na.strings = c("N/A","NA"))
            loadData(ratings)
    })
    
    observeEvent(input$reload, {
        loadData(NULL)
    })
    
    output$loading <- renderUI({
            if(!is.null(input$convIn)) {
                    actionButton("ldata","Load ratings")
            }
    })
    
    
    output$loadMessage <- renderText({
            loadMessage()
    })
    
    getData <- reactive({
            if(is.null(loadData())) 
            {return(NULL)}
            else {
                    if (dim(loadData())[2] != 32) {
                    loadMessage("Data format appears to be wrong.
                                Proceed with caution.")
                    } else loadMessage("")
                    return(1)
                    }
    })
    
    output$convLoaded <- reactive({
            return(!is.null(getData()))
    })
    outputOptions(output, 'convLoaded', suspendWhenHidden=FALSE)
    
    output$stats <- renderText({
            if(is.null(loadData())) return("")
            
            maxRun <- which.max(loadData()$Runtime)
            minRun <- which.min(loadData()$Runtime)
            
            paste("Number of ratings: ",dim(loadData())[1],"\n",
                  "\n",
                  "Average rating (Criticker): ",round(mean(loadData()$Score,na.rm=T),1),"\n",
                  "Average rating (IMDB): ",round(mean(loadData()$IMDB.Rating,na.rm = T),2),"\n",
                  "Average rating (Metacritic): ",round(mean(loadData()$Metacritic,na.rm=T),1),"\n",
                  "\n",
                  "Average runtime: ",round(mean(loadData()$Runtime,na.rm=T),0)," min.\n",
                  "Longest film: ",loadData()[maxRun,]$Film.Name," (",loadData()[maxRun,]$Runtime," min.)\n",
                  "Shortest film: ",loadData()[minRun,]$Film.Name," (",loadData()[minRun,]$Runtime," min.)\n",
                  sep="")
    })
    
    output$favs <- renderText({
            if(is.null(loadData())) return("")
            
            if (1 < 0) {
            alldirectors <- character()
            allwriters <- character()
            for (i in 1:dim(loadData())[1]) {
                    directors <- character()
                    writers <- character()
                    irow <- loadData()[i,]
                    for (d in strsplit(irow$Director,", ")[[1]]){
                            directors <- append(directors,strsplit(d," \\(")[[1]][[1]])
                    }
                    for (w in strsplit(irow$Writer,", ")[[1]]){
                            writers <- append(writers,strsplit(w," \\(")[[1]][[1]])
                    }
                    for (d in unique(directors)){
                            alldirectors <- append(alldirectors,d)
                    }
                    for (w in unique(writers)){
                            allwriters <- append(allwriters,w)
                    }
            }
            }
            alldirectors <- append(append(append(character(),loadData()$D1),loadData()$D2),loadData()$D3)
            allwriters <- append(append(append(character(),loadData()$W1),loadData()$W2),loadData()$W3)
            allgenres <- append(append(append(character(),loadData()$G1),loadData()$G2),loadData()$G3)
            alllanguages <- append(append(append(character(),loadData()$L1),loadData()$L2),loadData()$L3)
            allcountries <- append(append(append(character(),loadData()$C1),loadData()$C2),loadData()$C3)
            
            top5d <- head(sort(table(alldirectors,useNA ="no"),decreasing = TRUE),5)
            top5w <- head(sort(table(allwriters,useNA = "no"),decreasing = TRUE),5)
            top5g <- head(sort(table(allgenres,useNA = "no"),decreasing = TRUE),5)
            top5l <- head(sort(table(alllanguages,useNA = "no"),decreasing = TRUE),5)
            top5c <- head(sort(table(allcountries,useNA = "no"),decreasing = TRUE),5)
            fav <- "Favorite directors:\n"
            for (i in 1:5){
                    fav <- paste(fav,names(top5d)[i]," - ",top5d[i]," films\n",sep="")
            }
            fav <- paste(fav,"\n","Favorite writers:\n",sep="")
            for (i in 1:5){
                    fav <- paste(fav,names(top5w)[i]," - ",top5w[i]," films\n",sep="")
            }
            fav <- paste(fav,"\n","Favorite genres:\n",sep="")
            for (i in 1:5){
                fav <- paste(fav,names(top5g)[i]," - ",top5g[i]," films\n",sep="")
            }
            fav <- paste(fav,"\n","Top 5 countries:\n",sep="")
            for (i in 1:5){
                fav <- paste(fav,names(top5c)[i]," - ",top5c[i]," films\n",sep="")
            }
            fav

    })
    
    
    output$plotbar <- renderPlotly({
            if(is.null(loadData())) return(NULL)
        
            if(input$barSelect == "Genre"){
                data <- append(append(append(character(),loadData()$G1),loadData()$G2),loadData()$G3)
                data <- na.omit(data.frame(cuts=data,ratings=rep(loadData()$Score,3)))
                
                xa <- list(title = "Genre",categoryorder="category ascending")
            }      
            
            if(input$barSelect == "Decade/Year"){
                xa <- list(title = "Year")
                if(input$barBins == "def") {
                    bbreaks <- c(1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2020,2030)
                    blabels <- c("1900s","1910s","1920s","1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s","2020s")
                    xa <- list(title = "Decade")
                } else if (input$barBins == "max") {
                    drange <- range(loadData()$Year,na.rm = T)
                    bbreaks <- drange[2] - drange[1] + 1
                    blabels <- as.character(drange[1]:drange[2])
                } else {
                    drange <- range(loadData()$Year,na.rm = T)
                    bbreaks <- min(round(100/input$barBinW),drange[2]-drange[1]+1)
                    bwidth <- (drange[2]-drange[1])/bbreaks
                    blabels <- character()
                    for (i in 1:bbreaks) {
                        blabels <- append(blabels,paste(drange[1]+ceiling((i-1)*bwidth),"-",floor(drange[1]+i*bwidth)))
                    }
                }
                cuts <- cut(loadData()$Year,breaks=bbreaks,labels=blabels,right=F)
                data <- na.omit(data.frame(data=loadData()$Year,ratings=loadData()$Score,cuts=cuts))
                
            } 
        
            
        
            if(input$barSelect == "Runtime"){
                if(input$barBins == "def") {
                    bbreaks <- c(0,60,75,90,105,120,135,150,165,180,9999)
                    blabels <- c("< 60","< 75","< 90","< 105","< 120","< 135","< 150","< 165","< 180","180+")
                } else if (input$barBins == "max") {
                    drange <- range(loadData()$Runtime,na.rm = T)
                    bbreaks <- drange[2] - drange[1] + 1
                    blabels <- as.character(drange[1]:drange[2])
                } else {
                    drange <- range(loadData()$Runtime,na.rm = T)
                    bbreaks <- min(round(100/input$barBinW),drange[2]-drange[1]+1)
                    bwidth <- (drange[2]-drange[1])/bbreaks
                    blabels <- character()
                    for (i in 1:bbreaks) {
                        blabels <- append(blabels,paste(drange[1]+ceiling((i-1)*bwidth),"-",floor(drange[1]+i*bwidth)))
                    }
                }
                cuts <- cut(loadData()$Runtime,breaks=bbreaks,labels=blabels,right=F)
                data <- na.omit(data.frame(data=loadData()$Runtime,ratings=loadData()$Score,cuts=cuts))
                xa <- list(title = "Runtime (min)")
            } 
        
            if(input$barSelect == "Rating"){
                if(input$barBins == "def") {
                    bbreaks <- c(0,10,20,30,40,50,60,70,80,90,101)
                    blabels <- c("< 10","< 20","< 30","< 40","< 50","< 60","< 70","< 80","< 90","90+")
                } else if (input$barBins == "max") {
                    drange <- range(loadData()$Score,na.rm = T)
                    bbreaks <- drange[2] - drange[1] + 1
                    blabels <- as.character(drange[1]:drange[2])
                } else {
                    drange <- range(loadData()$Score,na.rm = T)
                    bbreaks <- min(round(100/input$barBinW),drange[2]-drange[1]+1)
                    bwidth <- (drange[2]-drange[1])/bbreaks
                    blabels <- character()
                    for (i in 1:bbreaks) {
                        blabels <- append(blabels,paste(drange[1]+ceiling((i-1)*(bwidth)),"-",floor(drange[1]+i*(bwidth))))
                    }
                }
                cuts <- cut(loadData()$Score,breaks=bbreaks,labels=blabels,right=F)
                data <- na.omit(data.frame(data=loadData()$Score,ratings=loadData()$Score,cuts=cuts))
                xa <- list(title = "Criticker rating")
            }
        
            ya <- list(title = input$barYSelect)
            
            
            if (input$barYSelect == "No. of movies") {
                if((!input$barAlpha) & input$barSelect == "Genre") {
                    xa <- list(title = "Genre",categoryorder="array",categoryarray=names(sort(table(data$cuts),decreasing = T)))
                }
                p <- plot_ly(data, x = ~cuts, type = 'histogram') %>%
                    layout( title = paste(input$barYSelect,", grouped by ",input$barSelect,sep=""),
                        xaxis = xa, yaxis = ya)
            } else {
                gdata <- data %>% group_by(cuts) %>% summarise_all(mean)
                if((!input$barAlpha) & input$barSelect == "Genre") {
                    gdata <- gdata[sort.list(gdata$ratings,decreasing = T),]

                    xa <- list(title = "Genre",categoryorder="array",categoryarray=gdata$cuts)
                }
                p <- plot_ly(gdata,x=~cuts,y=~ratings,type="bar") %>%
                    layout( title = paste(input$barYSelect,", grouped by ",input$barSelect,sep=""),
                            xaxis = xa, yaxis = ya)
            }
            
            p
    })
    
    
    output$plotpie <- renderPlotly({
            if(is.null(loadData())) return(NULL)
            
            if(input$pieSelect == "Genre") data <- append(append(append(character(),loadData()$G1),loadData()$G2),loadData()$G3)
            if(input$pieSelect == "Country") data <- append(append(append(character(),loadData()$C1),loadData()$C2),loadData()$C3)
            if(input$pieSelect == "Language") data <- append(append(append(character(),loadData()$L1),loadData()$L2),loadData()$L3)
            if(input$pieSelect == "Decade"){
                            data <- cut(loadData()$Year,breaks=c(1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2020,2030),
                            labels=c("1900s","1910s","1920s","1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s","2020s"),
                            right=F)
            }
            
            data <- na.omit(data.frame(data))
            
            pdata <- as.data.frame(sort(table(data),decreasing = T))
            
            other <- sum(pdata[pdata$Freq / sum(pdata$Freq) <= input$pieGroupThreshold/100,2])
            pdata <- pdata[!(pdata$Freq / sum(pdata$Freq) <= input$pieGroupThreshold/100),]
            
            if(input$pieGroupThreshold > 0) pdata <- rbind(pdata,data.frame(data="Other",Freq=other))
            tinfo <- if(input$pieLabels){if(input$piePercent){"label+percent"}else{"label"}}else 
                if(input$piePercent){"percent"}else{"text"}
            
            p <- plot_ly(pdata, labels = ~data, values = ~Freq, type = 'pie',sort=F,textinfo = tinfo,
                         showlegend = input$pieLegend) %>%
                layout(title = paste('Movie collection, broken down by',input$pieSelect),
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            p
                           
                    
    })
    
    outputOptions(output, 'plotpie', suspendWhenHidden=TRUE)
    
    output$plotxy <- renderPlotly({
        if(is.null(loadData())) return(NULL)
        
        if(input$xSelect == "Criticker rating") xdata <- loadData()$Score
        if(input$xSelect == "IMDB rating") xdata <- loadData()$IMDB.Rating
        if(input$xSelect == "Metacritic") xdata <- loadData()$Metacritic
        if(input$xSelect == "Runtime") xdata <- loadData()$Runtime
        if(input$xSelect == "Year") xdata <- loadData()$Year
        if(input$xSelect == "IMDB votes") xdata <- loadData()$IMDB.Votes
        
        if(input$ySelect == "Criticker rating") ydata <- loadData()$Score
        if(input$ySelect == "IMDB rating") ydata <- loadData()$IMDB.Rating
        if(input$ySelect == "Metacritic") ydata <- loadData()$Metacritic
        if(input$ySelect == "Runtime") ydata <- loadData()$Runtime
        if(input$ySelect == "Year") ydata <- loadData()$Year
        if(input$ySelect == "IMDB votes") ydata <- loadData()$IMDB.Votes
        
        if(input$cSelect == "-") cdata <- rep(0,length(loadData()$Score))
        if(input$cSelect == "Criticker rating") cdata <- loadData()$Score
        if(input$cSelect == "IMDB rating") cdata <- loadData()$IMDB.Rating
        if(input$cSelect == "Metacritic") cdata <- loadData()$Metacritic
        if(input$cSelect == "Runtime") cdata <- loadData()$Runtime
        if(input$cSelect == "Year") cdata <- loadData()$Year
        if(input$cSelect == "IMDB votes") cdata <- loadData()$IMDB.Votes
        
        xa <- list(title = input$xSelect)
        ya <- list(title = input$ySelect)
        
        pdata <- na.omit(data.frame(x=xdata,y=ydata,c=cdata,n=loadData()$Film.Name))
        groupoptions <- NULL
        
        hovertext <- paste('<br>%{xaxis.title.text}: %{x}','<br>%{yaxis.title.text}: %{y}')
        
        if(!(input$cSelect == "-")) {
            
            hovertext <- paste(hovertext,"<br>",input$cSelect,": %{marker.color}<br>",sep="")
            
            if(!input$logScale) {
                markeroptions <- list(color=pdata$c,
                                      colorbar=list(title=input$cSelect),
                                      colorscale='Viridis')
            } else {
                pdata$c <- log(pdata$c)
                markeroptions <- list(color=pdata$c,
                                      colorbar=list(title=paste("log(",input$cSelect,")",sep="")),
                                      colorscale='Viridis')
                
            }
        } else markeroptions <- list()
        
        print(markeroptions)
        print(head(pdata))
        
        p <- plot_ly(pdata,x=~x,y=~y,type="scatter",mode="markers",
                     marker=markeroptions,text=~substr(n,1,20),hovertemplate = paste('<b>%{text}</b>',
                                                                        hovertext,
                                                                        '<extra></extra>'
                                                                        )) %>%
            layout( title = paste(input$ySelect,"vs.",input$xSelect),
                    xaxis = xa, yaxis = ya)
        
        if(input$displayFit) {
            p <- p %>% add_lines(y = ~fitted(lm(y ~ x, pdata)),
                                 line = list(color = '#000000'),
                                 marker = NULL, hovertemplate = paste(hovertext,
                                                                      '<extra></extra>'
                                 ),
                                 name = "Linear fit", showlegend = F)
        }
        
        p
        
        
    })
    
    outputOptions(output, 'plotxy', suspendWhenHidden=TRUE)
})