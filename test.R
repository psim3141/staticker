if (interactive()) {
        
        ui <- fluidPage(
                downloadLink("downloadData", "Download")
        )
        
        server <- function(input, output) {
                # Our dataset
                data <- mtcars
                
                output$downloadData <- downloadHandler(
                        filename = function() {
                                paste("data-", Sys.Date(), ".csv", sep="")
                        },
                        content = function(file) {
                                write.csv(data, file)
                        }
                )
        }
        
        shinyApp(ui, server)
}


library(jsonlite)

ratings <- read.csv("ratings.csv",stringsAsFactors = F)

ratings <- head(ratings,20)




nas <- is.na(ratings$IMDB.ID)
if (sum(nas) > 0) {
        print(paste(sum(nas),"entries were removed due to unavailable IMDB IDs"))
        ratings <- ratings[!nas,]
}

doubles <- duplicated(ratings$IMDB.ID)
if (sum(doubles) > 0) {
        print(paste(sum(doubles),"entries were removed due to duplicate IMDB IDs"))
        ratings <- ratings[!doubles,]
}

nCrit <- dim(ratings)[1]
iYear <- numeric(nCrit)
iRuntime <- numeric(nCrit)
iDirect <- character(nCrit)
iWriter <- character(nCrit)
iLanguage <- character(nCrit)
iCountry <- character(nCrit)
iPoster <- character(nCrit)
iMeta <- numeric(nCrit)
iIMDB <- numeric(nCrit)
iVotes <- numeric(nCrit)
iType <- character(nCrit)

for (r in 1:nCrit){
        jr <- fromJSON(paste("http://www.omdbapi.com/?i=",ratings[r,]$IMDB.ID,"&apikey=789f5c89",sep=""))
        iYear[r] <- jr$Year
        iRuntime[r] <- as.numeric(strsplit(jr$Runtime," ")[[1]][[1]])
        iDirect[r] <- jr$Director
        iWriter[r] <- jr$Writer
        iLanguage[r] <- jr$Language
        iCountry[r] <- jr$Country
        iPoster[r] <- jr$Poster
        iMeta[r] <- jr$Metascore
        iIMDB[r] <- jr$imdbRating
        iVotes[r] <- as.numeric(gsub(",","",jr$imdbVotes))
        iType[r] <- jr$Type
}

ratings$Year <- iYear
ratings$Runtime <- iRuntime
ratings$Director <- iDirect
ratings$Writer <- iWriter
ratings$Language <- iLanguage
ratings$Country <- iCountry
ratings$Poster <- iPoster
ratings$Metacritic <- iMeta
ratings$IMDB.Rating <- iIMDB
ratings$IMDB.Votes <- iVotes
ratings$Type <- iType

write.csv(ratings,file="ratings-converted.csv")

bla <- read.csv("ratings-converted.csv",stringsAsFactors = F)
str(bla)



for (r in 1:dim(ratings)[1]){
        print(ratings[r,]$Score)
}

test = "Quentin Tarantino, Quentin Tarantino (character The Bride), Uma Thurman (character The Bride)"
directors = character()
for (i in strsplit(test,", ")[[1]]){
        print(i)
        directors <- append(directors,strsplit(i," \\(")[[1]][[1]])
}
unique(directors)

strsplit(test,",| \\(")[[1]][[1]]


testJSON$Title
names(testJSON)
head(testJSON)
str(testJSON)
strsplit(testJSON$Runtime,split = " ")
