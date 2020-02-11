#library(rsconnect)
library(shiny)
library(lubridate)
library(leaflet)
library(ggplot2)
library(plyr)
library(stringr)
library(dplyr)
library(shinydashboard)
library(reshape2)

# Getting the first Table
{
  data = read.csv("litterati_challenge-65.csv", stringsAsFactors = FALSE)
  data$standardformat_new <- force_tzs(ymd_hms(data$litterTimestamp), tzones = "America/Chicago")
  data$lat <- as.numeric(as.character((data$lat)))
  data$lon <- as.numeric(as.character((data$lon))) 
}

# Cleaning up the Data
{
  data <- data[!(data$lat < 41),]
  data <- data[!(data$lon > -87),]
  #data<-data[!(data$lon== -87.83948 & data$lat== 41.87541),]
  #data<-data[!(data$lon == -87.83948 & data$lat == 41.87541),]
  data<-data[!(data$litterId == 3015646),]
  data<-data[!(data$litterId == 3015640),]
  data<-data[!(data$litterId == 3281473),]
  data<-data[!(data$litterId == 1872532),]
  data<-data[!(data$litterId == 	3378439),]
  data<-data[!(data$litterId == 		3027359),]
  data<-data[!(data$litterId == 3027359),]
  data<-data[!(data$litterId == 3027451),]
  data<-data[!(data$litterId == 3027379),]
  data<-data[!(data$litterId == 3419257),]
  data<-data[!(data$litterId == 3015642),]
}


# Fixing the tags
data$tags[data$tags == ""] <- "untagged"

# Fixing the empty usernames
{
  data$username[data$username == "litterati-73940"] <- "John-Doe"
  data$username[data$username == "litterati-127490"] <- "Habib"
  data$username[data$username == "litterati-126822"] <- "KarlaWithAK"
  data$username[data$username == "litterati-117766"] <- "Joe"
  data$username[data$username == "litterati-115453"] <- "Abdul"
  data$username[data$username == "litterati-119389"] <- "Zuhaib"
  data$username[data$username == "litterati-57379"] <- "Nelofer"
  data$username[data$username == "litterati-64263"] <- "Khateeb"
}

# Top 10 Pickers
{
  Top10Pickers <- plyr::count(data$username)
  Top10Pickers <- dplyr::top_n(Top10Pickers, 10)
  Top10Pickers %>% arrange(freq)
  Top10Pickers %>% arrange(desc(freq))
  
  # adding "default" in the top 10 users table
  
  frame1 <- data.frame(Top10Pickers$x)
  names(frame1) <- c("User")
  
  frame2 <- data.frame(toString("default"))
  names(frame2) <- c("User")
  
  frame3 <- rbind(frame1,frame2)
  
}                   

# Litter picked up by Date
{
  
  data$date <- stringr::str_extract(data$standardformat_new, "^.{10}")
  TableByDate <- plyr::count(data$date)
  
}

# Litter picked up by Day
{
  data$newDate <- ymd_hms(data$standardformat_new)
  data$days <- weekdays(data$newDate,abbreviate = FALSE)
  TableByDay <- plyr::count(data$days)
  TableByDay$x <- factor(TableByDay$x, levels= c("Sunday", "Monday", 
                                                 "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  TableByDay[order(TableByDay$x), ]
}

# Litter picked up by hour
{
  data$hour <- substr(data$standardformat_new, 11, 13)
  TableByHour <- plyr::count(data$hour)
}

# Litter by Tag

{
  Tags_Top <-strsplit(data$tags, ",") 
  Tags_Top <- unlist(Tags_Top)
  
  Tags_Top <- table(Tags_Top)
  Tags_Top <- as.data.frame(Tags_Top)
  Tags_Top <- Tags_Top[order(-Tags_Top$Freq),]
  Tags_Top <- dplyr::top_n(Tags_Top, 10)
  
  frame4 <- data.frame(Tags_Top$Tags_Top)
  names(frame4) <- c("Tags_Top")
  
  frame5 <- data.frame(toString("default"))
  names(frame5) <- c("Tags_Top")
  
  frame6 <- rbind(frame4,frame5)
  
}

# Month
data$month <- substr(data$standardformat_new, 6, 7)
month <- plyr::count(data$month)

frame7 <- data.frame(month$x)
names(frame7) <- c("Month")

frame8 <- data.frame(toString("default"))
names(frame8) <- c("Month")

frame9 <- rbind(frame7,frame8)





r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

  ui <- dashboardPage(
    dashboardHeader(title = "Dasboard"),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL),
        menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      
      selectInput("selectedUser", "Select the username", frame3, selected = "default"),
      selectInput("selectedTag", "Select the Tag", frame6, selected = "default"),
      selectInput("selectedMonth", "Select the Month", frame9, selected = "default")
      
      
    ),
    dashboardBody(
      # Boxes need to be put in a row (or column)
      fluidRow(
        
        box(
          title = "Total Items: ", solidHeader = TRUE,
          textOutput("totalText")),
        
        box(
          title = "About: ", solidHeader = TRUE,
          textOutput("totalText2")),
        
        
        box(
          title = "Map", solidHeader = TRUE,
          leafletOutput("mymap2")),
        
        
        box(
          
          title = "Date Histogram",  solidHeader = TRUE,
          plotOutput("plotByDate")),
        
        box(title = "Date Table", solidHeader = TRUE,
            DT::dataTableOutput('tableByDate')
        ),
        
        
        box(
          
          title = "Top 10 Tags Histogram",  solidHeader = TRUE,
          plotOutput("plotByTag")),
        
        box(
          
          title = "Weekdays Histogram",  solidHeader = TRUE,
          plotOutput("plotByDay")),
        
        box(title = "Weekdays Table", solidHeader = TRUE,
            DT::dataTableOutput('tableByDay')
        ),
        
        
        box(
          
          title = "By the Hour Histogram",  solidHeader = TRUE,
          plotOutput("plotByHour")),
        
        box(title = "Hour Table", solidHeader = TRUE,
            DT::dataTableOutput('tableByHour')
        ),
        
        box(title = "Tags Table", solidHeader = TRUE,
            DT::dataTableOutput('tableByTag')
        ),
        
        
        box(
          
          title = "Top 10 Users Histogram",  solidHeader = TRUE,
          plotOutput("Top10Histogram")),
        
        
        
        
      )
    )
  )


server <- function(input, output) {
  
  output$totalText <- renderText({
    paste("", length(data$user_id))
  })
  
  output$totalText2 <- renderText({
    paste(("Name: Imaad Sohraab"))
  })
  
  
  example <- reactive(input$selectedUser)
  
  
  
  # Map
  {
    output$mymap2 <- renderLeaflet({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
      {
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=data$lon, lat=data$lat, clusterOptions = markerClusterOptions())
      }
      else if(name != "default" && tag == "default" && mon == "default"){
        
        indDataTable <- subset(data, data$username == name)
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=indDataTable$lon, lat=indDataTable$lat, clusterOptions = markerClusterOptions())  
      }
      
      else if(name == "default" && tag != "default" && mon == "default"){
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=indDataTable$lon, lat=indDataTable$lat, clusterOptions = markerClusterOptions())  
      }
      else if(name != "default" && tag != "default" && mon == "default"){
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, indDataTable$username == name)
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=indDataTable$lon, lat=indDataTable$lat, clusterOptions = markerClusterOptions())  
      }
      
      
      # month adding:
      
      else if(name == "default" && tag == "default" && mon != "default"){
        
        # indDataTable <- subset(data, grepl(tag, data$tags))
        # indDataTable <- subset(indDataTable, indDataTable$username == name)
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=indDataTable$lon, lat=indDataTable$lat, clusterOptions = markerClusterOptions())  
      }
      
      else if(name != "default" && tag != "default" && mon != "default"){
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, indDataTable$username == name)
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=indDataTable$lon, lat=indDataTable$lat, clusterOptions = markerClusterOptions())  
      }
      
      else if(name == "default" && tag != "default" && mon != "default"){
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        # indDataTable <- subset(indDataTable, indDataTable$username == name)
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=indDataTable$lon, lat=indDataTable$lat, clusterOptions = markerClusterOptions())  
      }
      
      else if(name != "default" && tag == "default" && mon != "default"){
        
        # indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        leaflet() %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addMarkers(lng=indDataTable$lon, lat=indDataTable$lat, clusterOptions = markerClusterOptions())  
      }
      
      
    })
  }
  
  
  # Top 10 by Username Graph
  {
    output$Top10Histogram <- renderPlot({
      
      name <- input$selectedUser
      tag <- input$selectedTag
      
      if(name == "default" && tag == "default")
        
      {
        
        ggplot(data=Top10Pickers, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Username") + ylab("Number Of Items")
        
      }
    })
  }
  
  # By Date Plot
  {
    output$plotByDate <- renderPlot({
      
      name <- input$selectedUser
      
      tag <- input$selectedTag
      
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        ggplot(data=TableByDate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
      }
      else if(name != "default" && tag == "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }     }
      
      
      else if(name == "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        # ggplot(data=indTableByDate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items with Selected Tag")
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
        
        
        
      }
      
      # month
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, indDataTable$username == name)
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
        
        
        
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        # ggplot(data=indTableByDate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items with Selected Tag")
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
        
        
        
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        # ggplot(data=indTableByDate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items with Selected Tag")
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
        
        
        
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(data, grepl(tag, data$tags))
        #indDataTable <- subset(indDataTable, indDataTable$username == name)
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
        
        
        
      }
      
      
      
    })
  }
  
  
  # By Date Table
  {
    output$tableByDate <- DT::renderDataTable({
      
      name <- input$selectedUser
      
      tag <- input$selectedTag
      
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
          TableByDate      
      }
      else if(name != "default" && tag == "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
          
        }     }
      
      
      else if(name == "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        # ggplot(data=indTableByDate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items with Selected Tag")
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
        
        
        
      }
      
      # month
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, indDataTable$username == name)
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
        
        
        
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        # ggplot(data=indTableByDate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items with Selected Tag")
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
        
        
        
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        # ggplot(data=indTableByDate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items with Selected Tag")
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
        
        
        
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(data, grepl(tag, data$tags))
        #indDataTable <- subset(indDataTable, indDataTable$username == name)
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        indDataTable$date <- stringr::str_extract(indDataTable$standardformat_new, "^.{10}")
        indTableByDate <- plyr::count(indDataTable$date)
        
        
        
        {
          combDT2 <- merge(indTableByDate, TableByDate, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
        
        
        
      }
      
      
})
  }
  
  
  # By Day Table
  {
    output$tableByDay <- renderDataTable({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        
        TableByDay     
        }
      else if(name != "default" && tag == "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      else if(name == "default" && tag != "default" && mon == "default")
      {
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
        
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
      }
      
      # month
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
        
      }
      
      
      
      
    })
  }
  
  # By Hour Table
  {
    output$tableByHour <- renderDataTable({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        names(TableByHour)[names(TableByHour) == "freq.y"] <- "total"
        TableByHour
      }
      
      else if(name != "default" && tag == "default"&& mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      else if(name == "default" && tag != "default"&& mon == "default")
      {
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      
      #month
      
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }
      }
      
      
      
      
      
    })
  }
  
  # By Day Plot
  {
    output$plotByDay <- renderPlot({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        
        ggplot(data=TableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Day") + ylab("Number Of Items")
      }
      else if(name != "default" && tag == "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      else if(name == "default" && tag != "default" && mon == "default")
      {
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
        
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
      }
      
      # month
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        indDataTable$newDate <- ymd_hms(indDataTable$standardformat_new)
        indDataTable$days <- weekdays(indDataTable$newDate,abbreviate = FALSE)
        indTableByDay <- plyr::count(indDataTable$days)
        indTableByDay$x <- factor(indTableByDay$x, levels= c("Sunday", "Monday", 
                                                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        indTableByDay[order(indTableByDay$x), ]
        #ggplot(data=indTableByDay, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Date") + ylab("Number Of Items")
        
        {
          combDT2 <- merge(indTableByDay, TableByDay, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
        
      }
      
      
      
      
    })
  }
  
  
  # By Hour Plot
  {
    output$plotByHour <- renderPlot({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        ggplot(data=TableByHour,aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Hour") + ylab("Number Of Items")
      }
      
      else if(name != "default" && tag == "default"&& mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      else if(name == "default" && tag != "default"&& mon == "default")
      {
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      #month
      
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      
      
      
      
    })
  }
  
  
  # By Tag Plot
  {
    output$tableByTag <- renderDataTable({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        Tags_Top
      }
      else if(name != "default" && tag == "default" && mon == "default")
      {
        
        indDataTable <- subset(data, data$username == name)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }        
      }
      else if(name == "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, grepl(tag, data$tags))
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.y)] <- 0
          
          
          names(combDT2)[names(combDT2) == "Freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "Freq.y"] <- "total"
          combDT2
        }                    
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.y)] <- 0
          
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }                   
      }
      
      #month
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.y)] <- 0
          
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }                   
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.y)] <- 0
          
          
          names(combDT2)[names(combDT2) == "freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "freq.y"] <- "total"
          combDT2
        }                   
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.y)] <- 0
          
          
          names(combDT2)[names(combDT2) == "Freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "Freq.y"] <- "total"
          combDT2
        }                   
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.y)] <- 0
          
          
          names(combDT2)[names(combDT2) == "Freq.x"] <- "Selected"
          names(combDT2)[names(combDT2) == "Freq.y"] <- "total"
          combDT2
        }                   
      }
    })
  }
  # By Hour Plot
  {
    output$plotByHour <- renderPlot({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        ggplot(data=TableByHour,aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Hour") + ylab("Number Of Items")
      }
      
      else if(name != "default" && tag == "default"&& mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      else if(name == "default" && tag != "default"&& mon == "default")
      {
        
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      #month
      
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        indDataTable$hour <- substr(indDataTable$standardformat_new, 11, 13)
        indTableByHour <- plyr::count(indDataTable$hour)
        
        {
          combDT2 <- merge(indTableByHour, TableByHour, by=c("x","x"), all = TRUE) # NA's match
          combDT2$freq.x[is.na(combDT2$freq.x)] <- 0
          
          df <- data.frame(Weekday = (combDT2$x), TotalWeekday = c(combDT2$freq.y), SelectedWeekday = c(combDT2$freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }
      }
      
      
      
      
      
    })
  }
  
  # By Tag Plot
  {
    output$plotByTag <- renderPlot({
      name <- input$selectedUser
      tag <- input$selectedTag
      mon <- input$selectedMonth
      
      if(name == "default" && tag == "default" && mon == "default")
        
      {
        ggplot(data=Tags_Top, aes(x=Tags_Top, y=Freq)) + geom_bar(stat="identity") + xlab("Tag") + ylab("Number Of Items")
      }
      else if(name != "default" && tag == "default" && mon == "default")
      {
        
        indDataTable <- subset(data, data$username == name)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          df <- data.frame(Weekday = (combDT2$Tags_Top), TotalWeekday = c(combDT2$Freq.y), SelectedWeekday = c(combDT2$Freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }        
      }
      else if(name == "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, grepl(tag, data$tags))
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          df <- data.frame(Weekday = (combDT2$Tags_Top), TotalWeekday = c(combDT2$Freq.y), SelectedWeekday = c(combDT2$Freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }                    
      }
      else if(name != "default" && tag != "default" && mon == "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          df <- data.frame(Weekday = (combDT2$Tags_Top), TotalWeekday = c(combDT2$Freq.y), SelectedWeekday = c(combDT2$Freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }                   
      }
      
      #month
      else if(name != "default" && tag != "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          df <- data.frame(Weekday = (combDT2$Tags_Top), TotalWeekday = c(combDT2$Freq.y), SelectedWeekday = c(combDT2$Freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }                   
      }
      
      else if(name != "default" && tag == "default" && mon != "default")
      {
        indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          df <- data.frame(Weekday = (combDT2$Tags_Top), TotalWeekday = c(combDT2$Freq.y), SelectedWeekday = c(combDT2$Freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }                   
      }
      
      else if(name == "default" && tag == "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        #indDataTable <- subset(indDataTable, grepl(tag, indDataTable$tags))
        indDataTable <- subset(data, substr(data$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          df <- data.frame(Weekday = (combDT2$Tags_Top), TotalWeekday = c(combDT2$Freq.y), SelectedWeekday = c(combDT2$Freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }                   
      }
      
      else if(name == "default" && tag != "default" && mon != "default")
      {
        #indDataTable <- subset(data, data$username == name)
        indDataTable <- subset(data, grepl(tag, data$tags))
        indDataTable <- subset(indDataTable, substr(indDataTable$standardformat_new, 6, 7) == mon)
        
        ind_Tags_Top <-strsplit(indDataTable$tags, ",") 
        ind_Tags_Top <- unlist(ind_Tags_Top)  
        
        ind_Tags_Top <- table(ind_Tags_Top)
        ind_Tags_Top <- as.data.frame(ind_Tags_Top)
        ind_Tags_Top <- ind_Tags_Top[order(-ind_Tags_Top$Freq),]
        ind_Tags_Top <- dplyr::top_n(ind_Tags_Top, 10)
        
        
        
        {
          names(ind_Tags_Top)[names(ind_Tags_Top) == "ind_Tags_Top"] <- "Tags_Top"
          combDT2 <- merge(ind_Tags_Top, Tags_Top, by=c("Tags_Top","Tags_Top"), all = TRUE) # NA's match
          combDT2$Freq.x[is.na(combDT2$Freq.x)] <- 0
          combDT2$Freq.y[is.na(combDT2$Freq.x)] <- 0
          
          
          df <- data.frame(Weekday = (combDT2$Tags_Top), TotalWeekday = c(combDT2$Freq.y), SelectedWeekday = c(combDT2$Freq.x))
          data.m <- melt(df, id.vars='Weekday')
          
          ggplot(data.m, aes(Weekday, value)) + geom_bar(aes(fill = variable), 
                                                         width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
            theme(legend.position="top", legend.title = 
                    element_blank(),axis.title.x=element_blank(), 
                  axis.title.y=element_blank())
        }                   
      }
    })
  }
  
  
}

shinyApp(ui = ui, server = server)
