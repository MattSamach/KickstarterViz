library(shiny)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggrepel)
library(DT)
library(leaflet)
library(lubridate)

# Read data
kickstarter <- read.csv("./ks-projects-201801.csv")
kickstarter$ID <- as.character(kickstarter$ID)
kickstarter$name <- as.character(kickstarter$name)
kickstarter$deadline <- as.Date(kickstarter$deadline, "%m/%d/%Y")
kickstarter$launched <- as.Date(kickstarter$launched, "%m/%d/%Y")
kickstarter[kickstarter$country == "N,0\"","country"] <- NA 
kickstarter$launched <- if_else(kickstarter$launched < as.Date("2000-01-01"), 
                                as.Date(NA), 
                                kickstarter$launched)
kickstarter <-  mutate(kickstarter, 
                       successful = ifelse(state %in% c("successful", "live"), "yes", 
                                           ifelse(state %in% c("failed", "canceled"), "no", NA)))
countries <- read.csv("./countries.csv")

# Define UI for application 
ui <- fluidPage(
  titlePanel("Kickstarter Projects Analysis"),
  
  sidebarLayout(
    sidebarPanel(
                 radioButtons("success",
                              "Results by total projects, success rate, or money raised",
                              list("Number Projects" = 'num',
                                   "Success Rate"='suc', 
                                   "Money Raised"='mon')
                 )
                 
    ),
    # Show a plot of the generated distribution
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data Table",
                           (DT::dataTableOutput("kickstarter"))),
                  
                  tabPanel("Categories",
                           p(),
                           radioButtons("p", "Choose your content of interest:",
                                        list("Main Category"='a', "Sub Category"='b')),
                           #slider for top number of categories that appears
                           uiOutput(outputId = "topnumber1"),
                           plotOutput("plot1"),
                           plotOutput("plot2")),
                  
                  tabPanel("Geography",
                           h1("Geographic Distribution"),
                           selectInput("geoFocus",
                                       "Geography Focus", 
                                       choices = c("World", "Europe", 
                                                   "North America", "Asia Pacific")),
                           leafletOutput("map1"),
                           h3("US vs Foreign project share over time"),
                           plotOutput("plot3"),
                           h3("Breakdown of Foreign project share over time"),
                           plotOutput("plot4")),
                  tabPanel("Hypothesis Test",
                           h3("Sample Hypothesis"),
                           selectInput("hypothesis", label = "Null Hypothesis",
                                       choices = c(
                             "On average, Projects in Film&Video have same success rate as projects in Music in 2017"="h1",
                             "On average, projects that are launched in the USA have same success rate as projects that are launched in Great Britain in 2017."="h2",
                             "On average, projects that are launched in the USA do raise more money than foreign projects in 2017."="h3",
                             "Tabletop Game Projects on average raise more money than Video Game Projects in 2017."="h4")
                             ),
                           h3("Statistics"),
                           radioButtons("s","Choose your content of interests:",
                                        list("Descriptive Statistics"='DS',
                                             "Stat-Test"='ST')),
                           verbatimTextOutput("text"),
                           textOutput("hypResult")
                           )
                           
                      )
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$kickstarter <- DT::renderDataTable({
    datatable(kickstarter)
  }, escape=FALSE)
  
  output$topnumber1 <- renderUI({
    if(input$p == 'a'){
      i<-4
    }
    if(input$p == 'b'){
      i<-3
    }    
    l = as.numeric(length(unique(kickstarter[[i]])))
    sliderInput("topnumber1",
                "Top number of categories:",
                min = 1,
                max = l,
                value = l/4)
  })
  
  numCategories <- reactive({
    return(as.numeric(input$topnumber1))
  })
  
  latLongDefault <- reactive({
    geo <- input$geoFocus
    if (geo == "Europe") {
      centerLat <- 51
      centerLong <- 5
      zoom <- 3.5
    }
    if (geo == "North America") {
      centerLat <- 40
      centerLong <- -100
      zoom <- 3
    }
    if (geo == "Asia Pacific") {
      centerLat <- -5
      centerLong <- 140
      zoom <- 2
    }
    if (geo == "World") {
      centerLat <- 15
      centerLong <- 10
      zoom <- 1.5
    }
    bounds <- data.frame(cbind(centerLat, centerLong, zoom))
    return(bounds)
  })
  
  output$plot1 <- renderPlot({
    
    if(input$p == 'a'){
      i<-4
    }
    if(input$p == 'b'){
      i<-3
    }
    
    #Averaging money spent only for projects with >= 1 backer
    category.freq <- kickstarter %>%
      group_by(kickstarter[[i]]) %>%
      summarize(count=n(),
                avgMoney = as.numeric(sum(usd_pledged_real) / sum(backers >= 1)),
                successRate = sum(successful == "yes", na.rm = TRUE) /
                  sum(successful == "yes" | successful == "no", na.rm = TRUE) * 100
      ) %>%
      arrange(desc(count))
    
    colnames(category.freq)[1] <- "cat"
    
    category.freq$cat <- 
      factor(category.freq$cat, 
             levels=category.freq$cat)
    
    data2plot <- category.freq[1:as.numeric(input$topnumber1),]
    
    if (input$success == 'suc') {
      yVar <- 'successRate'
      yLabel <- "Success Rate (%)"
    }
    if (input$success == 'mon') {
      yVar <- 'avgMoney'
      yLabel <- "Average Money Raised ($)"
    }
    if (input$success == 'num') {
      yVar <- 'count'
      yLabel <- "Total Number of Projects"
    }
    
    
    ggplot(data2plot, aes(x=data2plot$cat, y=eval(parse(text = yVar)))) + 
      geom_bar(aes(fill = palette(rainbow(numCategories()))),
               stat="identity", width=0.8, 
               position = position_dodge(0.8)) + 
      ggtitle("Projects by Category Frequency") + 
      xlab("Category") + 
      ylab(yLabel) + 
      theme(text = element_text(size=14),
            axis.text.x = element_text(angle=45, hjust=1),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            legend.position = "none")
  })
  
  
  
  output$plot2 <- renderPlot({
    if(input$p == 'a'){
      i<-4
    }
    if(input$p == 'b'){
      i<-3
    }
    
    #Averaging money spent only for projects with >= 1 backer
    category.freq <- kickstarter %>%
      group_by(kickstarter[[i]]) %>%
      summarize(count=n(),
                avgMoney = as.numeric(sum(usd_pledged_real) / sum(backers >= 1)),
                successRate = sum(successful == "yes", na.rm = TRUE) /
                  sum(successful == "yes" | successful == "no", na.rm = TRUE) * 100
      ) %>%
      arrange(desc(count))
    
    colnames(category.freq)[1] <- "cat"
    
    category.freq$cat <- 
      factor(category.freq$cat, 
             levels=category.freq$cat)
    
    data2plot <- category.freq[1:as.numeric(input$topnumber1),]
    
    if (input$success == 'suc') {
      yVar <- 'successRate'
      yLabel <- "Success Rate (%)"
    }
    if (input$success == 'mon') {
      yVar <- 'avgMoney'
      yLabel <- "Average Money Raised ($)"
    }
    if (input$success == 'num') {
      yVar <- 1
      yLabel <- ""
    }
    
    
    ggplot(data2plot, aes(x=count, y= eval(parse(text = yVar)) , 
                          color = palette(rainbow(numCategories()))))+
      geom_point(size=5) +
      ggtitle("Success  VS. Number of Projects by Categories") + 
      xlab("Number of Projects") + 
      ylab(yLabel) +
      geom_label_repel(aes(label = as.character(data2plot$cat)),
                       box.padding = 0.35,
                       point.padding = 0.5) + 
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            legend.position="none")
  })
  
  output$map1 <- renderLeaflet({
    # Summarizing metrics by country
    geo.freq <- kickstarter %>% 
      group_by(country) %>% 
      summarize(count = n(),
                avgMoney = as.numeric(sum(usd_pledged_real) / sum(backers >= 1)),
                successRate = sum(successful == "yes", na.rm = TRUE) /
                  sum(successful == "yes" | successful == "no", na.rm = TRUE) * 100
      ) %>% 
      arrange(desc(count))
    
    geo.freq <- na.omit(geo.freq)
    geo.freq$country <- trimws(as.character(geo.freq$country))
    countries$country <- trimws(as.character(countries$country))
    geo.freq <- inner_join(geo.freq, countries)
    geo.freq$country <- factor(geo.freq$country)
    
    geo.freq$successRadius <- geo.freq$successRate * 6000
    geo.freq$moneyRadius <- sqrt(geo.freq$avgMoney) * 2000
    geo.freq$projRadius <- sqrt(geo.freq$count) * 3000
    
    colRamp <- colorRampPalette(c("green", "red"))
    
    geo.freq$Color <- colRamp(nrow(geo.freq))
    
    if (input$success == 'suc') {
      evalVar <- 'successRadius'
      popOut <- 'successRate'
      evalTitle <- 'Project Success Rate (%) : '
      
      geo.freq <- geo.freq %>% arrange(desc(successRate))
      geo.freq$Color <- colRamp(nrow(geo.freq))
    }
    if (input$success == 'mon') {
      evalVar <- 'moneyRadius'
      popOut <- 'avgMoney'
      evalTitle <- 'Average USD Raised: $'
      
      geo.freq <- geo.freq %>% arrange(desc(avgMoney))
      geo.freq$Color <- colRamp(nrow(geo.freq))
    }
    if (input$success == 'num') {
      evalVar <- 'projRadius'
      popOut <- 'count'
      evalTitle <- 'Number Projects: '
      
      geo.freq <- geo.freq %>% arrange(desc(count))
      geo.freq$Color <- colRamp(nrow(geo.freq))
    }
    
    bounds <- latLongDefault()
    
    leaflet(geo.freq, options = leafletOptions(zoomControl = FALSE)) %>% 
      addTiles() %>%
      addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                 color = ~Color, radius = ~eval(parse(text = evalVar)), 
                 popup = ~paste(countryName,
                                "<br/>",
                                evalTitle,
                                round(eval(parse(text = popOut)))), opacity = 1 
      ) %>% 
      setView(lng = bounds$centerLong, 
              lat = bounds$centerLat, 
              zoom = bounds$zoom)
    
  })
  
  
  output$plot3 <- renderPlot({
    
    kickMonthGeo <- kickstarter %>% 
      mutate(month = floor_date(launched, "month"),
             isUS = if_else(country == "US", "US", "Foreign")) %>% 
      group_by(month, isUS) %>% 
      summarize(count=n(),
                money = as.numeric(sum(usd_pledged_real)),
                success = sum(successful == "yes", na.rm = TRUE)) %>% 
      mutate(projFreq = count / sum(count),
             moneyFreq = money / sum(money),
             successFreq = success / sum(success))
    
    kickMonthGeo <- na.omit(kickMonthGeo)
    
    if (input$success == 'suc') {
      evalVar <- 'successFreq'
      evalTitle <- 'Percent share of Successful Projects'
    }
    if (input$success == 'mon') {
      evalVar <- 'moneyFreq'
      evalTitle <- 'Percent share of Money Pledged ($)'
    }
    if (input$success == 'num') {
      evalVar <- 'projFreq'
      evalTitle <- 'Percent share of Total Projects'
    }
    
    ggplot(kickMonthGeo, aes(x = month, y = eval(parse(text = evalVar)), fill = isUS)) + 
      geom_area(position = 'stack') +
      ggtitle(evalTitle) + 
      xlab("Time") +
      ylab("Percent") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
      guides(fill=guide_legend(title=NULL))
    
  })
  
  output$plot4 <- renderPlot({
    
    foreignMonthGeo <- kickstarter %>%
      filter(country != 'US') %>% 
      mutate(month = floor_date(launched, "month")
      ) %>% 
      group_by(month, country) %>% 
      summarize(count=n(),
                money = as.numeric(sum(usd_pledged_real)),
                success = sum(successful == "yes", na.rm = TRUE)) %>% 
      mutate(projFreq = count / sum(count),
             moneyFreq = money / sum(money),
             successFreq = success / sum(success))
    
    aggregateForeignGeo <- kickstarter %>%
      filter(country != 'US') %>% 
      mutate(month = floor_date(launched, "month")
      ) %>% 
      group_by(country) %>% 
      summarize(count=n(),
                money = as.numeric(sum(usd_pledged_real)),
                success = sum(successful == "yes", na.rm = TRUE)) 
    
    foreignMonthGeo <- na.omit(foreignMonthGeo)
    foreignMonthGeo <- ungroup(foreignMonthGeo)
    
    if (input$success == 'suc') {
      evalVar <- 'successFreq'
      evalTitle <- 'Percent share of Successful Projects'
      
      foreignMonthGeo$country <- as.character(foreignMonthGeo$country)
      topN <- aggregateForeignGeo %>% arrange(desc(success))
      topN <- topN[1:8, 'country']
      
      ungroup(foreignMonthGeo)
      foreignMonthGeo <- foreignMonthGeo %>% 
        mutate(topCountry = ifelse(country %in% as.character(topN$country), country, 'other')) %>%
        select(month, success, topCountry) %>% 
        group_by(month, topCountry) %>% 
        summarize(success = sum(success)) %>% 
        mutate(successFreq = success / sum(success))
      
      foreignMonthGeo <- na.omit(foreignMonthGeo)
    }
    if (input$success == 'mon') {
      evalVar <- 'moneyFreq'
      evalTitle <- 'Percent share of Money Pledged ($)'
      
      foreignMonthGeo$country <- as.character(foreignMonthGeo$country)
      topN <- aggregateForeignGeo %>% arrange(desc(money))
      topN <- topN[1:8, 'country']
      
      ungroup(foreignMonthGeo)
      foreignMonthGeo <- foreignMonthGeo %>% 
        mutate(topCountry = ifelse(country %in% as.character(topN$country), country, 'other')) %>%
        select(month, money, topCountry) %>% 
        group_by(month, topCountry) %>% 
        summarize(money = sum(money)) %>% 
        mutate(moneyFreq = money / sum(money))
      
      foreignMonthGeo <- na.omit(foreignMonthGeo)
    }
    if (input$success == 'num') {
      evalVar <- 'projFreq'
      evalTitle <- 'Percent share of Total Projects'
      
      foreignMonthGeo$country <- as.character(foreignMonthGeo$country)
      topN <- aggregateForeignGeo %>% arrange(desc(count))
      topN <- topN[1:8, 'country']
      
      ungroup(foreignMonthGeo)
      foreignMonthGeo <- foreignMonthGeo %>% 
        mutate(topCountry = ifelse(country %in% as.character(topN$country), country, 'other')) %>%
        select(month, count, topCountry) %>% 
        group_by(month, topCountry) %>% 
        summarize(count = sum(count)) %>% 
        mutate(projFreq = count / sum(count))
      
      foreignMonthGeo <- na.omit(foreignMonthGeo)
    }
    
    ggplot(foreignMonthGeo, aes(x = month, y = eval(parse(text = evalVar)), fill = topCountry)) + 
      geom_area(position = 'stack') +
      ggtitle(evalTitle) + 
      xlab("Time") +
      ylab("Percent") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
      guides(fill=guide_legend(title=NULL))+
      scale_x_date(breaks = "1 years", date_labels = "%Y")
  })
  
  
  output$text <- renderPrint({
    
    if (input$s == 'DS'){
      if(input$hypothesis == 'h1'){
        
        filmAndVid <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                 launched < as.Date('2018-01-01', "%Y-%m-%d") &
                 main_category == 'Film & Video' &
                 !is.na(successful))  %>% 
          group_by(main_category) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum(successful=='no'))
        
        music <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   main_category == 'Music' &
                   !is.na(successful))  %>% 
          group_by(main_category) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum (successful == 'no'))
        
        Vid_Compare_Music <- as.matrix(rbind(filmAndVid[,2:3], music[,2:3]))
        rownames(Vid_Compare_Music) <- c("Film and Vid", "Music")
        
        results <- Vid_Compare_Music
      
      }
      if(input$hypothesis == 'h2'){
        
        US <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'US' &
                   !is.na(successful))  %>% 
          group_by(country) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum(successful=='no'))
        
        GB <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'GB' &
                   !is.na(successful))  %>% 
          group_by(country) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum (successful == 'no'))
        
        US_Compare_GB <- as.matrix(rbind(US[,2:3], GB[,2:3]))
        rownames(US_Compare_GB) <- c("US", "GB")
        
        results <- US_Compare_GB
        
      }
      if(input$hypothesis == 'h3'){
        
        US <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'US' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
          
        
        foreign <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country != 'US' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
        
        
        US_Summary <- summary(US$usd_pledged_real)
        Foreign_Summary <- summary(foreign$usd_pledged_real)
        US_Compare_Foreign <- as.matrix(rbind(US_Summary, Foreign_Summary))
        rownames(US_Compare_Foreign) <- c("US", "Foreign")
        
        results <- US_Compare_Foreign
        
      }
      if(input$hypothesis == 'h4'){
        
        tableTop <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   category == 'Tabletop Games' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
        
        
        videoGames <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   category == 'Video Games' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
        
        
        tableTop_Summary <- summary(tableTop$usd_pledged_real)
        videoGame_Summary <- summary(videoGames$usd_pledged_real)
        TableTop_Compare_VideoGame <- as.matrix(rbind(tableTop_Summary, videoGame_Summary))
        rownames(TableTop_Compare_VideoGame) <- c("Tabletop", "Videogames")
        
        results <- TableTop_Compare_VideoGame
        
      }
    }
    
    if (input$s == 'ST'){
      if(input$hypothesis == 'h1'){
        
        filmAndVid <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   main_category == 'Film & Video' &
                   !is.na(successful))  %>% 
          group_by(main_category) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum(successful=='no'))
        
        music <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   main_category == 'Music' &
                   !is.na(successful))  %>% 
          group_by(main_category) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum (successful == 'no'))
        
        Vid_Compare_Music <- as.matrix(rbind(filmAndVid[,2:3], music[,2:3]))
        rownames(Vid_Compare_Music) <- c("Film and Vid", "Music")
        
        results <- prop.test(x = Vid_Compare_Music, alternative = "two.sided", conf.level = 0.95)
        
      }
      if(input$hypothesis == 'h2'){
        
        US <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'US' &
                   !is.na(successful))  %>% 
          group_by(country) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum(successful=='no'))
        
        GB <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'GB' &
                   !is.na(successful))  %>% 
          group_by(country) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum (successful == 'no'))
        
        US_Compare_GB <- as.matrix(rbind(US[,2:3], GB[,2:3]))
        rownames(US_Compare_GB) <- c("US", "GB")
        
        results <- prop.test(x = US_Compare_GB, alternative = "two.sided", conf.level = 0.95)
      }
      if(input$hypothesis == 'h3'){
        
        US <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'US' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
        
        
        foreign <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country != 'US' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
        
        results <- t.test(US$usd_pledged_real, foreign$usd_pledged_real, alternative = "less")
      }
      if(input$hypothesis == 'h4'){
        
        tableTop <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   category == 'Tabletop Games' &
                   backers >= 1) %>% 
          select(ID, name, country, usd_pledged_real)
        
        
        videoGames <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   category == 'Video Games' &
                   backers >= 1) %>% 
          select(ID, name, country, usd_pledged_real)
        
        results <- t.test(tableTop$usd_pledged_real, videoGames$usd_pledged_real, alternative = "less")
        
      }
    }
    
    results
  })
  
  output$hypResult <- renderText({
    if (input$s == 'ST'){
      if(input$hypothesis == 'h1'){
        
        filmAndVid <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   main_category == 'Film & Video' &
                   !is.na(successful))  %>% 
          group_by(main_category) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum(successful=='no'))
        
        music <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   main_category == 'Music' &
                   !is.na(successful))  %>% 
          group_by(main_category) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum (successful == 'no'))
        
        Vid_Compare_Music <- as.matrix(rbind(filmAndVid[,2:3], music[,2:3]))
        rownames(Vid_Compare_Music) <- c("Film and Vid", "Music")
        
        p <- prop.test(x = Vid_Compare_Music, alternative = "two.sided", conf.level = 0.95)$p.value
        
        if (p <0.05) {
          results <- "REJECT NULL HYPOTHESIS - We can conclude that Film&Video projects and Music projects do not have same success rate on average"
        } else {
          results <- "DO NOT REJECT NULL HYPOTHESIS - We failed to reject the null hypothesis. We cannot say that Film&Video projects and Music projects have different success rates"
        }
      }
      if(input$hypothesis == 'h2'){
        
        US <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'US' &
                   !is.na(successful))  %>% 
          group_by(country) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum(successful=='no'))
        
        GB <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'GB' &
                   !is.na(successful))  %>% 
          group_by(country) %>%
          summarize(success = sum(successful=='yes'),
                    failure = sum (successful == 'no'))
        
        US_Compare_GB <- as.matrix(rbind(US[,2:3], GB[,2:3]))
        rownames(US_Compare_GB) <- c("US", "GB")
        
        p <- prop.test(x = US_Compare_GB, alternative = "two.sided", conf.level = 0.95)$p.value
        
        if (p <0.05) {
          results <- "REJECT NULL HYPOTHESIS - We can conclude that the US based projects and Great Britain based projects do not have same success rate on average"
        } else {
          results <- "DO NOT REJECT NULL HYPOTHESIS - We failed to reject the null hypothesis. Cannot say that US based projects and Great Britain based projects have different success rates"
        }
      }
      if(input$hypothesis == 'h3'){
        
        US <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country == 'US' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
        
        
        foreign <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   country != 'US' &
                   backers >= 1) %>% 
          select(country, usd_pledged_real)
        
        p <- t.test(US$usd_pledged_real, foreign$usd_pledged_real, alternative = "less")$p.value
        
        if (p <0.05) {
          results <- "REJECT NULL HYPOTHESIS 
            - We can conclude that on average, projects launched outside the US raise more money than projects launched in the USA"
        } else {
          results <- "DO NOT REJECT NULL HYPOTHESIS 
              - We failed to reject the null hypothesis. In average, projects that are launched in the USA do raise more money than foreign projects in 2017."
            }
      }
      if(input$hypothesis == 'h4'){
        
        tableTop <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   category == 'Tabletop Games' &
                   backers >= 1) %>% 
          select(ID, name, country, usd_pledged_real)
        
        
        videoGames <- kickstarter %>%
          filter(launched >= as.Date('2017-01-01', "%Y-%m-%d") & 
                   launched < as.Date('2018-01-01', "%Y-%m-%d") &
                   category == 'Video Games' &
                   backers >= 1) %>% 
          select(ID, name, country, usd_pledged_real)
        
        p <- t.test(tableTop$usd_pledged_real, videoGames$usd_pledged_real, alternative = "less")$p.value
        
        if (p <0.05) {
          results <- "REJECT NULL HYPOTHESIS - We conclude that On average, Table Top Game projects don't raise more money than Videogame projects"
        } else {
          results <- "DO NOT REJECT NULL HYPOTHESIS 
          - We failed to reject the null hypothesis. On average, Table Top Game projects do raise more money than Videogame projects"
        }
      }
    }
    
    if (input$s == 'DS'){results <- ""}
    
    results
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

