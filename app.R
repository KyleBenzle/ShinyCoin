library(shiny)
library(crypto)
library(remotes)
library(crypto)
library(ggplot2)
library(lubridate)
library(plotly)
library(zoo)


makeGraph <- function(coinList, startDate, endDate, df){
  df <- df %>% dplyr::filter(., symbol %in% coinList )
  graph <- ggplotly(ggplot(df, aes(x = date, y = open, col=slug))+
                      geom_line() +  labs(title = "Price in BTC", y= "Price in USD", x = "Date Range") +
                      ggtitle("Price in USD"))
  return(graph)}


makeGraphRatio <- function(coinList, startDate, endDate, df, coinDenom){
  df <- df %>% dplyr::filter(., symbol.x %in% coinList )
  graph <- ggplotly(ggplot(df, aes(x = date, y = priceRatio, col=slug))+
                      geom_line() + labs(color = "Portfolio", title = paste("Price in ", coinDenom), y= paste("Price in: ", coinDenom) , x = "Date Range") +
                      ggtitle(paste("Price in ", coinDenom)))
return(graph)}


makeGraphRolling <- function(coinList, startDate, endDate, df, coinDenom, allCoinDataTopValue){
  df <- df %>% dplyr::filter(., symbol %in% coinList )
  graph <- ggplotly(ggplot(df, aes(x = date, y = rollingMean, col=slug))+
                      geom_line() + labs(title = paste("Price in ", coinDenom), y= paste("Price in: ", coinDenom) , x = "Date Range") +
                      ggtitle("Rolling Mean Price")) 
return(graph)}



makeGraphBuySell <- function(coinList, startDate, endDate, df, coinDenom, allCoinDataTopValue){
  df <- df %>% dplyr::filter(., symbol %in% coinList )
  graph <- ggplotly(ggplot(df, aes(x = date, y = rollingMean/open, col=slug))+
                      geom_line() + labs(title = paste("Price in ", coinDenom), y= paste("Price in: ", coinDenom) , x = "Date Range") +
                      ggtitle("Rolling Mean/Daily Price")) 
return(graph)}


### Fetch historic data and save as .csv
# fullCoinList <- c("BTC", "ETH", "BCH", "XRP", "LTC", "BSV", "XMR", "BAT", "ZRX", "KNC")
# historic <- crypto_history(fullCoinList, start_date = '20170101', end_date = '20200930')
# write.csv(historic, "allDataToSep1.csv")


# Data 
fullCoinList <- c("BTC", "ETH", "BCH", "XRP", "LTC", "BSV", "XMR", "BAT", "ZRX", "KNC")
allCoinDataBeforeSep01 <- read.csv('allDataToOct1.csv', stringsAsFactors = FALSE)
allCoinDataAfterSep01 <- crypto_history(fullCoinList, start_date = '20201001')
allCoinDataBeforeSep01 <- dplyr::select(allCoinDataBeforeSep01, -1)
allCoinDataBeforeSep01$date <- as.Date(allCoinDataBeforeSep01$date)
allCoinDataAfterSep01$date <- as.Date(allCoinDataAfterSep01$date)
allCoinData <- rbind(allCoinDataBeforeSep01, allCoinDataAfterSep01)
allCoinData$date <- as.Date(allCoinData$date)


ui <- fluidPage(
  titlePanel("Cryptocurrency ratio calculator"),
  sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "coinDenom", 
                     label = "Choose Denomination", 
                     choices = unique(fullCoinList)),
      
        checkboxGroupInput("coinCheckChoice", "Choose coins:",
                         choiceNames =
                           fullCoinList,
                         choiceValues =
                           fullCoinList,
                         selected='BTC'),
        
        sliderInput("slider1", label = h3("Choose Rolling Mean in Days"), min = 1, 
                    max = 100, value = 3, step	= 2),
        
      textOutput('coinDenom'),

      textOutput("workingCoinList"),
      textOutput("coinCheckChoice"),
      htmlOutput('buySell'),
      textOutput("dateRangeStart"),
      textOutput("dateRangeEnd"),
      dateRangeInput("dateRangeInput", "Date range:",
                     start = "2018-01-01",
                     end   = "2020-09-01"),),
    
      
mainPanel(
      plotlyOutput("distPlot"),
      plotlyOutput("distPlotRatio"),
      plotlyOutput("distPlotRolling"),
      plotlyOutput("distPlotBuySell"),

)))


server <- function(input, output) {
  
  output$workingCoinList <- renderText({
    coinList <- paste(input$coinCheckChoice, collapse = ", ")
    paste("You portfolio: ", coinList)})
  
  
  output$coinDenom <- renderText({
    coinDenom <- paste(input$coinDenom)
    paste("You denom: ", coinDenom)})
  
  
  output$dateRangeInput <- renderText({
    dateRangeInput <- paste(input$dateRangeInput)
    paste("You date: ", dateRangeInput)})
  
  
  
  output$buySell <- renderText({
    allCoinDataRolling <- allCoinData %>%   mutate(rollingMean = rollmean(open, k = input$slider1, fill = NA))  
    denomPrice <- allCoinData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., c(open, date, symbol))
    activeCoinData <- allCoinData %>% dplyr::mutate(., ratioPriceDenom=(open/denomPrice$open)) 
    userDateInput <- as.Date(input$dateRangeInput) 
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::arrange(., desc(date))
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::mutate(., rollingRatio=(rollingMean/denomPrice$open)) 
    allCoinDataTopValue <- allCoinDataRolling %>% group_by(., symbol) %>% slice(1)
    allCoinDataTopValue$rollingRatioSignal <- lapply(allCoinDataTopValue$rollingRatio, function(x) ifelse(x>1, 'sell', 'buy'))
    signals <- paste(allCoinDataTopValue$symbol, ':', allCoinDataTopValue$rollingRatioSignal)
    paste(allCoinDataTopValue$symbol, ':', allCoinDataTopValue$rollingRatioSignal, " <br> ")})
  
  
output$value <- renderPrint({ input$slider1 })


output$distPlot <- renderPlotly({
  allCoinDataRolling <- allCoinData %>% mutate(rollingMean = rollmean(open, k = input$slider1, fill = NA))  
  denomPrice <- allCoinData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., c(open, date, symbol))
  activeCoinData <- allCoinData %>% dplyr::mutate(., ratioPriceDenom=(open/denomPrice$open)) 
  userDateInput <- as.Date(input$dateRangeInput) 
  activeCoinData <- activeCoinData %>% dplyr::filter(., date >= as.Date(userDateInput[1]) & date <= as.Date(userDateInput[2]))
  makeGraph(input$coinCheckChoice, userDateInput[1], userDateInput[2], activeCoinData)
  }) 


  output$distPlotRolling <- renderPlotly({
    
    # New column for the rolling mean
    allCoinDataRolling <- allCoinData %>%
      mutate(rollingMean = rollmean(open, k = input$slider1, fill = NA))  
    
    
    denomPrice <- allCoinData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., c(open, date, symbol))
    
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::mutate(., ratioPriceDenom=(open/denomPrice$open)) 
    userDateInput <- as.Date(input$dateRangeInput) 
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::filter(., date >= as.Date(userDateInput[1]) & date <= as.Date(userDateInput[2]))
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::arrange(., desc(date))
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::mutate(., rollingRatio=(rollingMean/open)) 
    allCoinDataTopValue <- allCoinDataRolling %>% group_by(., symbol) %>% slice(1)
    allCoinDataTopValue$rollingRatio <- lapply(allCoinDataTopValue$rollingRatio, function(x) ifelse(x>1, 'sell', 'buy'))
    makeGraphRolling(input$coinCheckChoice, userDateInput[1], userDateInput[2], allCoinDataRolling, input$coinDenom, allCoinDataTopValue)
  }) 


  
  

  output$distPlotBuySell <- renderPlotly({
    
    allCoinDataRolling <- allCoinData %>%
      mutate(rollingMean = rollmean(open, k = input$slider1, fill = NA))  
    denomPrice <- allCoinData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., c(open, date, symbol))
    activeCoinData <- allCoinData %>% dplyr::mutate(., ratioPriceDenom=(open/denomPrice$open)) 
    userDateInput <- as.Date(input$dateRangeInput) 
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::filter(., date >= as.Date(userDateInput[1]) & date <= as.Date(userDateInput[2]))
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::arrange(., desc(date))
    allCoinDataRolling <- allCoinDataRolling %>% dplyr::mutate(., rollingRatio=(rollingMean/open)) 
    allCoinDataTopValue <- allCoinDataRolling %>% group_by(., symbol) %>% slice(1)
    allCoinDataTopValue$rollingRatio <- lapply(allCoinDataTopValue$rollingRatio, function(x) ifelse(x>1, 'sell', 'buy'))
    makeGraphBuySell(input$coinCheckChoice, userDateInput[1], userDateInput[2], allCoinDataRolling, input$coinDenom, allCoinDataTopValue)
    
    
    
    
    
  }) 

  
  output$distPlotRatio <- renderPlotly({
    denomPrice <- allCoinData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., c(open, date, symbol))
    activeCoinData <- allCoinData %>%  group_by(., symbol) %>% dplyr::inner_join(denomPrice, .data, by='date' )
    activeCoinData$priceRatio <- activeCoinData$open.x/activeCoinData$open.y
    userDateInput <- as.Date(input$dateRangeInput) 
    activeCoinData <- activeCoinData %>% dplyr::filter(., date >= as.Date(userDateInput[1]) & date <= as.Date(userDateInput[2]))
    makeGraphRatio(input$coinCheckChoice, userDateInput[1], userDateInput[2], activeCoinData, input$coinDenom)
  })}




shinyApp(ui = ui, server = server)












#  write.csv(coinDataRaw2, 'newRawData.csv')

# denomPrice <- tempData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., open)
# coinDataRaw2 <- tempData %>% dplyr::mutate(., ratioPriceDenom=(open/denomPrice$open))



#    write.csv(coinDataRaw2, 'newRawData.csv')
#   tempData <- crypto_history(newCoinInput, start_date=userDateInput[1], end_date=userDateInput[2])    


# coinDataRaw2$ratioPriceDenom <- tempData %>% dplyr::mutate(., ratioPriceDenom = (open/denomPrice$open))
###
# tempData <- coinDataRaw3


# denomPrice <- tempData %>% dplyr::filter(., symbol == 'BTC') %>% dplyr::select(., open)
# coinDataRaw2 <- tempData %>% dplyr::mutate(., ratioPriceDenom=(open/denomPrice$open))
# 

####

# coinDataRaw3 <- crypto_history('btc', start_date='20180101', end_date='20200101') 
# coinDataRawTest <- coinDataRaw3
# denomPrice <- coinDataRaw3 %>% dplyr::filter(., symbol == 'BTC') %>% dplyr::select(., open)
# coinDataRaw3$ratioPriceDenom=coinDataRaw3$open/denomPrice$open

#coinDataRaw3 <- coinDataRaw3 %>% dplyr::mutate(., ratioPriceDenom=open/denomPrice)

# coinDataRaw3 <- crypto_history('btc', start_date='20180101', end_date='20200101') 
# 
# coinDataRaw3 %>% groupby(symbol)
# 
# coinDenomList <- coinDataRaw2 %>% filter(slug==coinDenom) 
# 
# date / slug 1 / slug 2 / 
# 
# %>% group_by(., slug) 

#    date, slug, open

#  denom <- coinDataRaw3 %>% filter(., symbol == input$coinDenom) %>% select(., open) 




# allCoinDataToSep01 <- read.csv('allCoinData.csv')
# allCoinDataAfterSep01 <- crypto_history(fullCoinList, start_date = '20200901')
# allCoinData <- bind_rows(allCoinDataToSep01, allCoinDataAfterSep01)

# allCoinData$symbol <- tolower(allCoinData$symbol)


# 
# 1. Make new data frame, coinData, with the columns: 
#   1.a. (coinName, startDate, endDate, closePrice)
#
# 2. User selects their "portfolio" of "tickers" using checkboxes input
#   2.a. Assign user's input T/F data to "selectedTF" in data frame
#   2.b. Make new list of names of selected coins, selectedCoins.
#
# 3. User selects time frame using slider bar input
#   3.a. Assign date input to start/end Date
#
# 4. Download data of given "tickers"
#   4.a. Assign data to coinData data frame
# 
# 5. Plot:    
#     a. Price of each "ticker" over given time frame in USD 
#     b-n. Price of each "ticker" over given time in "price of other selected coins" usd pice/usd price
#     



#111
# generate bins based on input$bins from ui.R
# x    <- kinHistory[, 6]
# bins <- seq(min(x), max(x), length.out = input$dateRange + 1)
# 
# # draw the histogram with the specified number of bins
# hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
# plot(x)
# 
# output$value0 <- renderText({ input$btc })
# output$value1 <- renderText({ input$eth })
# output$value2 <- renderText({ input$bch })
# output$value3 <- renderText({ input$xrp })
# output$value4 <- renderText({ input$ltc })
# output$value5 <- renderText({ input$bsv })
# output$value6 <- renderText({ input$xmr })
# output$value7 <- renderText({ input$bat })
# output$value8 <- renderText({ input$zrx })
# output$knc <- renderText({ input$knc })
# 

# For date, use number of months prior to current.
# plotNumber =  (length(coinList) * (length(coinList)-1)) / 2  



# 
# coinDataRaw$open[tolower(coinDataRaw$symbol) == coinList[1]]  /
#     coinDataRaw$open[tolower(coinDataRaw$symbol) == coinList[2]]  
# 


# 

# 
#             checkboxInput("btc", "BTC", T),
#            # verbatimTextOutput("value0"),
#             checkboxInput("eth", "ETH", F),
#           #  verbatimTextOutput("value1"),
#             checkboxInput("bch", "BCH", F),
#            # verbatimTextOutput("value2"),
#             checkboxInput("xrp", "XRP", F),
#            # verbatimTextOutput("value3"),
#             checkboxInput("ltc", "LTC", F),
#            # verbatimTextOutput("value4"),
#             checkboxInput("bsv", "BSV", F),
#            # verbatimTextOutput("value5"),
#             checkboxInput("xmr", "XMR", F),
#           #  verbatimTextOutput("value6"),
#             checkboxInput("bat", "BAT", F),
#            # verbatimTextOutput("value7"),
#             checkboxInput("zrx", "ZRX", F),
#            # verbatimTextOutput("value8"),
#             checkboxInput("knc", "KNC", F),
#            # verbatimTextOutput("knc"),


# cList <- c('btc', 'eth')
# crypto_history(cList)
# 
# btcData <- crypto_history("btc")
# 
# ethData <- crypto_history("eth")
# bchData <- crypto_history("bch")
# kinHistory <- crypto_history("kin")


###

# ll_product1 = reactive({
#     lululemon_reviews %>% filter(., `Product Name` == input$product_comparison1)
# })



###




# coinData <- data.frame("coinName" = coinList, 
#                        "date" = USER SELECTED TIME FRAME, 
#                        "price" = PRICE DATA)





# Get the data and assign to "coinName"+Data

# for(i in selectedCoins){
#     cat(i + "Data") <- ?crypto_history('i')
#     coinData[i] <- RBIND[i]

# }


# Done one-by-one

# head(btcData)



###



# How to show selecting time in the past?          

# sliderInput("dateRange", "Select Time Frame:",
#             min = 1, max = 48,
#             value = c(12,48)),




###



# if user inputs date beofer coin existed need to say COIN did not exist in DATE    
# coinList = reactive({
#     df %>% filter(., `ticker` in c(input$ticker1, input$ticker2))
# })


# coinList = c('btc','eth','bch')
# coinList <- paste(input$coinCheckChoice, collapse = ", ")
