library(shiny)

makeGraph <- function(coinList, startDate, endDate, df){

  df <- df %>% dplyr::filter(., symbol %in% toupper(coinList)) 
  # %>% dplyr::filter(., date >= as.date(startDate) & date <= as.date(endDate))
  graph <- ggplot(df, aes(x = date, y = open, col=slug))+
    geom_line() +
    ggtitle("by date")
  return(graph)
}

makeGraphRatio <- function(coinList, startDate, endDate, df){
  df <- df %>% dplyr::filter(., symbol %in% toupper(coinList) )
  graph <- ggplot(df, aes(x = date, y = newOpen, col=slug))+
    geom_line() +
    ggtitle("by date")
  return(graph)
}


fullCoinList <- c("BTC", "ETH", "BCH", "XPR", "LTC", "BSV", "XMR", "BAT", "ZRX", "KNC")
lowerFullCoinList <- tolower(fullCoinList)

# allCoinDataToSep01 <- read.csv('newRawData.csv')
# allCoinDataAfterSep01 <- crypto_history(lowerFullCoinList, start_date = '20200901')
# allCoinData <- bind_rows(allCoinDataToSep01, allCoinDataAfterSep01)






ui <- fluidPage(
  
  titlePanel("Cryptocurrency ratio calculator"),
  
  # Image wont show
  #    img(src='headImage.jpg', width='640'),
  
  sidebarLayout(
    sidebarPanel(
      
      selectizeInput(inputId = "coinDenom", 
                     label = "Choose Denom", 
                     choices = unique(fullCoinList)),
      
      
      checkboxGroupInput("coinCheckChoice", "Choose coins:",
                         choiceNames =
                           list('btc','eth','bch','xpr', 'ltc', 'bsv', 'xmr','bat','zrx','knc'),
                         choiceValues =
                           list('btc','eth','bch','xpr', 'ltc', 'bsv', 'xmr','bat','zrx','knc'),
                         
                         selected='btc'
      ),
      
      textOutput('coinDenom'),
      
      textOutput("workingCoinList"),
      textOutput("coinCheckChoice"),
      textOutput("dateRangeStart"),
      textOutput("dateRangeEnd"),
      dateRangeInput("dateRange", "Date range:",
                     start = "2018-01-01",
                     end   = "2020-05-31"),
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlotRatio"),
      
    )))


server <- function(input, output) {
  library(remotes)
  library(crypto)
  library(shiny)
  library(ggplot2)
  library(lubridate)
  
  
  output$workingCoinList <- renderText({
    coinList <- paste(input$coinCheckChoice, collapse = ", ")
    paste("You portfolio: ", coinList)
  })
  
  
  output$coinDenom <- renderText({
    coinDenom <- paste(input$coinDenom)
    paste("You denom: ", coinDenom)
  })
  
  
  
  
  coinDataRaw2 = reactive({
    userDateInput <- as.Date(input$dateRange, format = '%Y/%m/%d')
    userDateInput <- strftime(dat, '%Y%m%d')
    newCoinInput <- c(input$coinCheckChoice, input$coinDenom)

    denomPrice <- allCoinData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., open)
    coinDataRaw2 <- allCoinData %>% dplyr::mutate(., newOpen=(open/denomPrice$open))
     
    
    #  write.csv(coinDataRaw2, 'newRawData.csv')
    
    # denomPrice <- tempData %>% dplyr::filter(., symbol == input$coinDenom) %>% dplyr::select(., open)
    # coinDataRaw2 <- tempData %>% dplyr::mutate(., newOpen=(open/denomPrice$open))
    

    
  })
  
  
  
  #    write.csv(coinDataRaw2, 'newRawData.csv')
  #   tempData <- crypto_history(newCoinInput, start_date=userDateInput[1], end_date=userDateInput[2])    
  
  
  # coinDataRaw2$newOpen <- tempData %>% dplyr::mutate(., newOpen = (open/denomPrice$open))
  ###
 # tempData <- coinDataRaw3
  
  
  # denomPrice <- tempData %>% dplyr::filter(., symbol == 'BTC') %>% dplyr::select(., open)
  # coinDataRaw2 <- tempData %>% dplyr::mutate(., newOpen=(open/denomPrice$open))
  # 
  
  ####
  
  # coinDataRaw3 <- crypto_history('btc', start_date='20180101', end_date='20200101') 
  # coinDataRawTest <- coinDataRaw3
  # denomPrice <- coinDataRaw3 %>% dplyr::filter(., symbol == 'BTC') %>% dplyr::select(., open)
  # coinDataRaw3$newOpen=coinDataRaw3$open/denomPrice$open
  
  #coinDataRaw3 <- coinDataRaw3 %>% dplyr::mutate(., newOpen=open/denomPrice)
  
  
  output$distPlot <- renderPlot({
    dat <- as.Date(input$dateRange, format = '%d/%m/%Y')
    userDateInput <- strftime(dat, '%Y%m%d')
    makeGraph(input$coinCheckChoice, userDateInput[1], userDateInput[2], coinDataRaw2())
  }) 
  
  
  output$distPlotRatio <- renderPlot({
    dat <- as.Date(input$dateRange, format = '%d/%m/%Y')
    userDateInput <- strftime(dat, '%Y%m%d')
    makeGraphRatio(input$coinCheckChoice, userDateInput[1], userDateInput[2], coinDataRaw2())
  })
  
}




shinyApp(ui = ui, server = server)







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
