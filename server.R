library(shiny)
library(shinydashboard)
library(wordcloud)
library(SnowballC)
library(tm)
#detach(package:tm, unload=TRUE)
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(htmlwidgets)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(forcats)
library(methods)
library(rfm)
library(magrittr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(factoextra)



shinyServer(function(session,input, output) {
  
  output$min_ <- renderInfoBox({
    infoBox(title = "Transactions", 
            value = 4363,
            subtitle = "Transactions Count",
            fill = TRUE,color = "purple",
            icon("users")) 
    
  })
  
  # First fluid row and second infoBox
  output$max_ <- renderInfoBox({
    infoBox(title = "Product Count", 
            value = 2205,
            subtitle = "maximum value in dataset", fill = T, color = "purple",
            icon("thumbs-o-up")) 
    
  })
  
  # First fluid row and third infoBox
  output$sd_ <- renderInfoBox({
    infoBox(title = "Cities Count", 
            value = 22,
            icon("globe"),
            subtitle = "maximum value in dataset", fill = T, color = "purple") 
  })
#wordcloud code
  
    output$plott <- renderPlot({
      text <- readLines("C:/Users/home/Desktop/Rhishi_project/supermarket.txt")
      docs <- Corpus(VectorSource(text))
      inspect(docs)
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      docs <- tm_map(docs, content_transformer(tolower))
      docs <- tm_map(docs, removeNumbers)
      docs <- tm_map(docs, removeWords, stopwords("english"))
      docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
      docs <- tm_map(docs, removePunctuation)
      docs <- tm_map(docs, stripWhitespace)
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      head(d, 10)
      set.seed(1234)
      wordcloud(words = d$word,
                freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
      
      
    })
  

  observe({
    Market <- read.csv("C:/Users/home/Desktop/Rhishi_project/Market_Analysis.csv")
    Market <- Market %>%mutate(
      Description = as.character(Description),
      Country = as.character(Country),
      CustomerID = as.factor(CustomerID)
      #StockCode = as.factor(StockCode)
    )
    Market$Total_sales = Market$Quantity * Market$UnitPrice
    names(Market)
    str(Market)
    Market %>% is.na %>% colSums
    
    Market$InvoiceDate = as.character(Market$InvoiceDate, "%Y-%m-%d")
    str(Market)
    
    
    Market$InvoiceDate <- mdy(Market$InvoiceDate)
    
    Market$date <- format(Market$InvoiceDate, "%m/%d/%Y")
    Market$month <- format(Market$InvoiceDate, "%B")
    Market$week <- format(Market$InvoiceDate, "%A")
    Market %>% is.na %>% colSums
    head(Market)
    names(Market)
    str(Market)
  
#----------------------------------------------------------------------------------------------------    
#city wice sales    
  output$country_name <- renderPlot({
    
    top20_product_sales_city <- Market %>%
      filter (Country == input$select) %>%
      select(StockCode, Description, Total_sales)%>%
      group_by(StockCode, Description, Total_sales) %>%
      filter(rank(desc(Total_sales)) <= 20) %>%
      arrange(desc(Total_sales)) %>%
      distinct(top20_product_sales_city) %>%
      head(20)
    head(top20_product_sales_city,20)
    
    top20_product_sales_city %>% ggplot(aes(x= Description, y= Total_sales))+
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Description")+
      ylab("Sale of products")+
      ggtitle("Top 20 Product sales")
    
    
  })
  
  output$country_info <- renderPrint({
    Market$Total_sales = Market$Quantity * Market$UnitPrice
    top20_product_sales_city <- Market %>%
      filter (Country == input$select) %>%
      select(StockCode, Description, Total_sales)%>%
      group_by(StockCode, Description, Total_sales) %>%
      filter(rank(desc(Total_sales)) <= 20) %>%
      arrange(desc(Total_sales)) %>%
      distinct(top20_product_sales_city) %>%
      head(20)
    head(top20_product_sales_city,20)
    
  })
  
  
  output$country_all <- renderPlot({
    g <- ggplot(Market, aes(x = Country , y = StockCode)) +
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Cities")+
      ylab("Number of product sales")+
      ggtitle("Entire Year sales of products according to cities")
    plot(g)
    
  })
  
  output$country_all_sale <- renderPlot({
    
    #which contry having max sales
    
    ggplot(data=Market, aes(x=Country))+
      geom_bar(fill = "purple")+
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))+
      scale_y_continuous(labels = scales::format_format(scientific = FALSE))+
      ggtitle("Las Vegas shares the major customer base")+
      ylab("Supermarket Sales")+
      xlab("Cities")  
  })
  
  output$country_except <- renderPlot({
    p1 <- Market %>% filter (Country != "Las Vegas") %>%
      ggplot(aes(x=Country))+
      geom_bar( fill = "purple")+
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))+
      ggtitle("Customer base without Las Vegas")+
      ylab("frequency based on orders")
    p1
    
  })
#----------------------------------------------------------------------------------------------------
#month wise sale    
  output$all_month <- renderPlot({
    
    top20_product_sales <- Market %>%
      filter (month == input$var) %>%
      select(StockCode, Description, Total_sales)%>%
      group_by(StockCode, Description, Total_sales) %>%
      filter(rank(desc(Total_sales)) <= 20) %>%
      arrange(desc(Total_sales)) %>%
      distinct(top20_product_sales) %>%
      head(20)
    head(top20_product_sales,20)
    print(top20_product_sales)
    
    top20_product_sales %>% ggplot(aes(x= Description, y= Total_sales))+
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Description")+
      ylab("Sale of products")+
      ggtitle("Top 20 Product sales")
    
    
  })
  
  output$all_month_info <- renderPrint({
  
    top20_product_sales <- Market %>%
      filter (month == input$var) %>%
      select(StockCode, Description, Total_sales)%>%
      group_by(StockCode, Description, Total_sales) %>%
      filter(rank(desc(Total_sales)) <= 20) %>%
      arrange(desc(Total_sales)) %>%
      distinct(top20_product_sales) %>%
      head(20)
    head(top20_product_sales,20)
    print(top20_product_sales)
    
  })
  output$all_month_sale <- renderPlot({
    g <- ggplot(Market, aes(x = month , y = StockCode)) +
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Cities")+
      ylab("Number of product sales")+
      ggtitle("Entire Year sales of products according to Month")
    plot(g)
    
  })
  
  
  output$all_month_sale_total <- renderPlot({
    Market$Total_sales = Market$Quantity * Market$UnitPrice
    g <- ggplot(Market, aes(x = month , y = Total_sales)) +
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Cities")+
      ylab("Number of product sales")+
      ggtitle("Entire Year sales of products according to Month")
    plot(g)
    
  })
  

#------------------------------------------------------
#week wise sale  
  output$all_week <- renderPlot({

    top20_product_week <- Market %>%
      filter (week == input$wek) %>%
      select(StockCode, Description, Total_sales)%>%
      group_by(StockCode, Description, Total_sales) %>%
      filter(rank(desc(Total_sales)) <= 20) %>%
      arrange(desc(Total_sales)) %>%
      distinct(top20_product_week) %>%
      head(20)
    head(top20_product_week,20)
    print(top20_product_week)

    top20_product_week %>% ggplot(aes(x= Description, y= Total_sales))+
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Description")+
      ylab("Sale of products")+
      ggtitle("Top 20 Product sales")
    
  })

    
  output$all_week_info <- renderPrint({

    top20_product_week <- Market %>%
      filter (week == input$wek) %>%
      select(StockCode, Description, Total_sales)%>%
      group_by(StockCode, Description, Total_sales) %>%
      filter(rank(desc(Total_sales)) <= 20) %>%
      arrange(desc(Total_sales)) %>%
      distinct(top20_product_week) %>%
      head(20)
    head(top20_product_week,20)
    print(top20_product_week)

  })

  output$all_week_sale <- renderPlot({
    g <- ggplot(Market, aes(x = week , y = StockCode)) +
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Cities")+
      ylab("Number of product sales")+
      ggtitle("Entire Year sales of products according to Month")
    plot(g)
    
  })
  
  
  output$all_week_sale_total <- renderPlot({
    Market$Total_sales = Market$Quantity * Market$UnitPrice
    g <- ggplot(Market, aes(x = week , y = Total_sales)) +
      geom_bar(stat = 'identity', fill = "purple") +
      coord_flip() + theme_classic()+
      xlab("Cities")+
      ylab("Number of product sales")+
      ggtitle("Entire Year sales of products according to Month")
    plot(g)
    
  })
#--------------------------------------------------------------------------------------------------
    #code for Pie chart
    output$pie1 <- renderPlotly({
      Market$Total_sales = Market$Quantity * Market$UnitPrice
         p <- plot_ly(Market, labels = ~Country, values = ~Total_sales, type = 'pie') %>%
        layout(title = 'Supermarket Sales according cities in one Year',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
          p

    })

    output$pie2 <- renderPlotly({
      # Market$Total_sales = Market$Quantity * Market$UnitPrice
      top20_product_sales <- Market %>%
        select(StockCode, Description, Total_sales)%>%
        group_by(StockCode, Description, Total_sales) %>%
        filter(rank(desc(Total_sales)) <= 20) %>%
        arrange(desc(Total_sales)) %>%
        distinct(top20_product_sales) %>%
        head(20)
      head(top20_product_sales,20)


      p <- plot_ly(top20_product_sales,labels =~Description,  values = ~Total_sales, type = 'pie') %>%
        layout(title = 'Top 20  product Sales in one year',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p

      # p1 <- Market %>% filter (Country != "United Kingdom") %>%
      #   ggplot(aes(x=Country))+
      #   geom_bar( fill = "purple")+




    })


    output$pie3 <- renderPlotly({
      # Market$Total_sales = Market$Quantity * Market$UnitPrice
      # Market$InvoiceDate = as.character(Market$InvoiceDate, "%Y-%m-%d")
      # str(Market)
      # 
      # 
      # Market$InvoiceDate <- mdy(Market$InvoiceDate)
      # 
      # Market$date <- format(Market$InvoiceDate, "%m/%d/%Y")
      # Market$month <- format(Market$InvoiceDate, "%B")
      # Market$week <- format(Market$InvoiceDate, "%A")
      p <- plot_ly(Market, labels = ~month, values = ~Total_sales, type = 'pie') %>%
        layout(title = 'Supermarket Sales in every month',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p
    })

    output$pie4 <- renderPlotly({
      # Market$Total_sales = Market$Quantity * Market$UnitPrice
      # Market$InvoiceDate = as.character(Market$InvoiceDate, "%Y-%m-%d")
      # str(Market)
      # 
      # 
      # Market$InvoiceDate <- mdy(Market$InvoiceDate)
      # 
      # Market$date <- format(Market$InvoiceDate, "%m/%d/%Y")
      # Market$month <- format(Market$InvoiceDate, "%B")
      #Market$week <- format(Market$InvoiceDate, "%A")
      p <- plot_ly(Market, labels = ~week, values = ~Total_sales, type = 'pie') %>%
        layout(title = 'Supermarket Sales in every day',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p
    })
 })

#-----------------------------------------------------------------------------------------------------
#basket analysis  
    output$FrequencyPlot <- renderPlot({

    Market <- read.csv("C:/Users/home/Desktop/Rhishi_project/Market_Analysis.csv")
    Market <- Market[complete.cases(Market), ]
    Market %>% mutate(Description = as.factor(Description))
    Market %>% mutate(Country = as.factor(Country))
    InvoiceNo <- as.numeric(as.character(Market$InvoiceNo))

    #cbind(Market,InvoiceDate)
    cbind(Market,InvoiceNo)

    library(plyr)
    Market_Basket <- ddply(Market,c("InvoiceNo","InvoiceDate"),
                             function(df1)paste(df1$Description,
                                                collapse = ","))

    Market_Basket$InvoiceNo <- NULL
    Market_Basket$InvoiceDate <- NULL
    colnames(Market_Basket) <- c("items")
    Market_Basket


    write.csv(Market_Basket,"C:/Users/home/Desktop/Rhishi_project/Market_basket.csv", quote = FALSE, row.names = FALSE)

    tr <- read.transactions('C:/Users/home/Desktop/Rhishi_project/Market_basket.csv', format = 'basket', sep=',')
    tr

    if (!require("RColorBrewer")) {
      # install color package of R
      install.packages("RColorBrewer")
      #include library RColorBrewer
      library(RColorBrewer)
    }


    #first plot

    itemFrequencyPlot(tr,topN=20,type="absolute", main="Item Frequency Plot")

  })

  output$Association_Rules <- renderPrint({
    tr <- read.transactions('C:/Users/home/Desktop/Rhishi_project/Market_basket.csv', format = 'basket', sep=',')
    association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
    summary(association.rules)
    #inspect(association.rules[1:10])

  })

  output$Top_Rules <- renderPrint({
    tr <- read.transactions('C:/Users/home/Desktop/Rhishi_project/Market_basket.csv', format = 'basket', sep=',')
    detach(package:tm, unload=TRUE)
    association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
    inspect(association.rules[1:10])
  })

  output$Plot_Association_rules <- renderPlot({
    tr <- read.transactions('C:/Users/home/Desktop/Rhishi_project/Market_basket.csv', format = 'basket', sep=',')
     association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
     plot(association.rules,method = "graph")
     # plot(association.rules,method = "graph", interactive = T)
  })

  output$Plot_Ten_Association_rules <- renderPlot({
    #4th plot
    tr <- read.transactions('C:/Users/home/Desktop/Rhishi_project/Market_basket.csv', format = 'basket', sep=',')
    association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
    top10association.rules<- head(association.rules, n = 10, by = "confidence")
    plot(top10association.rules, method = "graph")
    plot(top10association.rules, method = "graph",  interactive = T)
  })

  output$Linking_Of_rules <- renderPlot({
    #5th plot
    tr <- read.transactions('C:/Users/home/Desktop/Rhishi_project/Market_basket.csv', format = 'basket', sep=',')
    association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
    top10association.rules<- head(association.rules, n = 10, by = "confidence")
    plot(top10association.rules, method = "graph",  engine = "htmlwidget")
  })

#----------------------------------------------------------------------------------------------------
#RFM analysis
  output$mytable <- DT::renderDataTable({
          

  Market <- read.csv('C:/Users/home/Desktop/Rhishi_project/Market_Analysis.csv')
  Market$Total_sales = Market$Quantity * Market$UnitPrice

  head(Market)
  names(Market)
  Market$StockCode <- NULL
  Market$Description <- NULL
  Market$Quantity <- NULL
  Market$UnitPrice <- NULL
  Market$InvoiceNo <- NULL
  Market$Country <- NULL

  Market
  str(Market)
  #Market$InvoiceDate = as.character(Market$InvoiceDate, "%Y-%m-%d")
  Market$InvoiceDate <- mdy(Market$InvoiceDate)
  #InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y')
  str(market)

  ## ----rfm table-----------------------------------------------------------
  analysis_date <- lubridate::as_date("2016-01-01", tz = "UTC")
  rfm_resultt    <- rfm_table_order(Market, CustomerID, InvoiceDate, Total_sales, analysis_date)
  rfm_resultt

  ## ----segments------------------------------------------------------------
  segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                     "New Customers", "Promising", "Need Attention", "About To Sleep",
                     "At Risk", "Can't Lose Them", "Hibernating", "Lost")

  recency_lower   <- c(4, 2, 3, 4, 3, 3, 2, 1, 1, 2, 1)
  recency_upper   <- c(5, 4, 5, 5, 4, 4, 3, 2, 1, 3, 1)
  frequency_lower <- c(4, 3, 1, 1, 1, 3, 1, 2, 4, 2, 1)
  frequency_upper <- c(5, 4, 3, 1, 1, 4, 2, 5, 5, 3, 1)
  monetary_lower  <- c(4, 4, 1, 1, 1, 3, 1, 2, 4, 2, 1)
  monetary_upper  <- c(5, 5, 3, 1, 1, 4, 2, 5, 5, 3, 1)

  segments <- rfm_segment(rfm_resultt, segment_names, recency_lower, recency_upper,
                          frequency_lower, frequency_upper, monetary_lower, monetary_upper)

  segments
  })
  
  output$Cust_count <- renderPrint({
    
  segments %>%
    dplyr::count(segment) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::rename(Segment = segment, Count = n)
    
  })
  output$rfm_plot_median_recency <- renderPlot({
    
    rfm_plot_median_recency(segments)
    
  })

    output$rfm_plot_median_frequency <- renderPlot({
      
      rfm_plot_median_frequency(segments)

    })
    output$rfm_plot_median_monetary <- renderPlot({
      
      rfm_plot_median_monetary(segments)
      
    })
    output$heatmap <- renderPlot({
      
      rfm_heatmap(rfm_resultt)
      
    })
#-----------------------------------------------------------------------------------------
#top customers
        output$topfrequent <- renderPlot({
      segments
      
      segments <- segments %>%mutate(
        customer_id = as.factor(customer_id)
      )
      
      top20_frequent_customer <- segments %>%
        select(customer_id, transaction_count)%>%
        group_by(customer_id, transaction_count) %>%
        filter(rank(desc(transaction_count)) <= 20) %>%
        arrange(desc(transaction_count)) %>%
        #distinct(top20_frequent_customer) %>%
        head(20)
      head(top20_frequent_customer,20)
      
      
      ggplot(data=top20_frequent_customer, aes(x=customer_id, y=transaction_count, group=1)) +
        geom_line(linetype = "dashed",color="purple", size=1)+
        geom_point()
      
      
    })
    
    output$frequency_summary <- renderPrint({
      top20_frequent_customer <- segments %>%
        select(customer_id, transaction_count)%>%
        group_by(customer_id, transaction_count) %>%
        filter(rank(desc(transaction_count)) <= 20) %>%
        arrange(desc(transaction_count)) %>%
        #distinct(top20_frequent_customer) %>%
        head(20)
      head(top20_frequent_customer,20)
    })
    
    
    output$toptransation <- renderPlot({
      
      top20_highsales_customer <- segments %>%
        select(customer_id, amount)%>%
        group_by(customer_id, amount) %>%
        filter(rank(desc(amount)) <= 20) %>%
        arrange(desc(amount)) %>%
        #distinct(top20_highsales_customer) %>%
        head(20)
      head(top20_highsales_customer,20)
      
      ggplot(data=top20_highsales_customer, aes(x=customer_id, y=amount, group=1)) +
        geom_line(linetype = "dashed",color="purple", size=1)+
        geom_point()+
        scale_y_continuous(labels = scales::format_format(scientific = FALSE))
      
    })
    output$revenue_summary <- renderPrint({
      top20_highsales_customer <- segments %>%
        select(customer_id, amount)%>%
        group_by(customer_id, amount) %>%
        filter(rank(desc(amount)) <= 20) %>%
        arrange(desc(amount)) %>%
        #distinct(top20_highsales_customer) %>%
        head(20)
      head(top20_highsales_customer,20)
    })
    
    
#-----------------------------------------------------------------------------------------    
#clustering 
       output$Scree <- renderPlot({
      segments
      o_segments = segments
      o_segments$customer_id <- NULL
      o_segments$segment <- NULL
      o_segments$date_most_recent <- NULL
      o_segments$transaction_count <- NULL
      o_segments$recency_days <- NULL
      o_segments$amount <- NULL
      o_segments <- scale(o_segments[-1])
      
      wss <- (nrow(o_segments)-1)*sum(apply(o_segments,2,var))
      for(i in 2:20) wss[i] <- sum(kmeans(o_segments,centers=i)$withinss)
      plot(1:20,wss, type="b", xlab = "numbers of clusters", ylab = "within the group ss")
      
    })
    
    output$Kmens <- renderPrint({
      kmeans2 <- kmeans(o_segments, centers = 6, nstart = 25)
      kmeans2
    })
    
    output$Kmens_table <- renderPrint({
      table(segments$segment,kmeans2$cluster)
    })
    
    output$fviz_cluster <- renderPlot({
      fviz_cluster(kmeans2, data = o_segments)
    }) 
    
    output$customer_allocation <- renderPrint({
      rhishi <- cbind(segments,cluster = kmeans2$cluster)
      rhishi$transaction_count <- NULL
      rhishi$recency_days <- NULL
      rhishi$amount <- NULL
      rhishi$date_most_recent <- NULL
      rhishi$recency_score <- NULL
      rhishi$frequency_score <- NULL
      rhishi$monetary_score <- NULL
      rhishi
      
    })
    
    
    
})
