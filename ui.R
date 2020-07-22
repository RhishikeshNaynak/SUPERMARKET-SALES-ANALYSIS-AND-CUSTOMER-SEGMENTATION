library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(ggpubr)
library(plotly)
library(ggplot2)
library(DT)

library(arules)
library(arulesViz)
library(readxl)
library(knitr)
library(lubridate)
library(plyr)
library(dplyr)


suppressWarnings(as.numeric("test"))

shinyUI(
  dashboardPage(title = "Supermarket Sales Analysis And Customer Segmentation", skin = "purple",
    dashboardHeader(title = "Supermarket Sales Analysis And Customer Segmentation "),
                    
    
    dashboardSidebar(
      
      sidebarMenu(
      sidebarSearchForm("searchText","buttonsearch","search"),
      menuItem("DASHBOARD", tabName = "DASHBOARD",icon = icon("dashboard")),
      menuItem("Product_Sales_Country", tabName = "Product_Sales_Country",icon = icon("bar-chart-o")),
      menuItem("Product_Sales_Month", tabName = "Product_Sales_Month",icon = icon("signal")),
      menuItem("Product_Sales_WeekDay", tabName = "Product_Sales_WeekDay",icon = icon("users")),
      menuItem("Pie_Chart", tabName = "Pie_Chart",icon = icon("pie-chart")),
      menuItem("RFM_Segmentation", tabName = "RFM_Segmentation",icon = icon("database"), badgeLabel = "New",badgeColor = "green"),
      menuItem("Customers", tabName = "Customers",icon = icon("user")),
      menuItem("Clustaring", tabName = "Clustaring",icon = icon("users")),
      menuItem("Basket_Analysis", tabName = "Basket_Analysis", icon = icon("shopping-basket"))
     
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "DASHBOARD",
        h2("SUPERMARKET SALES ANALYSIS AND CUSTOMER SEGMENTATION",
           style="color:purple; font-family:Cambria; font-weight:bold;
           font-size:50px"),
        
        
        
        fluidRow(infoBoxOutput("min_", width = 3),
                 infoBoxOutput("max_", width = 3),
                 infoBoxOutput("sd_", width = 3) 
                 #infoBoxOutput("mean_", width=3)
                 ),
    
        
        plotOutput("plott",height = "580px")
        
      ),
        
      
           
                tabItem(tabName ="Product_Sales_Country",
                        h2("City wise product Sales Analysis dashboard",
                           style="color:purple; font-family:Cambria; font-weight:bold;text-transform: uppercase; font-size:50px"),
                                                                     
                        tabBox(id="tb1",width = '600px',height = '1800px',
                               
                           tabPanel("Supermarket sales of product in each city",
                                    
                                    fluidRow(column(width = 12,align = "left",box(status = "primary", background = 'purple',
         
                                  selectInput("select", "Select Country From the year 2015", width = "700px",
                                          choices = list("Select input Country","Atlanta","Austin","Baltimore","Boston","Chicago","Dallas","Denver","Detroit","Houston",
                                                "Indianapolis","Las Vegas","Los Angeles","Miami","Minneapolis","Nashvile","New York","Phoenix",
                                                   "Portland","San Antonio","San Diego","San Francisco","San Jose","Seattle","Washington")
                                    )))),
                                        
                             plotOutput("country_name",width="1000",height = "500px"),
                              h2("Information about top 20  sales"),
                             verbatimTextOutput("country_info")
             ),
               tabPanel("Total Supermarket sales of product in all cities",
                        
                      h2("Analysis about top 20  sales across all the cities :- With Product count"),
                      plotOutput("country_all",width="1000",height = "500px"),
                      h2("Supermarket sales across all the cities :- With Product sale"),
                      plotOutput("country_all_sale",width="1000",height = "500px"),
                      h2("Supermarket sales across all the cities :- With Product sale != Las Vegas"),
                      plotOutput("country_except",width="1000",height = "500px")
             )
          )
           
      ),
                
      tabItem(tabName ="Product_Sales_Month",
              h2("Monthly product Sales Analysis dashboard",
                 style="color:purple; font-family:Cambria; font-weight:bold; text-transform: uppercase; font-size:50px"),
              tabBox(id="Pie_Chart1",width = '600px',height = '1800px',
                     tabPanel("Total Supermarket sales of product in each seprate Month",
                              fluidRow(column(width = 12,align = "left",box(status = "primary", background = 'purple',
                              selectInput("var", "Select Country From the year 2015", width = "700px",
                                          choices = list("Select input month","January" ,"February",
                                                         "March","April",
                                                         "May", "June",
                                                         "July","August",
                                                         "September","October",
                                                         "November","December")
                              )))),
                              
                              plotOutput("all_month",width="1000",height = "500px"),
                              h2("Top 20 Sales according to months"),
                              verbatimTextOutput("all_month_info")
                     ),
                     tabPanel("Total Supermarket sales of product in all Months",
                              h2("Analysis about top 20  product sales across all the Months :- With Stock count"),
                              plotOutput("all_month_sale",width="1000",height = "500px"),
                              h2("Supermarket sales of product across all the Months :- With Total Sale"),
                              plotOutput("all_month_sale_total",width="1000",height = "500px")
                              # h2("Supermarket sales across all the cities"),
                              # plotOutput("country_except",width="1000",height = "500px")
                     )
              )
              
      ),
      
      tabItem(tabName ="Product_Sales_WeekDay",
              h2("Weekly product Sales Analysis dashboard",
                 style="color:purple; font-family:Cambria; font-weight:bold; text-transform: uppercase; font-size:50px"),
              tabBox(id="Pie_Chart1",width = '600px',height = '1800px',
                     tabPanel("Total Supermarket sales of product in Overall week",
                              fluidRow(column(width = 12,align = "left",box(status = "primary", background = 'purple',
                                                                            selectInput("wek", "Select Week From the year 2015", width = "700px",
                                                                                        choices = list("Select input Day","Sunday","Monday","Tuesday", "Wednesday",
                                                                                                       "Thursday", "Friday", "Saturday")
                                                                            )))),

                              plotOutput("all_week",width="1000",height = "500px"),
                              h2("Top 20 Sales according to week"),
                              verbatimTextOutput("all_week_info")
                     ),
                     tabPanel("Total Supermarket sales of product in all Weeks",
                              h2("Analysis about top 20  product sales across all the Week :- With Stock count"),
                              plotOutput("all_week_sale",width="1000",height = "500px"),
                              h2("Supermarket sales of product across all the Week :- With Total Sale"),
                              plotOutput("all_week_sale_total",width="1000",height = "500px")
                              # h2("Supermarket sales across all the cities"),
                              # plotOutput("country_except",width="1000",height = "500px")
                     )
              )

      ),
      
         
        tabItem(tabName = "Pie_Chart",
                h2("Pie_Chart Analysis dashboard",
                   style="color:purple; font-family:Cambria;text-transform: uppercase; font-weight:bold; font-size:50px"),
                tabBox(id="Pie_Chart1",width = '400px',height = '600px',
                       tabPanel("Sales In City",
                                h2("Analysis about Yearly sales across all the Cities"),
                                plotlyOutput("pie1")),
                       tabPanel("Product Sale ",
                                h2("Analysis about product sales across all the Cities"),
                                plotlyOutput("pie2")),
                       tabPanel("Monthly Sale",
                                h2("Analysis about Montly sales in year"),
                                plotlyOutput("pie3")),
                       tabPanel("Weekly Sale", 
                                h2("Analysis about Weekly sales in year"),
                                plotlyOutput("pie4"))

                )
        ),
                
        tabItem(tabName = "RFM_Segmentation",
             h2("RFM_Segmentation",
                style="color:purple; font-family:Cambria;text-transform: uppercase; font-weight:bold; font-size:50px"),
             h2("RFM Data table for analysis of Customer Segmentation",
                style="color:purple; font-family:Cambria; text-transform: uppercase; font-weight:bold; font-size:50px"),
             DT::dataTableOutput("mytable"),
             h2("Count of Customer segmentation"),
             verbatimTextOutput("Cust_count"),
             h2("R = Recency plot based on RFM data table"),
              plotOutput("rfm_plot_median_recency",width = '1400px',height = '500px'),
             h2("F = Frequency plot based on RFM data table"),
              plotOutput("rfm_plot_median_frequency",width = '1400px',height = '500px'),
             h2("M = Monetary plot based on RFM data table"),
             plotOutput("rfm_plot_median_monetary",width = '1400px',height = '500px'),
             h2("Heatmap based on RFM data table"),
             plotOutput("heatmap",width = '1400px',height = '500px')
                ),
      
      tabItem(tabName = "Customers",
              h2("top 20 customers",
                 style="color:purple; font-family:Cambria; font-weight:bold; text-transform: uppercase; font-size:50px"),
              tabBox(id="tabchart1",width = '600px',
                     tabPanel("Most frequent Customer",
                              h2("The Most Frequent Customers"),
                              plotOutput("topfrequent"),
                              h2("Summary Of Customers"),
                              verbatimTextOutput("frequency_summary")
                              ),
                     tabPanel("Customer with heigh revenue",
                              h2("The customer With heigh revenue"),
                              plotOutput("toptransation"),
                              h2("Summary Of Customers"),
                              verbatimTextOutput("revenue_summary")
                              )
                  
              ) 
      ),
      
 
      tabItem(tabName = "Clustaring",
              h2("Clustering based on Customers",
                 style="color:purple; font-family:Cambria; font-weight:bold;text-transform: uppercase; font-size:50px"),
              h2("Based on RFM data we are doing Clustering with K-means",
                 style="color:purple; font-family:Cambria;text-transform: uppercase; font-weight:bold; font-size:50px"),
              h2("To Identify how many Cluster in dadaset"),
              plotOutput("Scree",width = '1400px',height = '500px'),
              h2("Kmeans clustering information "),
              verbatimTextOutput("Kmens"),
              h2("Kmeans clustering Membership table "),
              verbatimTextOutput("Kmens_table"),
              h2("Cluster plot based on fviz_cluster function"),
              plotOutput("fviz_cluster",width = '1400px',height = '500px'),
              h2("Which Customer under which Cluster"),
              verbatimTextOutput("customer_allocation")
            
              
              
      ),
        tabItem(tabName = "Basket_Analysis",
                h2("Basket Analysis Dashboard",
                   style="color:purple; font-family:Cambria; font-weight:bold; text-transform: uppercase; font-size:50px"),
                plotOutput("FrequencyPlot",width = '1400px',height = '500px'),
                h2("Association Rules"),
                verbatimTextOutput("Association_Rules"),
                h2("Top 10 Association Rules"),
                verbatimTextOutput("Top_Rules"),
                h2("Plot Association Rules"),
                plotOutput("Plot_Association_rules",width = '1400px',height = '700px'),
                h2("Plot Top 10 Association Rules"),
                plotOutput("Plot_Ten_Association_rules",width = '1400px',height = '700px'),
                fluidRow(plotOutput("Linking_Of_rules")))
               
        )
      )
    )
  )  
  
      


      


    
  
 



