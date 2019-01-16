# Set working directory
setwd("C:\\Users\\jeeva\\Documents\\BANA\\Career Management\\Amend Usecase\\")

# Loading required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(caret)
library(shinydashboard)
library(shiny)
library(h2o)
library(plotly)
library(scales)
h2o.init()

# Read data
amend_data <- read.csv("Amend_use_case.csv", header = TRUE, stringsAsFactors = FALSE)

# Reading model object
h2o_gbm_final <- h2o.loadModel("GBM_model_R_1547057987212_4")

# Defining columns
amend_data$unit_price <- ifelse(amend_data$Last.Unit.Cost == 0, amend_data$Original.Unit.Cost, 
                                amend_data$Last.Unit.Cost)
amend_data$final_cost <- amend_data$Received.Quantity * amend_data$unit_price

amend_data$order_date2 <- as.Date(amend_data$Order.Date, format = "%m/%d/%Y")

amend_data$month <- as.Date(cut(amend_data$order_date2,breaks = "month"))

# Defining vectors
item_codes = unique(amend_data$Item.Code.Acronym)
vendor_names = unique(amend_data$Vendor.Name)
species_names = unique(amend_data$Species)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Stairtek Price Prediction Dashboard", 
                          titleWidth = 450)  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    numericInput(inputId = "quantity", 
                 label = "Quantity", 
                 value = 100,
                 min = 1, 
                 max = 100000, 
                 step = 1), 
    selectInput(inputId = "item_code", 
                label = "Item Code",
                choices = item_codes, 
                selected = item_codes[1]),
    selectInput(inputId = "vendor_name", 
                label = "Vendor Name",
                choices = vendor_names, 
                selected = vendor_names[1]),
    selectInput(inputId = "species_name", 
                label = "Species Name",
                choices = species_names, 
                selected = species_names[1]),
    dateInput(inputId = "order_date", 
              label = "Order Date"
    ),
    dateInput(inputId = "receipt_date", 
              label = "Expected Receipt Date"
    )
    
  )
)

frow0 <- fluidRow(
  
  box(title = "Predicted Price for Selection (in Dollars)"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
  ,verbatimTextOutput("cost_output")),
  
  box(title = "Predicted Price Per Unit for Selection (in Dollars)"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,verbatimTextOutput("ppu_output"))

)  

frow1 <- fluidRow(
  
  box(
    title = "Quantity ordered historically for Selection"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("item_quan_hist", height = "300px")
  ),
  
  box(
    title = "Historical Price Per Unit for Selection"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("pp_quan_hist", height = "300px")
  )
  
)

frow2 <- fluidRow(
  
  box(
    title = "Additional Insight 1"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("species_ppq", height = "300px")
  ),
  
  box(
    title = "Additional Insight 2"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("manuf_ppq", height = "300px")
  )
)



# combine the two fluid rows to make the body
body <- dashboardBody(
  
  shinyUI(fluidPage(frow0 , frow1, frow2))

)

#completing the ui part with dashboardPage

#ui <- dashboardPage(column(3,offset = 4, titlePanel("How to Centre Me??")), header, sidebar, body, skin='red')

#ui <- dashboardPage(title = 'Stairtek Price Variation', header, sidebar, body, skin='red')

ui <- dashboardPage(title = 'Stairtek Price', header, sidebar, body, skin='red')



# create the server functions for the dashboard  
server <- function(input, output) { 
  
  # Predicting final cost
predict_df <- reactive ({data.frame( 
    Vendor.Name = as.factor(input$vendor_name),
    Species = as.factor(input$species_name),
    Item.Code.Acronym = as.factor(input$item_code),
    Received.Quantity = as.numeric(input$quantity),
    days_diff = as.numeric(as.Date(input$receipt_date) - as.Date(input$order_date))  
  )
})

#predict_df$Vendor.Name <- as.factor(predict_df$Vendor.Name)
#predict_df$Species <- as.factor(predict_df$Species)
#predict_df$Item.Code.Acronym <- as.factor(predict_df$Item.Code.Acronym)
#predict_df$Received.Quantity  <- as.numeric(predict_df$Received.Quantity)
#predict_df$days_diff <- as.numeric(predict_df$days_diff)

# Predicting for given test data
pred_h2o <- reactive({as.h2o(predict_df())})
predict_output <- reactive({h2o.predict(h2o_gbm_final, pred_h2o())})


output$cost_output <- renderText({

  print(round(as.vector(predict_output()),0))
    
})


output$ppu_output <- renderText({
  
  print(round((as.vector(predict_output()) / as.numeric(input$quantity)),0))
  
})



  #creating the plotOutput content
  output$item_quan_hist <- renderPlotly({
    
    amend_data %>%
      filter(Item.Code.Acronym == input$item_code & 
             Vendor.Name == input$vendor_name & 
             Species == input$species_name) %>%
      group_by(month) %>%
      summarise(total_quan = sum(Received.Quantity, na.rm = TRUE)) %>%
      ggplot(aes(x = month, y = total_quan, group = 1)) +
      geom_line() +
      geom_point() + 
      scale_x_date(labels = date_format("%Y-%m"),breaks = "2 month") +
      scale_y_continuous(name = "Total Quantity", labels = scales::comma) +
      labs(x = "Month", title = "Quantity by month")
  })
  
  output$pp_quan_hist <- renderPlotly({
    
    amend_data %>%
      filter(Item.Code.Acronym == input$item_code & 
               Vendor.Name == input$vendor_name & 
               Species == input$species_name) %>%
      group_by(month) %>%
      summarise(price_per_quan = sum(final_cost, na.rm = TRUE) / sum(Received.Quantity, na.rm = TRUE)) %>%
      ggplot(aes(x = month, y = price_per_quan, group = 1)) +
      geom_line() +
      geom_point() + 
      scale_x_date(labels = date_format("%Y-%m"),breaks = "2 month") +
      scale_y_continuous(name = "Price Per Quantity", labels = scales::comma) +
      labs(x = "Month", title = "Price Per Quantity by month")

  })
  
  
  output$species_ppq <- renderPlotly({
    
    amend_data %>%
      filter(Species == input$species_name) %>%
      group_by(month) %>%
      summarise(price_per_quan = sum(final_cost, na.rm = TRUE) / sum(Received.Quantity, na.rm = TRUE)) %>%
      ggplot(aes(x = month, y = price_per_quan, group = 1)) +
      geom_line() +
      geom_point() + 
      scale_x_date(labels = date_format("%Y-%m"),breaks = "2 month") +
      scale_y_continuous(name = "Price Per Quantity ", labels = scales::comma) +
      labs(x = "Month", title = "Price Per Quantity for selected Species")
    
  })
  
  
  output$manuf_ppq <- renderPlotly({
    
    amend_data %>%
      filter(Vendor.Name == input$vendor_name) %>%
      group_by(month) %>%
      summarise(price_per_quan = sum(final_cost, na.rm = TRUE) / sum(Received.Quantity, na.rm = TRUE)) %>%
      ggplot(aes(x = month, y = price_per_quan, group = 1)) +
      geom_line() +
      geom_point() + 
      scale_x_date(labels = date_format("%Y-%m"),breaks = "2 month") +
      scale_y_continuous(name = "Price Per Quantity ", labels = scales::comma) +
      labs(x = "Month", title = "Price Per Quantity for selected Vendor")
    
  })
  
  

}


shinyApp(ui, server)






