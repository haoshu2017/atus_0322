#######################################################
#### Data Visualization on American Time Use Survey####
# Time spent at work from 2003-2022
## Programmer: Haoshu Duan   ## Date: 10/12/23
##                           ## Updated: 12/12/23
## Propose: building a shiny app
#######################################################

## Set up ----

rm(list = ls())

library(haven)
library(tidyverse)
library(purrr)
library(summarytools)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(dslabs)
library(plotly)
library(rsconnect)


#setwd("/Users/haoshu/Desktop/atus/")
#getwd()


# data("us_contagious_diseases")
# disease <- us_contagious_diseases
# disease <- mutate(disease, percapita = count/(population/100000)) %>%
#              pivot_longer(cols = c(count, percapita), 
#              names_to = "data", values_to = "value")
# head(disease)
# Obtain grouped time-use data 

#data <-readRDS(paste0('data_save/', 'grouped_time.rds'))
data <-readRDS('grouped_time.rds') %>%
        mutate(category = case_when(category == 'wrk_edu' ~ 'work and eduction',
                                    category == 'self_care' ~ 'self care',
                                    category == 'child_care' ~ 'child care',
                                    category == 'adult_care' ~ 'care for adults',
                                    category == 'house_wrk' ~ 'housework',
                                    category == 'exec' ~ 'exercise',
                                    category == 'phone_fam' ~ 'calling family members',
                                    category == 'relig' ~ 'religious activities', 
                                    category == 'sed_act' ~'sedentary activities',
                                    category == 'soc_leis' ~ 'socializing leisure time',
                                    category == 'soc_serv' ~ 'volunteering services',
                                    category == 'travel' ~ 'traveling/communiting', 
                                    category == 'wrk_act' ~ 'work activities', 
                                    category == 'sleep' ~ 'sleep', 
                                    category == 'ind_leis' ~ 'lesisure time alone',
                                    TRUE ~ NA_character_))

head(data)
category_choice<-unique(data$category)

category_choice
## ui set-up

ui <- fluidPage(
        
        titlePanel("Time-use by category in the US 2003-2022"),
        sidebarLayout(
                sidebarPanel(
                        # inputs
                        selectizeInput("GenderInput", "Gender",
                                       choices = unique(data$gender),  
                                       selected="Female", multiple =FALSE), 
                        checkboxGroupInput("CategoryInput", "Category",
                                           choices = category_choice,
                                           selected = c("work and eduction", "housework")),
                        sliderInput("YearInput", "Year", min=2003, max=2022, 
                                    value=c(2003, 2022), sep=""),
                        # radioGroupButtons("dataInput", "Data",
                        #                   choiceNames = list("Count", "Per capita"),
                        #                   choiceValues = list("Minutes"))
                ),  
                
                mainPanel(
                        plotOutput("timeplot"),
                        br(), br(),
                        verbatimTextOutput("stats"), 
                        br(), br(),
                        plotlyOutput("distplot")
                ) 
        )   
)   


## Server set-up

server <- function(input, output) {
        
        d <- reactive({
                data %>%
                        filter(gender == input$GenderInput,
                               category %in% input$CategoryInput,
                               year >= input$YearInput[1],
                               year <= input$YearInput[2]
                               #data == input$dataInput
                               )
        }) 
        
        
        output$timeplot <- renderPlot({
                
                ggplot(d(), aes(x=year, y = minutes, color=category)) +
                        geom_line() + 
                        theme_bw() +
                        xlab("Year") +
                        ylab("Minutes") +
                        ggtitle("Time-use over time")
        })
        
        output$stats <- renderPrint({
                
                aggregate(minutes ~ category, data = d(), sum)
                
        })
        
        output$distplot <- renderPlotly({
                
                box <- plot_ly(d(), y = ~minutes,
                               color = ~category, type = "box")  %>%
                        layout(title = "Distribution of minutes over different years",
                               yaxis = list(title='Minutes'))
                
        })
        
}


# Create Shiny app
shinyApp(ui = ui, server = server)
