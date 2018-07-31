## Crime Analytics Dashboard - UI
## Author: SOLAI MURUGAN V 2018

## Usage:

## This file handles all the user interface of the Shiny app.
## The app consists in a navigation bar with two tabs: a map and a dashboardx .


## Required libraries
#install.packages("colourpicker")
library(shiny)
library(colourpicker)
library(dplyr)
library(ggplot2)
library(magrittr)
#install.packages('leaflet')
library(leaflet)
library(readr)
#install.packages("shinycssloaders")
library(shinycssloaders)
#library(Cairo)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

city_info <- read_tsv('./data/city_data.tsv')
state_list <- sort(append(unique(city_info$state),"ALL"))
city_list <- sort(append(unique(city_info$real_name),"ALL"))
city_comparison <- sort(unique(city_info$real_name))



#Added foe Age compariison 11/03/2018
women_data <- read_csv('./data/paCAW.csv')
colnmaes_crime <- colnames(women_data)
male <- grep("\\Male\\b", colnmaes_crime)
colnmaes_crime[male]
#end 



shinyUI(
  navbarPage(
    header = tags$style(type="text/css", "@import url('//fonts.googleapis.com/css?family=Bungee'); .navbar-brand {font-family: 'Bungee', cursive; font-size: 15pt; font-weight: 500;color: #ad1d28;}"),
    title = "Crime Analytics",
    inverse = TRUE,
    ## Set the navbar structure
    ## Code for the Overview tab.
    tabPanel("Overview",icon = icon("map", lib = "font-awesome"),
            hr(),
            fluidRow(
              column(4,
                     selectInput("crimeInput", "Crime Type",
                                 choices = c("All", "Homicide", "Rape","Robbery","Aggravated Assault")),
                     checkboxInput("relCheckbox", "Relative Statistics", value = TRUE),
                     p("*Relative means that the crime indicators are normalized on a base of 100,000 people")),
              column(4,
                     sliderInput("yearInput", 
                                 label = "Year",
                                 min = 1995, max = 2015, value = 2014,step=1,animate=TRUE,sep="")
                     ),
              column(4,
                     uiOutput("stateControls"),
                     selectInput("stateInput", "State",
                                 choices = state_list))
            ),
            hr(),
            fluidRow(
              column(8,
                     leafletOutput("mymap",height = "450px")),
              column(width=4,
                     fluidRow(withSpinner(plotOutput("bar_overview_1",height = "225px"))),
                     fluidRow(withSpinner(plotOutput("bar_overview_2",height = "225px"))))),
            hr()),
    
    ## Code for the Comparison tab.
    tabPanel("Comparison",icon = icon("bar-chart-o"),
            hr(),
            fluidRow(
              column(4,
                     selectInput("cityInput1", "City 1",
                                 choices = city_comparison,
                                 selected = "Boston")),
              column(4,
                     selectInput("cityInput2", "City 2",
                                 choices = city_comparison,
                                 selected = "New York")),
              column(4,
                     checkboxInput("forCheckbox", "Include Forecast", value = TRUE),
                     p("This is a 1 year forecast using a 3 year moving average. Only applies for 2016. It's represented by diamonds in the plot."))
            ), 
            hr(),
            fluidRow(
              column(12,
                     uiOutput("kpi1")
                     )
            ),
            hr(),
            fluidRow(
              splitLayout(cellWidths = c("50%"), withSpinner(plotOutput("distPlot1")),withSpinner(plotOutput("distPlot2")))
            ),
            hr(),
            fluidRow(
              splitLayout(cellWidths = c("50%","50%"), tableOutput("table1"),tableOutput("table2"))
            )
    ),
    #Added foe Age compariison 11/03/2018
    tabPanel("Age awise",icon = icon("bar-chart-o"),
             hr(),
             fluidRow(
               column(4,
                      selectInput("maleInput1", "Male convicted",
                                  choices = colnmaes_crime[male],
                                  selected = colnmaes_crime[male][1]))
               
               
             ), hr(),
             
             fluidRow(
               plotOutput("distPlotage1")
             )
                    
             ,hr(),
             fluidRow(
             tableOutput("crimetable1"))
             ) 
    
    
    
))