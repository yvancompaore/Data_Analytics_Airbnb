library(shiny)
library(stringr)
library(shinydashboard)
library(ggplot2) 
library(dplyr)
library(DT)
library(data.table)
library(leaflet)


shinyUI(
    dashboardPage(
        dashboardHeader(title='Air bnb Project'),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Introduction",tabName = "dashboard", icon = icon("list")),
                menuItem("Analysis 1 - Comparing Cities" ,icon = icon("dashboard"),
                         menuSubItem("One feature" ,tabName = "Comparaison",icon = icon("line-chart")),
                         menuSubItem("New Dimensions" ,tabName = "NewDimensions",icon = icon("chart-bar"))
                ),
                menuItem("Analysis 2 – Deep dive into a city ",tabName = "Map",icon = icon("dashboard")),
                menuItem("Data", tabName = "data", icon = icon("database")),
                menuItem("About us", tabName = "aboutus", icon = icon("user-circle"))
            )),
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                        fluidRow(
                            column(8, align="center", offset = 2,
                                   tags$img(src="air.jpg",height=250,width=350),
                            )),
                        br(),
                        fluidRow(
                            box(title = "Analysis 1 : Comparing Cities", width = 8, solidHeader = TRUE, status = "warning",
                            h4(" The user should be able to :"),
                            div("- Select the cities he would like to compare among the list of cities you already have"),
                            div("- Select a feature that he would like to compare (availability over last 30 days,revenue, price …)."),
                            div("- Select the aggregation type / plot type (average, median, histogram, density, boxplot…"),
                            div("- Have more granularity to your insights by allowing for the possibility of addition of
                                 new dimensions to the plot (room type / no bedrooms, neighborhood…), so that your
                                 plot can now plot the histogram of revenue over both the different cities and
                                 different room types for instance."),
                            )),
                        fluidRow(
                            box(title = "Analysis 2 : Deep dive into a city", width = 4, solidHeader = TRUE, status = "warning",
                            h4(" The user should be able to :"),
                            div("- Select the city he would like to analyze."),
                            div("- Display the finer grained analysis."),
                            div("- Display a map of listings in the selected city."),
                            )
                )),
                tabItem(tabName = "Comparaison",
                        box(h3("Comparaison with one feature"), width = 8, background = "blue"),
                        fluidRow( 
                            selectInput("city", "Select the city", c("barcelona","berlin","bordeaux","euskadi","girona","lyon","madrid","malaga","mallorca","menorca","munich","paris","sevilla","valencia"),multiple = TRUE),
                            box(title = "Graph", width = 6, solidHeader = TRUE, status = "warning",plotOutput("histogram1")),
                            box(title = "Feature / Type of plot", width = 4, solidHeader = TRUE, status = "warning",
                                selectInput("cat", "Select the category", c("property_type", "room_type" , 'bedrooms' ,
                                                                        "beds", "price","neighbourhood_cleansed","availability_30", "price_30", "revenue_30"
                                                                        )),
                                selectInput("typePlot1", "Select the plot type", c("histogram"="geom_bar","density"="geom_density","boxplot"="geom_boxplot")),
                                dateRangeInput(
                                    inputId = "daterange",
                                    label = "Select the date", 
                                    start = "2020-04-30",
                                    end  =  "2020-09-19",
                                    min  =  "2020-04-30",
                                    max  =  "2020-09-19",
                                    format = "yyyy/mm/dd",
                                    separator = " - "
                                ),
                                dataTableOutput('tableavg')
                                ),
                        )
                ),
                tabItem(tabName = "NewDimensions",
                        box(h3("Comparaison with two features"), width = 5, background = "blue"),
                        selectInput("citydim", "Select the city", c("barcelona","berlin","bordeaux","euskadi","girona","lyon","madrid","malaga","mallorca","menorca","munich","paris","sevilla","valencia"),multiple = TRUE),
                        fluidRow( 
                            box(title = "Graph", width = 6, solidHeader = TRUE, status = "warning",plotOutput("NewDim")),
                            box(title = "Features", width = 4, solidHeader = TRUE, status = "warning",
                                selectInput("Categorydim", "Select the category 1", c("property_type", "room_type" , 'bedrooms' ,
                                                                                "beds", "price","neighbourhood_cleansed","availability_30", "price_30", "revenue_30"
                                                                              )),
                                selectInput("Category2dim", "Select the category 2", c("property_type", "room_type" , 'bedrooms' ,
                                                                                   "beds", "price","neighbourhood_cleansed","availability_30", "price_30", "revenue_30"
                                                                                )),
                                selectInput("typePlot1dim", "Select the plot type", c("boxplot"="geom_boxplot")),
                                dateRangeInput(
                                    inputId = "daterange1",
                                    label = "Select the date", 
                                    start = "2020-04-30",
                                    end  =  "2020-09-19",
                                    min  =  "2020-04-30",
                                    max  =  "2020-09-19",
                                    format = "yyyy/mm/dd",
                                    separator = " - "
                                ),
                            )),
                ),
                tabItem(tabName = "Map",
                        fluidPage(
                            box(title = "Map", width =6, solidHeader = TRUE, status = "warning",
                                leafletOutput("map",height = "500"),
                            ),
                            box(title = "Options", width = 6, solidHeader = TRUE, status = "warning",
                                selectInput("cities", "Select the city", c("barcelona","berlin","bordeaux","euskadi","girona","lyon","madrid","malaga","mallorca","menorca","munich","paris","sevilla","valencia")),
                                selectizeInput("neighborhood", "Select the neighborhood", choices = "", selected = ""),
                                selectizeInput("roomtype", "Select the room type", choices = "", selected = ""),
                                sliderInput("slideBed","select the numbre of bedrooms", min=0, max= 10, value = c(2,5)),
                            )
                        )
                        
                        
                ),
                tabItem(tabName = "data",
                        fluidRow( 
                            box(title = "Data of the city", width =4, solidHeader = TRUE, status = "warning",
                                selectInput("statenamesdata", "Select the city", c("barcelona","berlin","bordeaux","euskadi","girona","lyon","madrid","malaga","mallorca","menorca","munich","paris","sevilla","valencia"))),
                            
                        ),
                        fluidPage(dataTableOutput('table'))
                ),
                tabItem(tabName = "aboutus",
                        fluidRow(
                            column(1,
                                   tags$img(src="ece.jpg",height=150,width=150),
                            )),
                        br(),
                        fluidRow(
                            box(title = "Presentation of our team", width = 8, solidHeader = TRUE, status = "primary",
                                h2("Projet R : App Shiny"),
                                h3("- We are 4 students of Big data and Analytics from ECE Paris : RAHLI Sofiane, Aya LAMKADEM, Yvan COMPAORE, Assia KABZOI."),
                                h3("- Teacher : Miss Maggie MHANNA."),
                                h3("- Link to the app : "),tags$a(href="https://projectappshinyrcal.shinyapps.io/APP_Shiny/", "Click here!"),
                                h3("- Link to the repository GitHub : "),tags$a(href="https://github.com/sofianerahli/ProjectR_AppShiny", "Click here!")
                            )), 
                        
                )
            )
            
            
        )
    )
)