#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shinythemes)
library(dplyr)
library(xtable)
library(ggplot2)
library(shinyWidgets)
library(ggthemes)
# 
# x <-paste("/Users/soheilmoattarmohammadiberenguer/Desktop/Entrega_Soheil_Moattar_Berenguer/Fifa_App",
#           sep="")
# 
# #Se the specified path as new directory
# setwd(x)
# source("./global.R")

# Define UI for application that draws a histogram

# UI for application
ui <- fluidPage(
    setBackgroundColor("#f3f6f4"), 

    tabsetPanel(
        tabPanel("About",
                img(src="mbappefifa22.jpg", width = "100%", style="display: block; margin-top: auto; margin-left: auto; margin-right: auto; margin-bottom: -4.25%"),
                hr(),
                hr(),
                hr(),
                hr(),
                h3(strong("Whats the point?")), 
                p("The purpose of this app is to allow the user to easily compare FIFA 22 players on the basis of many attributes that are included in the game."),
                hr(),
      

                 #h3(strong("Whats the point?")),
                 #p("The purpose of this app is to allow the user to easily compare FIFA 22 players on the basis of many attributes that are included in the game."),
                 #hr(),
                 h3(strong("What is FIFA22?")),
                 p("FIFA 22 is a football simulation video game developed by EA Vancouver as part of Electronic Arts' FIFA series.
                      It is the 29th installment in the FIFA series, and was released on 26 September 2021 for PlayStation 5, Xbox Series X/S, as well as Windows, PlayStation 4 and Xbox One."),
                 hr(),
                 h3(strong("About the dataset")),
                 h4("Where is the dataset from?"),
                 p("This dataset comes from Kaggle and was created by", a(href="https://www.kaggle.com/stefanoleone992", "Stefano Leone")), 
                 p("A link to the datasets can be found here:", a(href="https://www.kaggle.com/datasets/stefanoleone992/fifa-22-complete-player-dataset", "Kaggle 'FIFA 22 complete player dataset.'")),
                 h4("What is the content of the dataset?"),
                 p("This dataset contains detailed attributes for every player registered in the latest edition of FIFA 22 database (December 2021).",
                   "The data has been scraped from the website", a(href="https://sofifa.com/", "sofifa")),
                 p("- Over 19,000 FIFA 22 players and 100+ attributes extracted from the latest FIFA database "),
                 #p("- 100 attributes extracted from the latest FIFA database"),
                 hr(),
                h3(strong("About the Author")),
                p("This Shiny App has been developed by Soheil Moattar M. Berenguer as part of 
                  an assignment for the Data Science and Big Data Master at Complutense University of Madrid (2021-2022)", a(href="https://github.com/SoheilMb?tab=repositories", "GitHub")),

        ),
        
        tabPanel("Compare Players", 
                 titlePanel("Select Players to compare"),
                 p("Use the search box below to find and select players to compare their attributes."),
                 #I have to limit the number of player names otherwise i get this warning
                 #p("The description tab will allow you to see an overall view of the players and 
                 #their attributes while the visualization tab will give a closer look at a 
                 #specific attribute."),
                 #Warning: The select input "name" contains a large number of options; 
                 #consider using server-side selectize for massively improved performance. 
                 #See the Details section of the ?selectizeInput help topic.
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput(inputId = 'name', label = 'Search Players', choices = unique(fifa2$Name[1:1000]),
                                        selected = "K.Benzema", multiple = TRUE, options = list(create = FALSE))),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Description", tableOutput("table1")),
                             tabPanel("Visualization",
                                      selectInput("att", "Choose a variable to compare:",
                                                  choices = colnames(fifaNUM)#,
                                      ),

                                      plotOutput("plot"))
                         )))),
        
        
        
        
        
        
        
        tabPanel("Filter by Attribute",
                 titlePanel("Choose and Attribute to filter players"),
                 p("Choose an attribute by tab in order to filter players."),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("League", 
                                  selectInput("League", "Choose an League:", choices = unique(fifa2$League), selected = ""),
                                  tableOutput("tableLeague")),
                         tabPanel("Age",
                                  sliderInput("Age", "Choose an Age:", min = 15, max = 45, value = c(25)),
                                  tableOutput("tableAge")),
                         tabPanel("Nationality",
                                  selectInput("Nationality", "Choose a Nationality:", choices = unique(fifa2$Nationality), selected = "France"),
                                  tableOutput("tableNationality")),
                         tabPanel("Club",
                                  selectInput("Club", "Choose a Club:", choices = unique(fifa2$Club), selected = "Real Madrid FC"),
                                  tableOutput("tableClub"))
                     )))
        
    ))
               
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # First TAB Compare PLAYERS
    # Compare Players
    fifa_subset <- reactive({filter(fifa2, Name %in% input$name)})
    output$table1 <- renderTable({xtable(fifa_subset())})
    
    # Compare Players Plot
    output$plot <- renderPlot({
        y_axis = input$att
        ggplot(fifa_subset(), aes_string(x = fifa_subset()$Name, y = y_axis)) + 
            coord_flip()+
            #theme_classic() +
            theme_economist() +
            theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "#d1edf4")) +
            geom_bar(stat = "identity", fill = '#1CA9C9', color = '#E5E4E2')
    })  ##d1edf4
    
    # Second Tab Compare FIFA ATTRIBUTES
    # Compare Fifa Attributes
    #Overall
    fifa_subset_League <- reactive({filter(fifa2, League == input$League)})
    output$tableLeague  <- renderTable({xtable(fifa_subset_League())})
    
    # #Age
    fifa_subset_Age <- reactive({filter(fifa2, Age == input$Age)})
    output$tableAge <- renderTable({xtable(fifa_subset_Age())})
    # 
    #Nationality
    fifa_subset_Nationality <- reactive({filter(fifa2, Nationality == input$Nationality)})
    output$tableNationality <- renderTable({xtable(fifa_subset_Nationality())})

    #Release Clause
    fifa_subset_Club <- reactive({filter(fifa2, Club == input$Club)})
    output$tableClub <- renderTable({xtable(fifa_subset_Club())})
}
# Run the application 
shinyApp(ui = ui, server = server)
