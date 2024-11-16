library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(tidyverse)
library(readxl)

# Define UI
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Insight in Youth Mental Health Issues",
                  titleWidth = 350
                  ),
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("eye")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", startExpanded = TRUE, icon = icon("database"),
                 menuSubItem("NISR data", tabName = "raw_nisr", icon = icon("table")),
                 menuSubItem("RBC data", tabName = "raw_rbc", icon = icon("table"))
        ),
        menuItem("Educational Resources", tabName = "resources", startExpanded = TRUE, icon = icon("book"),
                 menuSubItem("Documents", tabName = "documents", icon = icon("file-alt")),
                 menuSubItem("Videos", tabName = "videos",  icon = icon("play-circle"))
        ),
        menuItem("Self Assessment", tabName = "assessments", icon = icon("check-square"))
      )
    ),
  
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  box(plotOutput("bargraph_status")),
                  box(plotOutput("bargraph_agegrpd")),
                  box(plotOutput("trend_line")),
                  box(title = tags$h3("Non-Frequent Mental Health Problem in Youth"), 
                      tableOutput("table_mentalhealth")
                  )
                )
        ),
        
        tabItem( tabName = "overview",
            fluidRow(
              box(
                p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                   "The insight in Youth Mental Health is a web app designed for the purpose of the NISR Hackathon 2024 Competion. 
                   It helps to gain insight into the problems."
                 ),
                
               p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                 "Rwanda faces significant mental health challenges, including high rates of depression and anxiety disorders.
                 The historical trauma of the 1994 Genocide continues to impact the mental health of many, particularly younger generations. 
                 A significant portion of the population remains unaware of mental health services, and even fewer actually seek help. 
                 Additionally, behavioral risk factors like substance abuse and low educational attainment contribute to poor mental health outcomes."
               )
             ),
               
            box(
               p(style = "font-size: 20px;text-align: justify; line-height: 2;" ,
                 "To address these challenges, a multi-faceted approach is necessary. This includes increasing awareness and reducing stigma, 
                 strengthening mental health services, addressing underlying social and economic factors, leveraging technology, 
                 and conducting research to inform evidence-based interventions. Specific strategies such as:",
                   tags$ul(style = "font-size: 20px;text-align: justify; line-height: 2;",
                           tags$li("School-based mental health programs,"),
                           tags$li("Community-based services, and"),
                           tags$li("Trauma-informed care")
                 ),
               p(style = "font-size: 20px;text-align: justify; line-height: 2;", 
                   "can play a crucial role in improving mental health outcomes in Rwanda."
                 )
               )
             )
            )
          ),
        
        tabItem(tabName = "data",
              dataTableOutput("raw_data_nisr"),
              dataTableOutput("raw_data_rbc")
          ),
        
        tabItem(tabName = "resources",
                fluidRow(
                  box(
                    title = "documents",
                    downloadButton("downloadExercises", "Download Exercises")
                  )
                ),
                box(dataTableOutput("videos"))
          ),
        
        tabItem(tabName = "assessments",
            box("Over the last 2 weeks, how often have you been bothered by any of the following problems?", width = 920),
            fluidRow(
              box(
                title = "1.",
                selectInput("question1", "Problem of little interest or pleasure in doing things",
                            choices = c("Not at all", 
                                        "Several days", 
                                        " More than half the days", 
                                        "Nearly every day")
                )
              ),
              box(
                title = "2.",
                selectInput("question2", "Problem of feeling down, depressed, or hopeless",
                            choices = c("Not at all", 
                                        "Several days",
                                        " More than half the days",
                                        "Nearly every day")
                )
              ),
              box(
                title = "3.",
                selectInput("question3", "Problem of trouble falling or staying asleep, or sleeping too much?",
                            choices = c("Not at all", 
                                        "Several days", 
                                        " More than half the days",
                                        "Nearly every day")
                )
              ),
              box(
                title = "4.",
                selectInput("question4", "Feeling tired or having little energy",
                            choices = c("Not at all", 
                                        "Several days", 
                                        "More than half the days", 
                                        "Nearly every day")
                )
              ),
              box(
                title = "5.",
                selectInput("question6", "Poor appetite or overeating",
                            choices = c("Not at all", 
                                        "Several days", 
                                        "More than half the days", 
                                        "Nearly every day")
                )
              ),
              box(
                title = "6.",
                selectInput("question6", " Feeling bad about yourself – or that you are a failure or have let yourself or your family down",
                            choices = c("Not at all",
                                        "Several days",
                                        "More than half the days",
                                        "Nearly every day")
                )
              ),
              box(
                title = "7.",
                selectInput("question7", "Trouble concentrating on things, such as reading the newspaper or watching television",
                            choices = c("Not at all",
                                        "Several days", 
                                        "More than half the days",
                                        "Nearly every day")
                )
              ),
              box(
                title = "8.",
                selectInput("question8", "Moving or speaking so slowly that other people could have noticed? Or the opposite – being so fidgety or restless that you have been moving around a lot more than usual",
                            choices = c("Not at all", 
                                        "Several days", 
                                        "More than half the days",
                                        "Nearly every day")
                )
              ),
              box(
                title = "9.",
                selectInput("question9", "Thoughts that you would be better off dead or of hurting yourself in some way",
                            choices = c("Not at all", 
                                        "Several days", 
                                        " More than half the days",
                                        "Nearly every day")
                )
              ),
              box(
                actionButton("submit", "Submit")
              )
          )
       )
     )
   )
 )