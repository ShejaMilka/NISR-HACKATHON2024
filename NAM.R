library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(tidyverse)

# Define UI
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Youth Mental Health Problems",
                  titleWidth = 350
                  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("eye")),
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
                box(tableOutput("table_mentalhealth"))
              )
      ),
      
      tabItem( tabName = "overview",
               fluidRow(
               box(p(style = "font-size: 20px; text-align: justify; line-height: 2;","Rwanda faces significant mental health challenges, including high rates of depression and anxiety disorders.
               The historical trauma of the 1994 Genocide continues to impact the mental health of many, particularly younger generations. 
               A significant portion of the population remains unaware of mental health services, and even fewer actually seek help. 
               Additionally, behavioral risk factors like substance abuse and low educational attainment contribute to poor mental health outcomes.")
                ),
               
               box(p(style = "font-size: 20px;text-align: justify; line-height: 2;" , "To address these challenges, a multi-faceted approach is necessary. This includes increasing awareness and reducing stigma, 
               strengthening mental health services, addressing underlying social and economic factors, leveraging technology, 
               and conducting research to inform evidence-based interventions. Specific strategies such as:",
               tags$ul(style = "font-size: 20px;text-align: justify; line-height: 2;",
                 tags$li("School-based mental health programs,"),
                 tags$li("Community-based services, and"),
                 tags$li("Trauma-informed care")
               ),
               p(style = "font-size: 20px;text-align: justify; line-height: 2;", "can play a crucial role in improving mental health outcomes in Rwanda.")
               ))
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


# Define Server
server <- function(input, output) {
  # Reactive value to store responses
  responses <- reactiveValues(
    name = NULL,
    color = NULL,
    shiny_like = NULL,
    shiny_like = NULL
  )
  
  observeEvent(input$submit, {
    responses$phq1 <- input$question1
    responses$phq2 <- input$question2
    responses$phq3 <- input$question3
    responses$phq4 <- input$question4
    responses$phq5 <- input$question5
    responses$phq6 <- input$question6
    responses$phq7 <- input$question7
    responses$phq8 <- input$question8
    responses$phq9 <- input$question8
    
    # Print the responses to the console
    print(paste("Depression:", responses$phq))
  })
  
  # Data for mental health status
  mental_health_by_sex <- data.frame(
    Disorder = rep(c("Major Depressive Episode", "Panic Disorder", "PTSD", "Obsessive-Compulsive Disorder", "Substance Use Disorder"), each = 2),
    Sex = rep(c("Male", "Female"), times = 5),
    Prevalence = c(8.2, 14.4, 5.2, 10.2, 2.6, 4.4, 2.7, 4.2, 3.4, 0.3) # Hypothetical prevalence rates in percentage
  )
  
  # Dataset for the data grouped by age group
    data_grpdage <- data.frame(
      AgeGroup = rep(c("10-14", "15-19"), each = 2),
      Prevalence = c(12, 14, 8, 16),  # Example prevalence rates
      Gender = rep(c("Male", "Female"), times = 2),
      Year = rep(2020, 4)
    )
    
    # Create a data frame with the provided data
    mentalhealthprob_data <- data.frame(
      Disorder = c("Social anxiety disorder", 
                   "Obsessive-compulsive disorder", 
                   "Alcohol use disorder", 
                   "Psychotic disorder", 
                   "Antisocial personality disorder", 
                   "Epilepsy", 
                   "Bipolar disorder", 
                   "Any mental disorder"),
      Male = c(1.20, 2.70, 3.40, 1.40, 1.70, 3.20, 0.20, 16.60),
      Female = c(1.30, 4.20, 0.30, 1.30, 0.20, 2.70, 0.10, 23.20),
      "14 to 18" = c(0.90, 2.10, 0.40, 0.40, 1.10, 2.80, 0.10, 10.20),
      "19 to 25" = c(1.50, 4.00, 1.50, 1.00, 1.10, 3.00, 0.10, 17.20),
      Total = c(4.90, 13.00, 5.60, 4.10, 4.10, 11.70, 0.50, 67.20)
    )
    
  # Assuming you have data for different years
    data_trend <- data.frame(
      Year = rep(2018:2020, each = 4),
      Disorder = rep(c("Major Depressive Episode", "Panic Disorder", "PTSD", "Obsessive-Compulsive Disorder"), 
                     times = 3),
      Male_Prevalence = c(8.2, 7.5, 6.0, 5.0, 10.0, 9.5, 8.0, 6.5, 11.0, 10.5, 9.0, 7.5),
      Female_Prevalence = c(14.4, 13.0, 12.0, 10.0, 15.0, 14.0, 13.5, 12.0, 16.0, 15.5, 14.0, 13.0)
    )
    
    # Reshape the data for different years to enable plotting
    data_long <- data_trend %>%
      pivot_longer(cols = c(Male_Prevalence, Female_Prevalence), 
                   names_to = "Sex", 
                   values_to = "Prevalence")
    

  # Generate plot 
    output$bargraph_status <- renderPlot({
    # Plotting code here
    # Create a grouped bar chart to represent the prevalence of mental disorders by sex
    ggplot(mental_health_by_sex, aes(x = Disorder, y = Prevalence, fill = Sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Prevalence of Mental Disorders among youth aged 19 - 25 years by Sex",
           x = "Mental Disorder",
           y = "Prevalence (%)") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 16)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1") + # Optional: Use a color palette for better aesthetics
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(face = "bold")
      )
    
  })
    
  # Generate plot grouped by age
  output$bargraph_agegrpd <- renderPlot({ 
    ggplot(data_grpdage, aes(x = AgeGroup, y = Prevalence, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Prevalence of Mental Health Issues by Age Group and Gender",
           x = "Age Group",
           y = "Prevalence (%)") +
      theme_minimal() + 
      theme(
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(face = "bold")
      )
    })
  
  # Generate trend plot over years
  output$trend_line <- renderPlot({ 
    ggplot(data_long, aes(x = Year, y = Prevalence, color = Sex, group = Sex)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      facet_wrap(~ Disorder, scales = "free_y") +  # Create separate plots for each disorder
      labs(title = "Trend of Mental Disorders Over Years by Sex",
           x = "Year",
           y = "Prevalence (%)") +
      scale_y_continuous(limits = c(0, NA)) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
        )
  })
    
    
  # Generate table
  output$table_mentalhealth <- renderTable({
    # Table data here
    mentalhealthprob_data
  })
  
  # Display raw data from NISR
  output$raw_data_nisr <- renderTable({
    # Raw data here
    
  })
  
  # Display raw data from RBC
  output$raw_data_rbc <- renderDataTable({
    # Raw data here
  })
  
  # Documents Resource 1
  output$downloadExercises <- downloadHandler(
    filename = "Introduction to Programming_Programming with R_EXERCISES.pdf",
    content = function(file) {
      file.copy("C:/Users/HP ENVY/OneDrive/Desktop/Introduction to Programming_Programming with R_EXERCISES.pdf", file)
    })
}

# Run the app
shinyApp(ui, server)
