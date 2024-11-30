library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(tidyverse)
library(readxl)

# Define Server
server <- function(input, output) {
 
   # Reactive value to store responses
  questions_phq9 <- reactiveValues(
    question1 = 0,
    question2 = 0,
    question3 = 0,
    question4 = 0,
    question5 = 0,
    question6 = 0,
    question7 = 0,
    question8 = 0,
    question9 = 0
  )
  
  observeEvent(input$question1, {
    questions_phq9$question1 <- as.numeric(input$question1)
  })
  
  observeEvent(input$question2, {
    questions_phq9$question2 <- as.numeric(input$question2)
  })
  
  observeEvent(input$question3, {
    questions_phq9$question3 <- as.numeric(input$question3)
  })
  
  observeEvent(input$question4, {
    questions_phq9$question4 <- as.numeric(input$question4)
  })
  
  observeEvent(input$question5, {
    questions_phq9$question5 <- as.numeric(input$question5)
  })
  
  observeEvent(input$question6, {
    questions_phq9$question6 <- as.numeric(input$question6)
  })
  
  observeEvent(input$question7, {
    questions_phq9$question7 <- as.numeric(input$question7)
  })
  
  observeEvent(input$question8, {
    questions_phq9$question8 <- as.numeric(input$question8)
  })
  
  observeEvent(input$question9, {
    questions_phq9$question9 <- as.numeric(input$question9)
  })
  
  output$phq9_score <- renderText({
    score_display <-  questions_phq9$question1 + questions_phq9$question2 + 
      questions_phq9$question3 + questions_phq9$question4 + 
      questions_phq9$question5 + questions_phq9$question6 + 
      questions_phq9$question7 + questions_phq9$question8 + 
      questions_phq9$question9
    paste("Your PHQ-9 score is", score_display,".")
  })
  
  output$depression_severity <- renderText({
    score <-  questions_phq9$question1 + questions_phq9$question2 + 
      questions_phq9$question3 + questions_phq9$question4 + 
      questions_phq9$question5 + questions_phq9$question6 + 
      questions_phq9$question7 + questions_phq9$question8 + 
      questions_phq9$question9

    depression_severity <- if (score >= 0 & score <= 4) {
      "Minimal depression symptoms"
    } else if (score >= 5 & score <= 9) {
      "Mild depression"
    } else if (score >= 10 & score <= 14) {
      "Moderate depression"
    } else if (score >= 15 & score <= 19) {
      "Moderately severe depression"
    } else {
      "Severe depression"
    }
    paste("This indicates that you have", depression_severity, ".")
  })
  

  # Data for mental health status by sex
  mental_health_by_sexrbc <- read_excel("mentalhealth_bysex_rbc.xlsx")
  mental_health_by_sex <- data.frame(mental_health_by_sexrbc)
  
  
  # Dataset for the data grouped by age group
  data_grouped_datarbc <- read_excel("data_grpdage_rbc.xlsx")
  data_grpdage <- data.frame(data_grouped_datarbc)
  
  
  # Create a data frame with the rest of the mental helth problem in youth
  
  data_rest_datarbc <- read_excel("mentalhealth_rbcdata.xlsx")
  mentalhealthprob_data <- data.frame(data_rest_datarbc)
  
  
  # Assuming you have data for different years
  data_trend_rbc <- read_excel("data_trend_rbc.xlsx")
  data_datatrend <- data.frame(data_trend_rbc)
  
  
  # Reshape the data for different years to enable plotting
  data_long_rbc <- data_datatrend %>%
    pivot_longer(cols = c(Male_Prevalence, Female_Prevalence), 
                 names_to = "Sex", 
                 values_to = "Prevalence"
    )
  
  
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
    ggplot(data_long_rbc, aes(x = Year, y = Prevalence, color = Sex, group = Sex)) +
      geom_line(linewidth = 2) +
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
  
}