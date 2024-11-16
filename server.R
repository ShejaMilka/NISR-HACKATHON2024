library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(tidyverse)
library(readxl)

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
    responses$phq9 <- input$question9
    
    # Print the responses to the console
    print(paste("Depression:", responses$phq))
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
  
}