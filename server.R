
server <- function(input, output) {
  
  library(readxl)
  library(tidyverse)
  
  
  data <- read_excel("IPV Data Quantitative collection Compiled (1).xlsx") %>% 
    rename(outcome = MedCond) %>% 
    filter(outcome %in% c(0, 1))
  
  
  model <- glm(outcome ~ LevEduc + Age + sex + Ubudehe, data = data, family = binomial)
  

  output$model_summary <- renderPrint({
    summary(model)
  })
  
  
  output$predictions <- renderTable({
    new_data <- data.frame(
      Age = c(19, 25),
      LevEduc = factor(c("No education", "Primary education", "Secondary education", "TVET", "University education")),
      Ubudehe = factor(c("Category 1", "Category 2", "Category 3", "Category 4"))
    )
    predictions <- predict(model, newdata = new_data, type = "response")
    data.frame(new_data, Predicted_Probability = predictions)
  })
  
  
  
  
  
    
    # Database connection
    con <- dbConnect(RSQLite::SQLite(), dbname = "recorded_data.sqlite")
    
    # Create the table if it doesn't exist
    dbExecute(con, "CREATE TABLE IF NOT EXISTS mental_health_data (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    gender INTEGER,
                    age_group INTEGER,
                    disorder INTEGER
                  )")
    
    observeEvent(input$submit, {
      # Insert data into the database
      dbExecute(con, "INSERT INTO mental_health_data (gender, age_group, disorder) VALUES (?, ?, ?)",
                params = list(input$gender, input$age_groupe_selection, paste(input$disorder_selection, collapse = ",")))
      
      # Immediately update the dashboard with the latest data
      output$table_mentalhealth <- renderTable({
        dbGetQuery(con, "SELECT * FROM mental_health_data")
      })
      
      #you can show a message indicating the data has been submitted
      showModal(modalDialog(
        title = "Data Submitted",
        "Your data has been successfully submitted and the dashboard has been updated.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    
    
    onStop(function() {
      dbDisconnect(con)
    })
  
  
  observeEvent(input$submit, {
    
    dbExecute(con, "INSERT INTO mental_health_data (gender, age_group, disorder) VALUES (?, ?, ?)",
              params = list(input$gender, input$age_groupe_selection, paste(input$disorder_selection, collapse = ",")))
    
    
    print(paste("Inserted into database: Gender:", input$gender, "Age Group:", input$age_groupe_selection, "Disorder:", paste(input$disorder_selection, collapse = ",")))
  })
  
  
  

    
    # Reactive value to track login status
    logged_in <- reactiveVal(FALSE)
    
    # Define valid credentials (for demonstration purposes)
    valid_username <- "Mind wellness"
    valid_password <- "marie2020@134"
    
    observeEvent(input$login, {
      # Check if the provided username and password are correct
      if (input$username == valid_username && input$password == valid_password) {
        logged_in(TRUE)  
      } else {
        showModal(modalDialog(
          title = "Login Failed",
          "Invalid username or password. Please try again.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    output$data_table <- renderTable({
      # Only fetch and display data if the user is logged in
      req(logged_in())  # This will stop execution if is FALSE
      
      # Fetch data from the database
      dbGetQuery(con, "SELECT * FROM mental_health_data")
    })
  
  
  
  
  onStop(function() {
    dbDisconnect(con)
  })
  
  observeEvent(input$submit, {
    # Collect responses from the input selections
    questions_responses <- sapply(1:17, function(i) {  # Change 2 to 17 when all questions are added
      as.numeric(input[[paste0("question", i)]])  # Ensure values are numeric
    })
    
    # Calculate the total score
    score_display <- sum(questions_responses, na.rm = TRUE)
    
    # Determine severity based on the score
    mental_disorder_severity <- if (score_display >= 0 & score_display <= 7) {
      "Minimal mental disorder symptoms"
    } else if (score_display >= 8 & score_display <= 17) {
      "Mild mental disorder"
    } else if (score_display >= 18 & score_display <= 27) {
      "Moderate mental disorder"
    } else if (score_display >= 28 & score_display <= 37) {
      "Moderately severe depression"
    } else if (score_display >= 38 & score_display <= 47) {
      "Severe mental disorder"
    } else {
      "Highly severe mental disorder"
    }
    
    # Render the score and severity outputs
    output$score_display <- renderText({
      paste("Your total score is", score_display, ".")
    })
    
    output$severity_display <- renderText({
      paste("This indicates that you have", mental_disorder_severity, ".")
    })
  })
  mental_health_by_sexrbc <- read_excel("mentalhealth_bysex_rbc.xlsx")
  mental_health_by_sex <- data.frame(mental_health_by_sexrbc)
  
  
  data_grouped_datarbc <- read_excel("data_grpdage_rbc.xlsx")
  data_grpdage <- data.frame(data_grouped_datarbc)
  
  
  data_rest_datarbc <- read_excel("mentalhealth_rbcdata.xlsx")
  mentalhealthprob_data <- data.frame(data_rest_datarbc)
  
  
  data_trend_rbc <- read_excel("data_trend_rbc.xlsx")
  data_datatrend <- data.frame(data_trend_rbc)
  
  
  data_long_rbc <- data_datatrend %>%
    pivot_longer(cols = c(Male_Prevalence, Female_Prevalence), 
                 names_to = "Sex", 
                 values_to = "Prevalence"
    )
  

  output$bargraph_status <- renderPlot({
    ggplot(mental_health_by_sex, aes(x = Disorder, y = Prevalence, fill = Sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Prevalence of Mental Disorders among youth aged 19 - 25 years by Sex in %",
           x = "Mental Disorder",
           y = "Prevalence (%)") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 16)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set1") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
  
  
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
  
  
  output$trend_line <- renderPlot({ 
    ggplot(data_long_rbc, aes(x = Year, y = Prevalence, color = Sex, group = Sex)) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) +
      facet_wrap(~ Disorder, scales = "free_y") +
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
  
  
  output$table_mentalhealth <- renderTable({
    mentalhealthprob_data
  })
}