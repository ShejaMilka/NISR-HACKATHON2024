library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(tidyverse)
library(readxl)
library(DBI)

# Define UI
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Youth Mental Health Problems in Rwanda",
                  titleWidth = 350
                  ),
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("eye")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Logistic Model", tabName = "logistic_model", icon = icon("chart-line")),
        menuItem("signs_symptoms", tabName = "signs_symptoms", icon = icon("check-square")),
        menuItem("Educational Resources", tabName = "resources", startExpanded = TRUE, icon = icon("book")
                 ),
        menuItem("self_assessment", tabName = "self_assessment", icon = icon("pen")),
        
          menuItem("Data", tabName = "data", startExpanded = TRUE, icon = icon("database")
           )
  )
  ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
            titlePanel(h2("NISR 2024 HACKATHON COMPETITION", style = "font-size: 36px; text-align: center;font-weight: bold;")),
            fluidRow(
              box(solidHeader = TRUE,background = "light-blue",
                p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                   "The insight Mental Health problems in youth is a web app designed for the purpose of the NISR Hackathon 2024 Competion. 
                   It helps to gain insight into the problems."
                 ),
                
               p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                 "Rwanda faces significant mental health challenges, including high rates of depression and anxiety disorders.
                 The historical trauma of the 1994 Genocide continues to impact the mental health of many, particularly younger generations. 
                 A significant portion of the population remains unaware of mental health services, and even fewer actually seek help. 
                 Additionally, behavioral risk factors like substance abuse and low educational attainment contribute to poor mental health outcomes."
               )
             ),
               
            box(solidHeader = TRUE,background = "light-blue",
               p(style = "font-size: 20px;text-align: justify; line-height: 2;" ,
                 "To address these challenges, a multi-faceted approach is necessary. This includes increasing awareness and reducing stigma, 
                 strengthening mental health services, addressing underlying social and economic factors, leveraging technology, 
                 and conducting research to inform evidence-based interventions. Specific strategies such as:",
                   tags$ul(style = "font-size: 20px;text-align: justify; line-height: 2;",
                           tags$li("School-based mental health programs,"),
                           tags$li("Community-based services, and"),
                           tags$li("Trauma-informed care")
                 )
                 ),
               p(style = "font-size: 20px;text-align: justify; line-height: 2;", 
                   "can play a crucial role in improving mental health outcomes in Rwanda."
                 )
               )
             )
          ),
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
        
        tabItem(tabName = "logistic_model",
                fluidRow(
                  box(title = "Logistic Regression Model Summary", width = 12, solidHeader = TRUE,
                      verbatimTextOutput("model_summary")),
                  box(title = "Predictions", width = 12, solidHeader = TRUE,
                      tableOutput("predictions"))
                )
        ),
        tabItem(tabName = "resources",
                titlePanel(h2("POSSIBLE SIGNS OF MENTAL UN-WELLNESS", style = "font-size: 36px; text-align: center; font-weight: bold;",column = 2)),
                fluidRow(
                 
           
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Major Depressive Disorder",
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "A mood disorder characterized by persistent feelings of sadness and a loss of interest in activities."
                      ),
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "Signs and Symptoms: Persistent sadness, loss of interest in activities, changes in appetite and sleep patterns, fatigue, 
                        difficulty concentrating, feelings of worthlessness or guilt,

 thoughts of death or suicide."
                      )
                  ),
                  
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Panic Disorder",
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "An anxiety disorder characterized by recurrent, unexpected panic attacks."
                      ),
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "Signs and Symptoms: Frequent panic attacks, intense fear or discomfort, rapid heart rate, sweating, trembling, shortness of breath, chest pain, nausea, dizziness, chills or hot flashes, numbness or tingling sensations, fear of losing control or dying."
                      )
                  ),
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Post-Traumatic Stress Disorder (PTSD)",
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        " A mental health condition that is triggered by a terrifying event — either experiencing it or witnessing it. 
                        Symptoms may include flashbacks, nightmares and
                        severe anxiety, as well as uncontrollable thoughts about the event  
."
                      ),
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "Signs and Symptoms: Reliving the traumatic event through flashbacks or nightmares, avoidance of reminders of the trauma, persistent negative thoughts and feelings, hyperarousal (increased alertness, irritability, difficulty sleeping)."
                        
                      )
                  ),
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Psychotic Disorder",
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        "A mental disorder characterized by a loss of contact with reality, often involving delusions and hallucinations. "
                      ),
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "Signs and Symptoms: Delusions (false beliefs not based on reality), hallucinations (seeing or hearing things that aren't there), disorganized thinking, disorganized behavior, lack of motivation, social withdrawal."
                        
                      )
                  ),
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Bipolar Disorder",
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        "A mental health condition characterized by mood swings that alternate between manic (high) and depressive (low) episodes."
                      ),
                      
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "Signs and Symptoms: Mood swings that alternate between manic (high) and depressive (low) episodes."
                      )
                      
                  ),
                  
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Substance Use Disorder",   
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        " A mental health condition characterized by compulsive substance use despite negative consequences."
                      ),
                      
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",  
                        "Signs and Symptoms: Cravings for the substance, difficulty controlling use, continued use despite negative consequences, tolerance (need for increasing amounts to achieve the same effect), withdrawal symptoms when use is stopped."
                      )
                      
                  ),
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Epilepsy",
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        "A neurological disorder characterized by recurrent seizures."
                      ),
                      
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "Signs and Symptoms: Seizures (temporary changes in brain activity that cause changes in behavior, movement, sensation, or consciousness), aura (warning signs before a seizure), loss of consciousness, muscle spasms, staring spells, changes in behavior or mood."
                      )
                      
                      
                  ),
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Antisocial Personality Disorder",
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        "A mental health condition in which people consistently show no regard for right and wrong and ignore the rights and feelings of others. "
                      ),
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",
                        "Signs and Symptoms: Disregard for the rights and feelings of others, manipulative behavior, lack of empathy, impulsivity, aggressiveness, disregard for rules and laws."
                        
                      )
                  ),
                  
                  
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Social Anxiety Disorder",   
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        " An anxiety disorder characterized by intense fear of social situations and the fear of being judged or negatively evaluated."
                      ),
                      
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",  
                        "Signs and Symptoms:Intense fear of social situations, fear of being judged or negatively evaluated, avoidance of social situations, physical symptoms like blushing, sweating, trembling, rapid heart rate."
                      )
                  ),
                  box(solidHeader = TRUE, background = "light-blue",
                      title = "Obsessive-Compulsive Disorder (OCD)",   
                      p(style = "font-size: 20px; text-align: justify; line-height: 2 ;" ,
                        "A mental disorder in which people have unwanted and intrusive thoughts, feelings, ideas, sensations (obsessions) and engage in repetitive behaviors (compulsions) in response to these thoughts."
                      ),
                      p(style = "font-size: 20px; text-align: justify; line-height: 2;",  
                        "Signs and Symptoms: Obsessions (recurring, unwanted thoughts, images, or urges), compulsions (repetitive behaviors or mental acts performed to reduce anxiety caused by obsessions)."
                      )
                  )
                )
                
        ),    
        
        tabItem(tabName = "signs_symptoms",
                titlePanel(h2("SIGNS OF MENTAL DISORDER", style = "font-size: 36px; text-align: center;font-weight: bold;")),
                      fluidRow(style = "background-color: lightblue;",
               
                     
                     box(background = "light-blue",
       
                  title = "Q-1.",width = 12,
                selectInput("question1", "Manic episodes (elevated mood, increased energy, racing thoughts, impulsive behavior)",
                            choices = c("Not at all" = 0, 
                                        "Several days" = 1, 
                                        "More than half the days" = 2, 
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-2.",width = 12,
                selectInput("question2", "Depressive episodes (similar to MDD)",
                            choices = c("Not at all" = 0, 
                                        "Several days" = 1,
                                        "More than half the days" = 2,
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-3.",width = 12,
                selectInput("question3", "Panic attacks, characterized by intense fear or discomfort, often accompanied by physical symptoms like chest pain, rapid heart rate, sweating, trembling, shortness of breath, and dizziness
",
                            choices = c("Not at all" = 0, 
                                        "Several days" = 1, 
                                        "More than half the days" = 2,
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-4.",width = 12,
                selectInput("question4", "Worry about future panic attacks",
                            choices = c("Not at all" = 0, 
                                        "Several days" = 1, 
                                        "More than half the days" = 2, 
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-5.",width = 12,
                selectInput("question5", "Reliving the traumatic event through flashbacks or nightmares)",
                            choices = c("Not at all" = 0, 
                                        "Several days" = 1, 
                                        "More than half the days" = 2, 
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-6.", width = 12,
                selectInput("question6", "Obsessions (recurring, unwanted thoughts, images, or urges)
",
                            choices = c("Not at all" = 0,
                                        "Several days" = 1,
                                        "More than half the days" = 2,
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-7.", width = 12,
                selectInput("question7", "Compulsions (repetitive behaviors or mental acts performed to reduce anxiety)",
                            choices = c("Not at all" = 0,
                                        "Several days" = 1, 
                                        "More than half the days" = 2,
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-8.", width = 12,
                selectInput("question8", "Cravings for the substance
",
                            choices = c("Not at all" = 0, 
                                        "Several days" = 1, 
                                        "More than half the days" = 2,
                                        "Nearly every day" = 3)
                )
              ),
              box(background = "light-blue",
                title = "Q-9.",width = 12,
                selectInput("question9", "Difficulty controlling use
",
                            choices = c("Not at all" = 0, 
                                        "Several days" = 1, 
                                        " More than half the days" = 2,
                                        "Nearly every day" = 3)
                )
              ),
              
              box(background = "light-blue",
                  title = "Q-10.",width = 12,
                  selectInput("question10", "Continued use despite negative consequences
",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Q-11.",width = 12,
                  selectInput("question11", "Intense fear of social situations or interactions

",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Q-12.",width = 12,
                  selectInput("question12", "Self-consciousness and fear of negative evaluation

",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Q-13.",width = 12,
                  selectInput("question13", "Delusions (false beliefs not based on reality)


",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Q-14.",width = 12,
                  selectInput("question14", "Hallucinations (sensory experiences that are not real)


",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Q-15.",width = 12,
                  selectInput("question15", "Deceitfulness, manipulation and Lack of remorse or guilt.



",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Q-16.",width = 12,
                  selectInput("question16", "Irritability and aggressiveness



",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Q-17.",width = 12,
                  selectInput("question17", "Seizures, which can vary in severity and presentation



",
                              choices = c("Not at all" = 0, 
                                          "Several days" = 1, 
                                          " More than half the days" = 2,
                                          "Nearly every day" = 3)
                  )
              ),
              box(background = "light-blue",
                  title = "Results", width = 12,
                  actionButton("submit", "Submit"),
                  textOutput("score_display"),
                  textOutput("severity_display")
              ),
               box(background = "light-blue",
                actionButton("submit", "Submit")
              ),
             )
  
                                          ),

          tabItem(tabName = "self_assessment",
                  titlePanel(h2("Self Assessment", style = "font-size: 36px; text-align: center;font-weight: bold;")),
                  box(p("According to what you read in Educational Resources, what disorder do you think you have?"), 
                      width = 920, style = "font-size: 20px;text-align: justify; line-height: 2;"),
                  fluidRow(style = "background-color: lightblue;",
                           
                           box(solidHeader = TRUE, background = "light-blue",
                               title = "Gender", width = 12,
                               selectInput("gender", "Your gender",
                                           choices = c("none" = 0,
                                             "male" = 1, 
                                                       "female" = 2)
                               )
                           ),
                           box(background = "light-blue",
                               title = "Age Group", width = 12,
                               selectInput("age_groupe_selection", "Age",
                                           choices = c("none" = 0,
                                                       "0-14"= 1,
                                                       "14-19"=2,
                                                       "19-21" = 3,
                                                       "21-55" = 4
                                           )
                               )
                           ),
                     
                           box(background = "light-blue",
                               title = "Mental Disorder", width = 12,
                               selectInput("disorder_selection", "Disorder",
                                           choices = c("none" = 0,
                                             "Major Depressive" = 1, 
                                                       "Panic Disorder" = 2,
                                                       "PTSD" = 3,
                                                       "Obsessive Compulsive Disorder" = 4,
                                                       "Substance Use Disorder" = 5,
                                                       "Social Anxiety Disorder" = 6,
                                                       "Psychotic Disorder" = 7,
                                                       "Antisocial Personality" = 8,
                                                       "Epilepsy" = 9,
                                                       "Bipolar Disorder" = 10
                                           ),
                                           multiple = TRUE,
                                           selectize = TRUE # Allow multiple selections
                               )
                           ),
                           box(background = "light-blue",
                               actionButton("submit", "Submit")
                           ),
                           box(background = "light-blue",
                               title = "Selected Information", width = 12,
                               textOutput("selected_disorders_display"),
                               textOutput("selected_age_group_display"),
                               textOutput("selected_gender_display")  # Output for gender
                           ),
                        
            )
    ),
    
    tabItem(tabName = "data",
            titlePanel(h2("Recorded Data", style = "font-size: 36px; text-align: center; font-weight: bold;")),
            fluidRow(
              box(title = "Login", width = 12, solidHeader = TRUE, background = "light-blue",
                  textInput("username", "Username"),
                  passwordInput("password", "Password"),
                  actionButton("login", "Login")
              ),
              box(title = "Data Table", width = 12, solidHeader = TRUE, background = "light-blue",
                  tableOutput("table_mentalhealth")  # Ensure this output is defined
              )
            )
    )
  
    
    )
       )
    )


   