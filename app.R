#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# calc AICc for rest models
# calculate BIC for all models
# 

library(shiny)
library(car)
library(caTools)
library(MuMIn)
library(olsrr)


ui <- fluidPage(
    
    # Application title
    titlePanel("Algorithm Application"),
    
    
    # Sidebar  
    sidebarLayout(
        sidebarPanel(
            helpText("This is application of some algorithms on", em("iris"), "dataset."),
            navlistPanel(
                tabPanel("Dataset",
                         selectInput("data","Select what you want to see",c("Data Description","Data Preview","Data Structure","Data Summary"),selected = NULL),
                         #Dataset tab
                         conditionalPanel(condition ="input.data=='Data Description'",
                                          h3("A few words about dataset:"),
                                          textOutput("data_desc_1")),
                         br(),
                         conditionalPanel(condition ="input.data=='Data Description'",
                                          textOutput("data_desc_2")),
                         br(),
                         conditionalPanel(condition ="input.data=='Data Description'",
                                          textOutput("data_desc_3")),
                         
                         conditionalPanel(condition = "input.data=='Data Preview'",
                                          numericInput(inputId = "obs",
                                                       label = "Number of observations to view:",
                                                       value = 10),
                                          h3("Dataset:"),
                                          tableOutput("view"),
                                          
                                          tableOutput("data_prev")),
                         conditionalPanel(condition = "input.data=='Data Structure'",
                                          h3("Structure of dataset iris:"),
                                          verbatimTextOutput("data_str")),
                         conditionalPanel(condition = "input.data=='Data Summary'",
                                          h3("Summary of dataset iris:"),
                                          verbatimTextOutput("data_summ")),
                         
                         
                         
                ),
                
                tabPanel("Linear Regression",
                         p(strong("We will see Linear Regression Model fitting on iris dataset.")),
                         p("You can check fitted linear models considering various set of independent variables listed below. Also various test values can be calculated and plots can be to drawn to check which model, satisfying what criterias to be considered as final model."),
                         helpText("We are using", em(strong("Sepal.Length")), "as", em(strong("dependent variable")),"and select from the list below which set of independent variables you want to use for bulding",em(strong("Linear Regression Model."))),
                         selectInput("chk","Select the independent variable(s) of model",c("Sepal.Width","Petal.Length","Petal.Width","Species","Sepal.Width,Petal.Length","Sepal.Width,Petal.Width","Sepal.Width,Species","Petal.Length,Petal.Width","Petal.Length,Species","Petal.Width,Species","Sepal.Width,Petal.Length,Petal.Width","Sepal.Width,Petal.Length,Species","Sepal.Width,Petal.Width,Species","Petal.Length,Petal.Width,Species","Sepal.Width,Petal.Length,Petal.Width,Species"),selected = NULL),
                         
                         #Linear Regression tab
                         #model
                         h3("The model is:"),
                         conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                          verbatimTextOutput("m1")),
                         conditionalPanel(condition = "input.chk=='Petal.Length'",
                                          verbatimTextOutput("m2")),
                         conditionalPanel(condition = "input.chk=='Petal.Width'",
                                          verbatimTextOutput("m3")),
                         conditionalPanel(condition = "input.chk=='Species'",
                                          verbatimTextOutput("m4")),
                         conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                          verbatimTextOutput("m5")),
                         conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                          verbatimTextOutput("m6")),
                         conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                          verbatimTextOutput("m7")),
                         conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                          verbatimTextOutput("m8")),
                         conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                          verbatimTextOutput("m9")),
                         conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                          verbatimTextOutput("m10")),
                         conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                          verbatimTextOutput("m11")),
                         conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                          verbatimTextOutput("m12")),
                         conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                          verbatimTextOutput("m13")),
                         conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                          verbatimTextOutput("m14")),
                         conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                          verbatimTextOutput("m15")),
                         
                         helpText("By next option you can see the result corresponding the above model you made."),
                         
                         selectInput("value","Select option you want to see value",c("Model Summary","AIC and AICc","VIF","Residuals vs Fitted","Normal Q-Q plot", "Durbinwatson Test", "Correlation (Actual, Prediction)", "Test Accuracy", "Visual Accuracy", "Prediction"),selected = NULL),
                         
                         
                         #Model Summary
                         conditionalPanel(condition = "input.value=='Model Summary'",
                                          h3("Model Summary is:"),
                                          
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           verbatimTextOutput("ms1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           verbatimTextOutput("ms2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           verbatimTextOutput("ms3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           verbatimTextOutput("ms4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           verbatimTextOutput("ms5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           verbatimTextOutput("ms6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           verbatimTextOutput("ms7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("ms8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           verbatimTextOutput("ms9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           verbatimTextOutput("ms10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("ms11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           verbatimTextOutput("ms12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           verbatimTextOutput("ms13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("ms14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("ms15")),
                                          
                                          conditionalPanel(condition = "input.value=='Model Summary'",
                                                           p("You can check the", em(strong("Adjusted R-square Value")),"for different combinations of independent variables and decide for combinations of", em(strong("statistically significant variables")),"from model summary.")
                                          )),
                         
                         
                         
                         #AIC
                         conditionalPanel(condition = "input.value=='AIC and AICc'",
                                          h3("AIC and AICc Value:"),
                                          conditionalPanel(condition = "input.value=='AIC and AICc'",
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                                            verbatimTextOutput("AIC1")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                                            verbatimTextOutput("AIC2")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                                            verbatimTextOutput("AIC3")),
                                                           conditionalPanel(condition = "input.chk=='Species'",
                                                                            verbatimTextOutput("AIC4")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                                            verbatimTextOutput("AIC5")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                                            verbatimTextOutput("AIC6")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                                            verbatimTextOutput("AIC7")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                                            verbatimTextOutput("AIC8")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                                            verbatimTextOutput("AIC9")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                                            verbatimTextOutput("AIC10")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                                            verbatimTextOutput("AIC11")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                                            verbatimTextOutput("AIC12")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                                            verbatimTextOutput("AIC13")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                                            verbatimTextOutput("AIC14")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                                            verbatimTextOutput("AIC15")))
                                          
                         ),
                         
                         
                         
                         
                         
                         #VIF
                         conditionalPanel(condition = "input.value=='VIF'",
                                          h3("VIF value:"),
                                          conditionalPanel(condition = "input.value=='VIF'",
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                                            textOutput("VIF1")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                                            textOutput("VIF2")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                                            textOutput("VIF3")),
                                                           conditionalPanel(condition = "input.chk=='Species'",
                                                                            textOutput("VIF4")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                                            verbatimTextOutput("VIF5")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                                            verbatimTextOutput("VIF6")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                                            verbatimTextOutput("VIF7")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                                            verbatimTextOutput("VIF8")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                                            verbatimTextOutput("VIF9")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                                            verbatimTextOutput("VIF10")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                                            verbatimTextOutput("VIF11")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                                            verbatimTextOutput("VIF12")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                                            verbatimTextOutput("VIF13")),
                                                           conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                                            verbatimTextOutput("VIF14")),
                                                           conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                                            verbatimTextOutput("VIF15")))
                                          
                         ),
                         
                         
                         #Durbinwatson Test
                         conditionalPanel(condition = "input.value=='Durbinwatson Test'",
                                          h3("Durbinwatson Test for the above model is:"),
                                          
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           verbatimTextOutput("d1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           verbatimTextOutput("d2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           verbatimTextOutput("d3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           verbatimTextOutput("d4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           verbatimTextOutput("d5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           verbatimTextOutput("d6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           verbatimTextOutput("d7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("d8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           verbatimTextOutput("d9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           verbatimTextOutput("d10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("d11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           verbatimTextOutput("d12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           verbatimTextOutput("d13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("d14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("d15")),
                                          
                                          conditionalPanel(condition = "input.value=='Durbinwatson Test'",
                                                           p("'Durbinwatson Test' checks",span(em(strong("residuals of fitted model are autocorrelated or not.")))))
                                          
                         ),
                         
                         
                         #Residuals vs Fitted
                         conditionalPanel(condition = "input.value=='Residuals vs Fitted'",
                                          h3("Residuals vs Fitted plot of the above model is:"),
                                          
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           plotOutput("r1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           plotOutput("r2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           plotOutput("r3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           plotOutput("r4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           plotOutput("r5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           plotOutput("r6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           plotOutput("r7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           plotOutput("r8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           plotOutput("r9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           plotOutput("r10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           plotOutput("r11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           plotOutput("r12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           plotOutput("r13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           plotOutput("r14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           plotOutput("r15")),
                                          
                                          conditionalPanel(condition = "input.value=='Residuals vs Fitted'",
                                                           p("We can check that", em(strong("residuals are homoscedastic or not")),"from graph 'Residuals vs Fitted values'."))
                                          
                         ),
                         
                         #Normal Q-Q plot
                         conditionalPanel(condition = "input.value=='Normal Q-Q plot'",
                                          h3("Normal Q-Q plot of the above model is:"),
                                          
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           plotOutput("q1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           plotOutput("q2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           plotOutput("q3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           plotOutput("q4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           plotOutput("q5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           plotOutput("q6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           plotOutput("q7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           plotOutput("q8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           plotOutput("q9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           plotOutput("q10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           plotOutput("q11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           plotOutput("q12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           plotOutput("q13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           plotOutput("q14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           plotOutput("q15")),
                                          
                                          
                                          conditionalPanel(condition = "input.value=='Normal Q-Q plot'",
                                                           p("From Q-Q plot, we can check that",em(strong("residuals are normally distributed or not."))))
                                          
                                          
                         ),
                         
                         
                         #Correlation (Actual, Prediction)
                         conditionalPanel(condition = "input.value=='Correlation (Actual, Prediction)'",
                                          h3("Correlation between actual and prediction of the above model is:"),
                                          
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           verbatimTextOutput("c1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           verbatimTextOutput("c2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           verbatimTextOutput("c3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           verbatimTextOutput("c4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           verbatimTextOutput("c5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           verbatimTextOutput("c6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           verbatimTextOutput("c7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("c8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           verbatimTextOutput("c9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           verbatimTextOutput("c10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("c11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           verbatimTextOutput("c12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           verbatimTextOutput("c13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("c14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("c15")),
                                          
                                          conditionalPanel(condition = "input.value=='Correlation (Actual, Prediction)'",
                                                           p("This gives the",em(strong("correlation")), "between actuals and predicted values in test part.")
                                                           )
                                          
                         ),
                         
                         
                         # Test Accuracy
                         conditionalPanel(condition = "input.value=='Test Accuracy'",
                                          h3("Test Accuracy of the above model is:"),
                                          
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           verbatimTextOutput("testacc1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           verbatimTextOutput("testacc2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           verbatimTextOutput("testacc3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           verbatimTextOutput("testacc4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           verbatimTextOutput("testacc5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           verbatimTextOutput("testacc6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           verbatimTextOutput("testacc7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("testacc8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           verbatimTextOutput("testacc9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           verbatimTextOutput("testacc10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           verbatimTextOutput("testacc11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           verbatimTextOutput("testacc12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           verbatimTextOutput("testacc13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("testacc14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           verbatimTextOutput("testacc15")),
                                          
                                          conditionalPanel(condition = "input.value=='Test Accuracy'",
                                                           p("This gives the",em(strong("accuracy on test data")), "with the selected model.")
                                          )
                                          
                         ),
                         
                         
                         
                         
                         #Visual Accuracy
                         conditionalPanel(condition = "input.value=='Visual Accuracy'",
                                          h3("Here we plot fitted over actual values:"),
                                          
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           plotOutput("v1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           plotOutput("v2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           plotOutput("v3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           plotOutput("v4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           plotOutput("v5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           plotOutput("v6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           plotOutput("v7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           plotOutput("v8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           plotOutput("v9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           plotOutput("v10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           plotOutput("v11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           plotOutput("v12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           plotOutput("v13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           plotOutput("v14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           plotOutput("v15")),
                                          
                                          conditionalPanel(condition = "input.value=='Visual Accuracy'",
                                                           p("From here, we can check that",em(strong("how much closure")), "the",em(strong("fitted and actual values are."))))
                                          
                         ),
                         
                         
                         # Prediction
                         conditionalPanel(condition = "input.value == 'Prediction'",
                                          helpText("Please put input for which you want to see predicted value of Sepal.Length. The selected model will be used."),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width'",
                                                           numericInput("sw1","Enter value for Sepal.Width", value = 0),
                                                           verbatimTextOutput("p1")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length'",
                                                           numericInput("sw2","Enter value for Petal.Length", value = 0),
                                                           verbatimTextOutput("p2")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width'",
                                                           numericInput("sw3","Enter value for Petal.Width", value = 0),
                                                           verbatimTextOutput("p3")),
                                          conditionalPanel(condition = "input.chk=='Species'",
                                                           selectInput("sw4","Enter value for Species", , choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p4")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length'",
                                                           numericInput("sw5","Enter value for Sepal.Width", value = 0),
                                                           numericInput("sw6","Enter value for Petal.Length", value = 0),
                                                           verbatimTextOutput("p5")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width'",
                                                           numericInput("sw7","Enter value for Sepal.Width", value = 0),
                                                           numericInput("sw8","Enter value for Petal.Width", value = 0),
                                                           verbatimTextOutput("p6")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Species'",
                                                           numericInput("sw9","Enter value for Sepal.Width", value = 0),
                                                           selectInput("sw10","Enter value for Species", , choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p7")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width'",
                                                           numericInput("sw11","Enter value for Petal.Length", value = 0),
                                                           numericInput("sw12","Enter value for Petal.Width", value = 0),
                                                           verbatimTextOutput("p8")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Species'",
                                                           numericInput("sw13","Enter value for Petal.Length", value = 0),
                                                           selectInput("sw14","Enter value for Species", , choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p9")),
                                          conditionalPanel(condition = "input.chk=='Petal.Width,Species'",
                                                           numericInput("sw15","Enter value for Petal.Width", value = 0),
                                                           selectInput("sw16","Enter value for Species", , choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p10")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width'",
                                                           numericInput("sw17","Enter value for Sepal.Width", value = 0),
                                                           numericInput("sw18","Enter value for Petal.Length", value = 0),
                                                           numericInput("sw19","Enter value for Petal.Width", value = 0),
                                                           verbatimTextOutput("p11")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Species'",
                                                           numericInput("sw20","Enter value for Sepal.Width", value = 0),
                                                           numericInput("sw21","Enter value for Petal.Length", value = 0),
                                                           selectInput("sw22","Enter value for Species", , choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p12")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Width,Species'",
                                                           numericInput("sw23","Enter value for Sepal.Width", value = 0),
                                                           numericInput("sw24","Enter value for Petal.Width", value = 0),
                                                           selectInput("sw25","Enter value for Species", , choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p13")),
                                          conditionalPanel(condition = "input.chk=='Petal.Length,Petal.Width,Species'",
                                                           numericInput("sw26","Enter value for Petal.Length", value = 0),
                                                           numericInput("sw27","Enter value for Petal.Width", value = 0),
                                                           selectInput("sw28","Enter value for Species", , choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p14")),
                                          conditionalPanel(condition = "input.chk=='Sepal.Width,Petal.Length,Petal.Width,Species'",
                                                           numericInput("sw29","Enter value for Sepal.Width", value = 0),
                                                           numericInput("sw30","Enter value for Petal.Length", value = 0),
                                                           numericInput("sw31","Enter value for Petal.Width", value = 0),
                                                           selectInput("sw32","Enter value for Species", choices = c("virginica","versicolor","setosa")),
                                                           verbatimTextOutput("p15"))
                         )
                         
                         
                )
                
                
            ),width=9
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            helpText(em("Here you can see some extra information about model selection procedure for linear regression, varities types of tests, which to use for what purpose. Just click on the corresponding radio button.")),
            radioButtons("extra_info_l","Linear Regression ",c("R-square and Adjusted R-square","Statistically Significant Variable","AIC and AICc value","VIF value","Residuals vs Fitted value plot","Normal Q-Q plot", "Durbinwatson Test"),selected = character(0)),
            
            
            
            
            conditionalPanel(condition="input.extra_info_l=='R-square and Adjusted R-square'",
                             textOutput("text_3")),
            conditionalPanel(condition="input.extra_info_l=='Statistically Significant Variable'",
                             textOutput("text_4")),
            
            conditionalPanel(condition="input.extra_info_l=='AIC and AICc value'",
                             textOutput("text_6")),
            
            conditionalPanel(condition="input.extra_info_l=='VIF value'",
                             textOutput("text_8")),
            
            conditionalPanel(condition="input.extra_info_l=='Residuals vs Fitted value plot'",
                             textOutput("text_10")),
            conditionalPanel(condition="input.extra_info_l=='Normal Q-Q plot'",
                             textOutput("text_11")),
            conditionalPanel(condition="input.extra_info_l=='Durbinwatson Test'",
                             textOutput("text_12")),
            
            
            
            width=3,
            br(),
            helpText(tags$u(strong("Conclusion:")), "Here after considering AICc, AIC, VIF, Linearity, Heteroscadasticity and Autocorrelation in residuals, Adjusted R-square and Results against Test set combindly, we will select features sets", strong("Sepal.Width, Petal.Length, Petal.Width"),"as Baseline Model.")
        )
        
        
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    output$text_3<-renderText({
        "Whenever new variable is included then R^2 will increase. But Adjusted R^2 will increase only if the new added variable have some significant amount of influence over dependent variable. So, we should check Adjusted R^2 to make the model parsimonious."
    })
    output$text_4<-renderText({
        "The variable which has quite amount of influence in predicting dependent variable proven by some statistical test."
    })
    
    output$text_6<-renderText({
        "Full form of AIC: Akaike Information Criteria. AIC is a measure of how well the model will fit the new data. It is an estimator of prediction error, thereby estimates the relative amount of information lost by a given model. The less information a model losses i.e. the less the AIC  value, the higher the quality of the model. AICc is AIC value corrected for small sample sizes, which is approximately same as AIC for large sample sizes. The criterion for selecting best model based on AICc is same as AIC"
    })
    
    output$text_8<-renderText({
        "Full form: Variance Inflation Factor. This measures how much variance of an estimated regression coefficient increases if predictors/independent variables are correlated. Rule of thumb is to remove predictor with vif >10. VIF around 1 is indication of using that variable in the model."
    })
    
    output$text_10<-renderText({
        "Residuals are homoscedastic if variation is more or less constant over fitted values."
    })
    output$text_11<-renderText({
        "This plot has quantiles of a sample from normal distribution at x axis and quantiles of residuals at y axis. If more or less straight line is formed, the residuals are normally distributed"
    })
    output$text_12<-renderText({
      "This test helps in determining presence of autocorrelation in the residuals. Test Statistic value should be 2 for ideal case."
    })
    
    
    
    
    
    
    
    #Dataset tab
    output$data_desc_1<-renderText({
        "This is inbulit dataset."
    })
    output$data_desc_2<-renderText({
        "
        This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
    })
    output$data_desc_3<-renderText({
        "
        iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."
    })
    output$view <- renderTable({
        head(iris, n = input$obs)
    })
    
    output$data_str<-renderPrint({
        str(iris)
    })
    output$data_summ<-renderPrint({
        summary(iris)
    })
    
    
    
    #Linear Regression tab
    
    
    set.seed(123)
    split<-sample.split(iris$Sepal.Length,SplitRatio = 0.7)
    split
    
    
    iris_train<-subset(iris,split==TRUE)
    iris_test<-subset(iris,split==FALSE)
    
    
    model1<-lm(Sepal.Length~Sepal.Width,data=iris_train)
    model2<-lm(Sepal.Length~Petal.Length,data=iris_train)
    model3<-lm(Sepal.Length~Petal.Width,data=iris_train)
    model4<-lm(Sepal.Length~Species,data=iris_train)
    model5<-lm(Sepal.Length~Sepal.Width+Petal.Length,data=iris_train)
    model6<-lm(Sepal.Length~Sepal.Width+Petal.Width,data=iris_train)
    model7<-lm(Sepal.Length~Sepal.Width+Species,data=iris_train)
    model8<-lm(Sepal.Length~Petal.Length+Petal.Width,data=iris_train)
    model9<-lm(Sepal.Length~Petal.Length+Species,data=iris_train)
    model10<-lm(Sepal.Length~Petal.Width+Species,data=iris_train)
    model11<-lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=iris_train)
    model12<-lm(Sepal.Length~Sepal.Width+Petal.Length+Species,data=iris_train)
    model13<-lm(Sepal.Length~Sepal.Width+Petal.Width+Species,data=iris_train)
    model14<-lm(Sepal.Length~Petal.Length+Petal.Width+Species,data=iris_train)
    model15<-lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris_train)
  
    
    #model
    output$m1<-renderPrint({
        model1
    })
    
    output$m2<-renderPrint({
        model2
    })
    output$m3<-renderPrint({
        model3
    })
    output$m4<-renderPrint({
        model4
    })
    output$m5<-renderPrint({
        model5
    })
    output$m6<-renderPrint({
        model6
    })
    output$m7<-renderPrint({
        model7
    })
    output$m8<-renderPrint({
        model8
    })
    output$m9<-renderPrint({
        model9
    })
    output$m10<-renderPrint({
        model10
    })
    output$m11<-renderPrint({
        model11
    })
    output$m12<-renderPrint({
        model12
    })
    output$m13<-renderPrint({
        model13
    })
    output$m14<-renderPrint({
        model14
    })
    output$m15<-renderPrint({
        model15
    })
    
    
    
    #Model summary
    output$ms1<-renderPrint({
        summary(model1)
    })
    
    output$ms2<-renderPrint({
        summary(model2)
    })
    output$ms3<-renderPrint({
        summary(model3)
    })
    output$ms4<-renderPrint({
        summary(model4)
    })
    output$ms5<-renderPrint({
        summary(model5)
    })
    output$ms6<-renderPrint({
        summary(model6)
    })
    output$ms7<-renderPrint({
        summary(model7)
    })
    output$ms8<-renderPrint({
        summary(model8)
    })
    output$ms9<-renderPrint({
        summary(model9)
    })
    output$ms10<-renderPrint({
        summary(model10)
    })
    output$ms11<-renderPrint({
        summary(model11)
    })
    output$ms12<-renderPrint({
        summary(model12)
    })
    output$ms13<-renderPrint({
        summary(model13)
    })
    output$ms14<-renderPrint({
        summary(model14)
    })
    output$ms15<-renderPrint({
        summary(model15)
    })
    
    
    
    
    #AIC
    output$AIC1<-renderPrint({
      aic_aicc_values <- c(AIC(model1), AICc(model1))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
      
    })
    
    output$AIC2<-renderPrint({
      aic_aicc_values <- c(AIC(model2), AICc(model2))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC3<-renderPrint({
      aic_aicc_values <- c(AIC(model3), AICc(model3))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC4<-renderPrint({
      aic_aicc_values <- c(AIC(model4), AICc(model4))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC5<-renderPrint({
      aic_aicc_values <- c(AIC(model5), AICc(model5))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC6<-renderPrint({
      aic_aicc_values <- c(AIC(model6), AICc(model6))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC7<-renderPrint({
      aic_aicc_values <- c(AIC(model7), AICc(model7))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC8<-renderPrint({
      aic_aicc_values <- c(AIC(model8), AICc(model8))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC9<-renderPrint({
      aic_aicc_values <- c(AIC(model9), AICc(model9))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC10<-renderPrint({
      aic_aicc_values <- c(AIC(model10), AICc(model10))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC11<-renderPrint({
      aic_aicc_values <- c(AIC(model11), AICc(model11))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC12<-renderPrint({
      aic_aicc_values <- c(AIC(model12), AICc(model12))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC13<-renderPrint({
      aic_aicc_values <- c(AIC(model13), AICc(model13))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC14<-renderPrint({
      aic_aicc_values <- c(AIC(model14), AICc(model14))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    output$AIC15<-renderPrint({
      aic_aicc_values <- c(AIC(model15), AICc(model15))
      names(aic_aicc_values) <- c("AIC Value", "AICc Value")
      print(aic_aicc_values)
    })
    
    
    
    
    
    
    #VIF
    output$VIF1<-renderText({
        "As VIF is measure of multicolinearity between different predictor variables , it is not valid for model containning only one variable."
    })
    
    output$VIF2<-renderText({
        "As VIF is measure of multicolinearity between different predictor variables , it is not valid for model containning only one variable."
    })
    output$VIF3<-renderText({
        "As VIF is measure of multicolinearity between different predictor variables , it is not valid for model containning only one variable."
    })
    output$VIF4<-renderText({
        "As VIF is measure of multicolinearity between different predictor variables , it is not valid for model containning only one variable."
    })
    output$VIF5<-renderPrint({
        vif(model5)
      #ols_vif_tol(model5)
    })
    output$VIF6<-renderPrint({
        vif(model6)
    })
    output$VIF7<-renderPrint({
        vif(model7)
    })
    output$VIF8<-renderPrint({
        vif(model8)
    })
    output$VIF9<-renderPrint({
        vif(model9)
    })
    output$VIF10<-renderPrint({
        vif(model10)
    })
    output$VIF11<-renderPrint({
        vif(model11)
    })
    output$VIF12<-renderPrint({
        vif(model12)
    })
    output$VIF13<-renderPrint({
        vif(model13)
    })
    output$VIF14<-renderPrint({
        vif(model14)
    })
    output$VIF15<-renderPrint({
        vif(model15)
    })
    
    
    #Durbinwatson Test
    output$d1<-renderPrint({
        durbinWatsonTest(model1)
    })
    
    output$d2<-renderPrint({
        durbinWatsonTest(model2)
    })
    output$d3<-renderPrint({
        durbinWatsonTest(model3)
    })
    output$d4<-renderPrint({
        durbinWatsonTest(model4)
    })
    output$d5<-renderPrint({
        durbinWatsonTest(model5)
    })
    output$d6<-renderPrint({
        durbinWatsonTest(model6)
    })
    output$d7<-renderPrint({
        durbinWatsonTest(model7)
    })
    output$d8<-renderPrint({
        durbinWatsonTest(model8)
    })
    output$d9<-renderPrint({
        durbinWatsonTest(model9)
    })
    output$d10<-renderPrint({
        durbinWatsonTest(lm(model10))
    })
    output$d11<-renderPrint({
        durbinWatsonTest(model11)
    })
    output$d12<-renderPrint({
        durbinWatsonTest(model12)
    })
    output$d13<-renderPrint({
        durbinWatsonTest(model13)
    })
    output$d14<-renderPrint({
        durbinWatsonTest(model14)
    })
    output$d15<-renderPrint({
        durbinWatsonTest(model15)
    })
    
    
    #Residuals vs Fitted
    output$r1<-renderPlot({
        l<-model1
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    
    output$r2<-renderPlot({
        l<-model2
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r3<-renderPlot({
        l<-model3
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r4<-renderPlot({
        l<-model4
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r5<-renderPlot({
        l<-model5
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r6<-renderPlot({
        l<-model6
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r7<-renderPlot({
        l<-model7
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r8<-renderPlot({
        l<-model8
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r9<-renderPlot({
        l<-model9
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r10<-renderPlot({
        l<-model10
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r11<-renderPlot({
        l<-model11
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r12<-renderPlot({
        l<-model12
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r13<-renderPlot({
        l<-model13
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r14<-renderPlot({
        l<-model14
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    output$r15<-renderPlot({
        l<-model15
        plot(l$fitted.values,l$residuals,xlab="Fitted values",ylab="Residuals")
    })
    
    
    #Normal Q-Q plot
    output$q1<-renderPlot({
        l<-model1
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q2<-renderPlot({
        l<-model2
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q3<-renderPlot({
        l<-model3
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q4<-renderPlot({
        l<-model4
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q5<-renderPlot({
        l<-model5
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q6<-renderPlot({
        l<-model6
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q7<-renderPlot({
        l<-model7
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q8<-renderPlot({
        l<-model8
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q9<-renderPlot({
        l<-model9
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q10<-renderPlot({
        l<-model10
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q11<-renderPlot({
        l<-model11
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q12<-renderPlot({
        l<-model12
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q13<-renderPlot({
        l<-model13
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q14<-renderPlot({
        l<-model14
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    output$q15<-renderPlot({
        l<-model15
        qqnorm(l$residuals)
        qqline(l$residuals,col='red')
    })
    
    
    
    # Correlation (Actual, Prediction)
    output$c1<-renderPrint({
        l<-model1
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]^2
        colMeans(new_data)
        new_data[2]
    })
    output$c2<-renderPrint({
        l<-model2
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c3<-renderPrint({
        l<-model3
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c4<-renderPrint({
        l<-model4
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c5<-renderPrint({
        l<-model5
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c6<-renderPrint({
        l<-model6
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c7<-renderPrint({
        l<-model7
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c8<-renderPrint({
        l<-model8
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c9<-renderPrint({
        l<-model9
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c10<-renderPrint({
        l<-model10
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c11<-renderPrint({
        l<-model11
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c12<-renderPrint({
        l<-model12
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c13<-renderPrint({
        l<-model13
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c14<-renderPrint({
        l<-model14
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    output$c15<-renderPrint({
        l<-model15
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        cor(new_data)[1,2]
    })
    
    
    # Test Accuracy
    output$testacc1<-renderPrint({
      pred<-predict(model1,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc2<-renderPrint({
      pred<-predict(model2,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc3<-renderPrint({
      pred<-predict(model3,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc4<-renderPrint({
      pred<-predict(model4,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc5<-renderPrint({
      pred<-predict(model5,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc6<-renderPrint({
      pred<-predict(model6,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc7<-renderPrint({
      pred<-predict(model7,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc8<-renderPrint({
      pred<-predict(model8,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc9<-renderPrint({
      pred<-predict(model9,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc10<-renderPrint({
      pred<-predict(model10,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc11<-renderPrint({
      pred<-predict(model11,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc12<-renderPrint({
      pred<-predict(model12,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc13<-renderPrint({
      pred<-predict(model13,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc14<-renderPrint({
      pred<-predict(model14,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    output$testacc15<-renderPrint({
      pred<-predict(model15,iris_test)
      rss<- sum((iris_test$Sepal.Length - pred)^2)
      tss<-sum((iris_test$Sepal.Length - mean(iris_test$Sepal.Length))^2)
      r_squared<- 1- (rss/tss)
      r_squared
    })
    
    
    # visual accuracy
    output$v1<-renderPlot({
        #l<-model1
        pred<-predict(model1,iris_test)
        #new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(iris_test$Sepal.Length,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v2<-renderPlot({
        l<-model2
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v3<-renderPlot({
        l<-model3
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v4<-renderPlot({
        l<-model4
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v5<-renderPlot({
        l<-model5
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v6<-renderPlot({
        l<-model6
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v7<-renderPlot({
        l<-model7
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v8<-renderPlot({
        l<-model8
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v9<-renderPlot({
        l<-model9
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v10<-renderPlot({
        l<-model10
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v11<-renderPlot({
        l<-model11
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v12<-renderPlot({
        l<-model12
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v13<-renderPlot({
        l<-model13
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v14<-renderPlot({
        l<-model14
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    output$v15<-renderPlot({
        l<-model15
        pred<-predict(l,iris_test)
        new_data<-data.frame(cbind(iris_test$Sepal.Length,pred))
        plot(new_data$V1,type="l",col="blue")
        lines(pred,type="l",col="green")
    })
    
    
    
    
    # Prediction on User Input
    output$p1<-renderPrint({
      l<-model1
      #predict(l, data.frame(Sepal.Width=c(input$sw1))) 
      l_temp <- data.frame(predict(l, data.frame(Sepal.Width=c(input$sw1))))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p2<-renderPrint({
      l<-model2
      l_temp <- predict(l, data.frame(Petal.Length=c(input$sw2)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p3<-renderPrint({
      l<-model3
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw3)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p4<-renderPrint({
      l<-model4
      l_temp <- predict(l, data.frame(Species=c(input$sw4)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p5<-renderPrint({
      l<-model5
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw5), Petal.Length=c(input$sw6)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p6<-renderPrint({
      l<-model6
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw7), Petal.Width=c(input$sw8)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p7<-renderPrint({
      l<-model7
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw9), Species=c(input$sw10)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p8<-renderPrint({
      l<-model8
      l_temp <- predict(l, data.frame(Petal.Length=c(input$sw11), Petal.Width=c(input$sw12)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p9<-renderPrint({
      l<-model9
      l_temp <- predict(l, data.frame(Petal.Length=c(input$sw13), Species=c(input$sw14)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p10<-renderPrint({
      l<-model10
      l_temp <- predict(l, data.frame(Petal.Width=c(input$sw15),Species=c(input$sw16)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p11<-renderPrint({
      l<-model11
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw17),Petal.Length=c(input$sw18),Petal.Width=c(input$sw19)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p12<-renderPrint({
      l<-model12
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw20),Petal.Length=c(input$sw21),Species=c(input$sw22)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p13<-renderPrint({
      l<-model13
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw23),Petal.Width=c(input$sw24),Species=c(input$sw25)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p14<-renderPrint({
      l<-model14
      l_temp <- predict(l, data.frame(Petal.Length=c(input$sw26),Petal.Width=c(input$sw27),Species=c(input$sw28)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    output$p15<-renderPrint({
      l<-model15
      l_temp <- predict(l, data.frame(Sepal.Width=c(input$sw29),Petal.Length=c(input$sw30),Petal.Width=c(input$sw31),Species=c(input$sw32)))
      colnames(l_temp) <- 'output'
      rownames(l_temp) <- 'output with given inputs: '
      print(l_temp)
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
