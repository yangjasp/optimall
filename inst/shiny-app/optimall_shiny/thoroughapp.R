#Interactive Shiny for Optimall. Round 2

library(shiny)
library(DT)
library(optimall)

data <- datasets::iris


# Splitting Strata with Optimal Allocation
ui <- fluidPage(
  
  # Application title
  titlePanel("Splitting Strata with Optimum Allocation"),
  
  # Data input and describe its columns
  # radio buttons from test result
  sidebarLayout(
    sidebarPanel(
      fileInput("data", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("strata"),
      uiOutput("split_var"),
      uiOutput("y"),
      uiOutput("strata_to_split"),
      uiOutput("type"),
      uiOutput("split_at"),
      uiOutput("nsample"),
      actionButton("confirm", "Confirm Split"),
      actionButton("reset", "Reset")
    ),
    
    # Show table with output of optimum_allocation
    mainPanel(
      DT::dataTableOutput("datatable"),
      textOutput("list")
    )
  )
)


# Define server logic required to produce table
server <- function(input, output, session) {
  #Take the inputted data and call it df
  values <- reactiveValues(df_data = NULL)
  observeEvent(input$data, {
    values$df_data <- read.csv(input$data$datapath)
  })
  #Render the reactive UI
  output$strata <- renderUI({
    shiny::req(input$data)
    selectInput(inputId = "strata", 
                label = "Column Holding Strata", 
                choices = names(values$df_data))
  })
  output$split_var <- renderUI({
    shiny::req(input$data)
    selectInput(inputId = "split_var", 
                label = "Column Holding Variable to Split On", 
                choices = names(values$df_data))
  })
  output$y <- renderUI({
    shiny::req(input$data)
    selectInput(inputId = "y", 
                label = "Column Holding Variable of Interest", 
                choices = names(values$df_data))
  })  
  output$strata_to_split <- renderUI({
    shiny::req(input$data)
    selectInput(inputId = "strata_to_split",
                label = "Name of Strata to Split",
                choices = c("All", unique(dplyr::select(values$df_data, input$strata))))
  })
  output$type <- renderUI({
    shiny::req(input$data)
    radioButtons(inputId = "type", 
                 label = "Split Type:",
                 list("global quantile","local quantile", "value", "categorical"))
  })
  output$nsample <- renderUI({
    shiny::req(input$data)
    numericInput("nsample", "n to sample", 10)
  })
  #Change the parameters of split_at based on the type
  observeEvent(input$type,{
    shiny::req(input$data)
    output$split_at <- 
      renderUI({
        sliderInput(inputId = "split_at", label = "Split At",
                    min = min(dplyr::select(values$df_data, input$split_var)[,1]),
                    max = max(dplyr::select(values$df_data, input$split_var)[,1]),
                    value = max(dplyr::select(values$df_data, input$split_var)[,1]),
                    step = 0.01)
      })
  })
  observeEvent(input$type %in% c("global quantile", "local quantile"),{
    shiny::req(input$data)
    output$split_at <- 
      renderUI({
        sliderInput(inputId = "split_at", label = "Split At:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.01)
      })
  })

  #if(input$type == "categorical"){
  #checkboxGroupInput(inputId = "split_at", label = "Split At:",
  #                  choices = unique(dplyr::select(df(), input$split_var)))
  #}
  ##})
  
  #Render the datatable with the optimum_allocation output.
  output$datatable <- DT::renderDataTable({
    shiny::req(input$data)
    shiny::req(input$y)
    shiny::req(input$split_var)
    shiny::req(input$strata)
    shiny::req(input$split_at)
    output_df <- optimall::split_strata(data = values$df_data, strata = input$strata, split = input$strata_to_split ,split_var = input$split_var, type = input$type, split_at = input$split_at)
    output_df <- optimall::optimum_allocation(data = output_df, strata = "new_strata", y = input$y, nsample = input$nsample)
    output_df
  })
  
  
  #What happens when you confirm your split. 1: output_df updates and 2: code for the split pops up.
  myValues <- reactiveValues()
  observeEvent(input$confirm,{
    temp <- optimall::split_strata(data = values$df_data, strata = input$strata, split = input$strata_to_split ,split_var = input$split_var, type = input$type, split_at = input$split_at)
    myValues$dList <- c(isolate(myValues$dList), isolate(paste0("split_strata(data = 'df_name', strata = ",input$strata,", split = ",input$strata_to_split,", split_var = ",input$split_var,", type = ",input$type,", split_at = ",input$split_at,")")))
    output$list<-renderPrint({
      myValues$dList
    })
    values$df_data <- temp
  })
  
  #Reset button resets df and clears changes in text
  dfy <- observeEvent(input$reset,{
    output_df <- df()
    myValues$dlist <- reactiveValues()
    output$list <- renderPrint({
      myValues <- NULL
    })
  })
}       

# Run the application 
shinyApp(ui = ui, server = server)
