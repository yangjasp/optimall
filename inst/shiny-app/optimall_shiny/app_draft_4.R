#Interactive Shiny for Optimall. Adds on to Draft 3 by adding option for allocate wave.

library(shiny)
library(DT)
library(optimall)
library(glue)

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
      uiOutput("allocation"),
      uiOutput("key"),
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

  #Add option for allocate wave and store it as a reactive value
  output$allocation <- renderUI({
    shiny::req(input$data)
    radioButtons(inputId = "allocation",
                 label = "Include Information from Previous Wave",
                 list("optimum_allocation","allocate_wave (have some units already been sampled?)"),)
  })
  type_vals <- reactiveValues()
  type_vals$func <- "optimum_allocation"
  observeEvent(input$allocation, {type_vals$func <- input$allocation})

  #Now add the choice to select which column specifies the wave.
  output$key <- renderUI({
    shiny::req(input$data)
    if(type_vals$func != c("optimum_allocation")){
      selectInput(inputId = "key", label = "Column Indicating Which Have Already Been Sampled (Y/N or 1/0):",
                  choices = names(values$df_data))
    } else{
      NULL
    }
  })

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
                choices = c("All", sort(unique(dplyr::select(values$df_data, input$strata)[,input$strata]))))
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
  type_vals$type <- "global quantile"

  observeEvent(input$type, {type_vals$type <- input$type})
  #observe(input$type == "global quantile",{slidertype$type <- "global quantile"})
  #observe(input$type == "local quantile", {slidertype$type <- "local quantile"})
  #observe(input$type == "value", {slidertype$type <- "value"})
  #observe(input$type == "categorical", {slidertype$type <- "categorical"})

  output$split_at <- renderUI({
    shiny::req(input$data)
    if(type_vals$type %in% c("global quantile","local quantile")){
      sliderInput(inputId = "split_at", label = "Split At:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.01)
    }
    else if(type_vals$type == "value"){
      sliderInput(inputId = "split_at", label = "Split At",
                  min = min(dplyr::select(values$df_data, input$split_var)[,1]),
                  max = max(dplyr::select(values$df_data, input$split_var)[,1]),
                  value = max(dplyr::select(values$df_data, input$split_var)[,1]),
                  step = 0.01)
    }
    else{
      checkboxGroupInput(inputId = "split_at", label = "Split At:",
                         choices = c(unique(dplyr::select(values$df_data, input$split_var)[,input$split_var])))
    }
  })

  strata_to_split <- reactiveValues(split = NULL)
  observeEvent(input$strata_to_split, {
    shiny::req(input$data)
    if(input$strata_to_split == "All"){
      strata_to_split$split <- NULL
    } else{
      strata_to_split$split <- input$strata_to_split
    }
  })

  #Render the datatable with the optimum_allocation output.
  output$datatable <- DT::renderDataTable({
    shiny::req(input$data)
    shiny::req(input$y)
    shiny::req(input$split_var)
    shiny::req(input$strata)
    shiny::req(input$split_at)
    output_df <- optimall::split_strata(data = values$df_data, strata = input$strata, split = strata_to_split$split ,split_var = input$split_var, type = input$type, split_at = input$split_at)
    if(type_vals$func == "optimum_allocation"){
    output_df <- optimall::optimum_allocation(data = output_df, strata = "new_strata", y = input$y, nsample = input$nsample, allow.na = T)
    } else {
    output_df <- optimall::allocate_wave(data = output_df, strata = "new_strata", y = input$y, nsample = input$nsample, wave2a = input$key)
    }
    output_df
  })


  #What happens when you confirm your split. 1: output_df updates and 2: code for the split pops up.
  myValues <- reactiveValues()
  observeEvent(input$confirm,{
    temp <- optimall::split_strata(data = values$df_data, strata = input$strata, split = strata_to_split$split ,split_var = input$split_var, type = input$type, split_at = input$split_at)
    if(type_vals$type != "categorical"|
       (type_vals$type == "categorical" & length(input$split_at) == 1)){
      myValues$dList <- c(isolate(myValues$dList), isolate(paste0("split_strata(data = 'df_name', strata = '",input$strata,"', split = ",ifelse(is.null(strata_to_split$split), "NULL", paste0("'",strata_to_split$split,"'")),", split_var = '",input$split_var,"', type = '",input$type,"', split_at = '",input$split_at,"')")))
    }
    if(type_vals$type == "categorical" & length(input$split_at) > 1){
      split_cat_text <- paste0("c('", glue::glue_collapse(input$split_at, sep = "','"), "')")
      myValues$dList <- c(isolate(myValues$dList), isolate(paste0("split_strata(data = 'df_name', strata = '",input$strata,"', split = ",ifelse(is.null(strata_to_split$split), "NULL", paste0("'",strata_to_split$split,"'")),", split_var = '",input$split_var,"', type = '",input$type,"', split_at = ",split_cat_text,")")))
    }
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
