#' UI for Shiny App for Splitting Strata with Optimum Allocation
#' @return Creates the UI for the Shiny app that is loaded with
#' \code{optimall_shiny}.
#' @export

shiny_ui <- function(){

  shiny::fluidPage(

  # Application title
  shiny::titlePanel("Splitting Strata with Optimum Allocation"),

  # Set Application Theme
  theme = bslib::bs_theme(version = 4, bootswatch = "spacelab",
                          secondary = "#446E9B"),

  # Data input and describe its columns
  # radio buttons from test result
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput("data", "Choose CSV File",
                multiple = FALSE,
                accept = c(".csv",
                           ".rds",
                           ".rda")),
      shiny::uiOutput("strata_output"),
      shiny::uiOutput("split_var_output"),
      shiny::uiOutput("y_output"),
      shiny::uiOutput("strata_to_split_output"),
      shiny::uiOutput("type_output"),
      shiny::uiOutput("split_at_output"),
      shiny::uiOutput("allocation_output"),
      shiny::uiOutput("key_output"),
      shiny::uiOutput("nsample_output"),
      shiny::actionButton("confirm", "Confirm Split"),
      shiny::actionButton("reset", "Reset")
    ),

    # Show table with output of optimum_allocation
    shiny::mainPanel(
      DT::dataTableOutput("datatable"),
      shiny::textOutput("list")
    )
  )
)
}

#' Server logic for Interactive Shiny for Optimall.
#' @param input input for Shiny server.
#' @param output output for by Shiny server.
#' @param session session for Shiny server.
#' @return Defines server logic for Shiny app that can be loaded with
#' \code{optimall_shiny()}.
#' @export

shiny_server <- function(input, output, session) {
  #Take the inputted data and call it df
  values <- shiny::reactiveValues(df_data = NULL)
  shiny::observeEvent(input$data, {
    path <- input$data$datapath
    pathext <- substr(path, nchar(path) - 3, nchar(path))
    if (pathext == ".csv"){
      values$df_data <- utils::read.csv(input$data$datapath)
    } else if (pathext == ".rds"){
      values$df_data <- readRDS(input$data$datapath)
    } else if (pathext == ".rda"){
      env <- new.env(parent = emptyenv())
      dat <- load(input$data$datapath, envir = env)[1]
      values$df_data <- env[[dat]]
    }
  })
  #Render the reactive UI

  #Add option for allocate wave and store it as a reactive value
  output$allocation_output <- shiny::renderUI({
    shiny::req(input$data)
    shiny::radioButtons(inputId = "allocation",
                        label = "Include Information from Previous Wave",
                        list("optimum_allocation","allocate_wave
                              (have some units already been sampled?)"),)
  })
  type_vals <- shiny::reactiveValues()
  type_vals$func <- "optimum_allocation"
  shiny::observeEvent(input$allocation, {type_vals$func <- input$allocation})

  #Now add the choice to select which column specifies the wave.
  output$key_output <- shiny::renderUI({
    shiny::req(input$data)
    if(type_vals$func != c("optimum_allocation")){
      shiny::selectInput(inputId = "key",
                         label = "Column Indicating Which Have Already
                         Been Sampled (Y/N or 1/0):",
                         choices = names(values$df_data))
    } else{
      NULL
    }
  })

  output$strata_output <- shiny::renderUI({
    shiny::req(input$data)
    shiny::selectInput(inputId = "strata",
                       label = "Column Holding Strata",
                       choices = names(values$df_data))
  })
  output$split_var_output <- shiny::renderUI({
    shiny::req(input$data)
    shiny::selectInput(inputId = "split_var",
                       label = "Column Holding Variable to Split On",
                       choices = names(values$df_data))
  })
  output$y_output <- shiny::renderUI({
    shiny::req(input$data)
    shiny::selectInput(inputId = "y",
                       label = "Column Holding Variable of Interest",
                       choices = names(values$df_data))
  })
  output$strata_to_split_output <- shiny::renderUI({
    shiny::req(input$data)
    shiny::selectInput(inputId = "strata_to_split",
                       label = "Name of Strata to Split",
                       choices = c("All", sort(unique(
                       dplyr::select(values$df_data,
                                     input$strata)[,input$strata]))))
  })
  output$type_output <- shiny::renderUI({
    shiny::req(input$data)
    shiny::radioButtons(inputId = "type",
                        label = "Split Type",
                        list("global quantile","local quantile",
                              "value", "categorical"))
  })
  output$nsample_output <- shiny::renderUI({
    shiny::req(input$data)
    shiny::numericInput("nsample", "n to sample", 10)
  })
  #Change the parameters of split_at based on the type
  type_vals$type <- "global quantile"

  shiny::observeEvent(input$type, {type_vals$type <- input$type})
  #observe(input$type == "global quantile",{slidertype$type <-
  #"global quantile"})
  #observe(input$type == "local quantile", {slidertype$type <-
  #"local quantile"})
  #observe(input$type == "value", {slidertype$type <- "value"})
  #observe(input$type == "categorical", {slidertype$type <-
  #"categorical"})

  output$split_at_output <- shiny::renderUI({
    shiny::req(input$data)
    if(type_vals$type %in% c("global quantile","local quantile")){
      shiny::sliderInput(inputId = "split_at", label = "Split At",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         step = 0.01)
    }
    else if(type_vals$type == "value"){
      shiny::sliderInput(inputId = "split_at", label = "Split At",
                         min = min(dplyr::select(values$df_data,
                                                 input$split_var)[,1]),
                         max = max(dplyr::select(values$df_data,
                                                 input$split_var)[,1]),
                         value = max(dplyr::select(values$df_data,
                                                   input$split_var)[,1]),
                         step = 0.01)
    }
    else{
      shiny::checkboxGroupInput(inputId = "split_at", label = "Split At:",
                                choices = c(unique(
                                  dplyr::select(values$df_data,
                                                input$split_var)[
                                                  ,input$split_var])))
    }
  })

  strata_to_split <- shiny::reactiveValues(split = NULL)
  shiny::observeEvent(input$strata_to_split, {
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
    output_df <- optimall::split_strata(data = values$df_data,
                                        strata = input$strata,
                                        split = strata_to_split$split,
                                        split_var = input$split_var,
                                        type = input$type,
                                        split_at = input$split_at)
    if(type_vals$func == "optimum_allocation"){
      output_df <- optimall::optimum_allocation(data = output_df,
                                                strata = "new_strata",
                                                y = input$y,
                                                nsample = input$nsample,
                                                ndigits = 4,
                                                allow.na = TRUE)
    } else {
      output_df <- optimall::allocate_wave(data = output_df,
                                           strata = "new_strata",
                                           y = input$y,
                                           nsample = input$nsample,
                                           already_sampled = input$key,
                                           method = "simple")
    }
    DT::datatable(output_df, options = list(pageLength = 25))
  })

  #Make reactiveVal that says where to start list of code commands.
  startVal <- shiny::reactiveVal()
  startVal(1)

  #What happens when you confirm your split. 1: output_df updates
  #and 2: code for the split pops up.
  myValues <- shiny::reactiveValues(dList = NULL)
  shiny::observeEvent(input$confirm,{
    temp <- optimall::split_strata(data = values$df_data,
                                   strata = input$strata,
                                   split = strata_to_split$split ,
                                   split_var = input$split_var,
                                   type = input$type,
                                   split_at = input$split_at)
    if(type_vals$type != "categorical"|
       (type_vals$type == "categorical" & length(input$split_at) == 1)){
      myValues$dList <- c(shiny::isolate(myValues$dList),
                          shiny::isolate(paste0(
                            "split_strata(data = 'df_name', strata = '",
                            input$strata,"', split = ",
                            ifelse(is.null(strata_to_split$split),
                                   "NULL",
                                   paste0("'",
                                          strata_to_split$split
                                          ,"'")),
                            ", split_var = '",input$split_var,
                            "', type = '",input$type,
                            "', split_at = ",input$split_at,")")))
    }
    if(type_vals$type == "categorical" & length(input$split_at) > 1){
      split_cat_text <- paste0("c('",
                               glue::glue_collapse(input$split_at,
                                                   sep = "','"),"')")
      myValues$dList <- c(shiny::isolate(myValues$dList),
                          shiny::isolate(paste0(
                            "split_strata(data = 'df_name',
                            strata = '",
                            input$strata,"', split = ",
                            ifelse(is.null(strata_to_split$split),
                                   "NULL",
                                   paste0("'",
                                          strata_to_split$split
                                          ,"'")),
                            ", split_var = '",input$split_var,
                            "', type = '",input$type,
                            "', split_at = ",split_cat_text,")")))
    }
    output$list<-shiny::renderPrint({
      c(myValues$dList[startVal():length(myValues$dList)])
    })
    values$df_data <- temp
  })

  #Reset button resets df, clears changes in text, and updates startVal
  #So code from before reset will no longer appear.
  shiny::observeEvent(input$reset,{
    output_df <- NULL
    path <- input$data$datapath
    pathext <- substr(path, nchar(path) - 3, nchar(path))
    if (pathext == ".csv"){
      values$df_data <- utils::read.csv(input$data$datapath)
    } else if (pathext == ".rds"){
      values$df_data <- readRDS(input$data$datapath)
    } else if (pathext == ".rda"){
      env <- new.env(parent = emptyenv())
      dat <- load(input$data$datapath, envir = env)[1]
      values$df_data <- env[[dat]]
    }
    startVal((length(myValues$dList)) + 1)
    output$list <- shiny::renderPrint({
      myValues <- NULL
    })
  })
}

#' Run the shiny application
#'
#' Launches an R Shiny application locally. This app can be used to
#' interactively split strata and determine how the results affect
#' optimum allocation of a fixed number of samples. It accepts
#' .csv and .rds files as well as .rda files that contain a single
#' dataset. See vignette titled "Splitting Strata with Optimall Shiny"
#' for more information.
#' @param ... Optional arguments to pass to \code{shiny::runApp}.
#' \code{display.mode} is already set to normal.
#' @return Launches an R Shiny application locally.
#' @export
optimall_shiny <- function( ...){
  if (! requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install shiny: install.packages('shiny')",
         call. = FALSE)}
  if (! requireNamespace("DT", quietly = TRUE)) {
    stop("Please install DT: install.packages('DT')",
         call. = FALSE)}
  if (! requireNamespace("bslib", quietly = TRUE)) {
    stop("Please install bslib: install.packages('bslib')",
         call. = FALSE)}
  app <- shiny::shinyApp(ui = shiny_ui, server = shiny_server)
  shiny::runApp(app, ...)
}
