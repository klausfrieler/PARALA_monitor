#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(DT)
library(sjPlot)
library(pcalg)
library(networkD3)
library(igraph)

source("analysis.R")
source("plot_util.R")

on_server <- grepl("shiny-server", getwd())
if(on_server){
    result_dir <- "../PARALA/output/results"
} else{
    result_dir <- "data/from_server"
}

setup_workspace(result_dir)

var_choices <- setdiff(names(master), c("p_id",
                                       "session.time_started", 
                                       "time_ended", 
                                       "pilot", 
                                       "session.complete", 
                                       "num_restarts", 
                                       "DEG.first_language", 
                                       "DEG.second_language", 
                                       "DEG.gender", 
                                       "DEG.age"))
var_types <- c("categorial", "numeric")[1 + map_lgl(var_choices, ~{(master[[.x]] %>% class())[1] == "numeric"})]
var_data <- tibble(variable = var_choices, type = var_types)


theme_set(get_default_theme())

get_intro_text <- function(){
  div(h3("Welcome to the PARALA Monitor App"), 
         p("This app allows you visualize and inspect the data from a PARALA pre-screening study on speech and rhythm",
           "that was carried out by the  Max Planck Institute for Empirical Aesthetics, Frankfurt/M., Germany"),
      p("Have fun!"),
      style = "width:50%;text-align:justify")
}

impressum <- function(){
    p(
        "PARALA Monitor  v0.2", 
        shiny::tags$br(), 
        shiny::tags$br(), 
        "Author: Klaus Frieler", 
        shiny::tags$br(), 
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(), 
        "PI", 
        shiny::p("PD Dr. Ines Schindler, Prof. Winfried Menninghaus, Dr. Stefan Blohm, Dr. Valentin Wagner, Anne Siebrasse (M.Sc.)"), 
        shiny::tags$br(),
        shiny::tags$br(), 
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        shiny::tags$br(),
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        shiny::tags$br(), 
        shiny::tags$br(),
        shiny::a(href = "https://github.com/klausfrieler/PARALA_monitor", "On Github", target = "_blank"), 
        style = "font-size: 10pt; display: block"
    )
    
}

input_width <- 300

ui_new <-   
    shiny::shinyUI(
        navbarPage(
            title = "PARALA Speech & Music", 
            theme = shinytheme("spacelab"),
            id = "tabs",
            tabPanel(
                "Home",
                sidebarLayout(
                    sidebarPanel(
                      selectizeInput("study_filter", 
                                     "Filter:", 
                                     c("---", 
                                       "<30", 
                                       "30+"), multiple = F), 
                      impressum(),
                      downloadButton("download_all_data_csv", "Download data"),
                      checkboxInput("dec", label = "Use German Format", value = 0),
                      
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        htmlOutput("introduction"),
                        h4("Summary"),
                        tableOutput("overall_stats"),
                        htmlOutput("model_tab")
                    )
                    
                )
            ),
            tabPanel(
                "Univariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("uv_variable", "Variable:", var_choices, multiple = F), 
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("univariate_plot", width = "800px")
                        )
                    
                )
            ),            
            tabPanel(
                "Bivariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("bv_variable1", "Variable X:", var_choices, selected = "JAJ.ability", multiple = F), 
                        selectizeInput("bv_variable2", "Variable y:", var_choices, selected = "IMI.earworm_frequency", multiple = F), 
                        actionButton("switch_axes", 
                                     label = "Switch axes", style = "margin-bottom: 10px"),
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("bivariate_plot", width = "800px"),
                        tableOutput("corr_tab")
                    )
                    
                )
            ),            
            
            tabPanel(
                "Data",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        DT::DTOutput("raw_data")
                    )
                    
                )
            )))

apply_filters <- function(data, input){
  tabs <- input$tabs
  if(tabs == "Home"){
    filter_val <- input$study_filter
  }
  else{
    return(data)
  }
  #print(input$pc_study_filter)
  if(filter_val == "Music Students"){
    data <- data %>% filter(music_studies == "Music Student")
  }
  else if(filter_val == "No Music Students"){
    data <- data %>% filter(music_studies != "Music Student")
    
  }
  else if(filter_val == "<30"){
    data <- data %>% filter(DEG.age_group == "<30")
    
  }
  else if(filter_val == "30+"){
    data <- data %>% filter(DEG.age_group == "30+")
    
  }
  data
}
# Define server logic required to draw a plot
server <- function(input, output, session) {
   message("*** STARTING APP***")
   check_data <- reactiveFileReader(1000, session, result_dir, setup_workspace)
   
   shiny::observeEvent(input$switch_axes, {
       x <- input$bv_variable1
       y <- input$bv_variable2
       updateSelectizeInput(session, inputId = "bv_variable1",
                            selected = y)
       updateSelectizeInput(session, inputId = "bv_variable2",
                            selected = x)
       
   })
   output$introduction <- renderUI({
     get_intro_text()
   })
   output$overall_stats <- renderTable({
      check_data()
      #data <- apply_filters(master, input)
      data <- master
      p_id_stats <- master %>% 
         distinct(p_id, DEG.gender, DEG.age, GMS.general, session.complete, SEL.status, SEL.group) %>% 
         summarise(n_female = sum(DEG.gender == "female", na.rm = T), 
                   n_male = sum(DEG.gender == "male", na.rm = T), 
                   n_other = sum(DEG.gender == "other", na.rm = T), 
                   n_not_say = sum(DEG.gender == "not_say", na.rm = T), 
                   mean_age = mean(DEG.age, na.rm = T), 
                   mean_GMS = mean(GMS.general, na.rm = T), 
                   n_unique = n(),
                   n_include  = sum(SEL.status == "in"), 
                   n_complete = sum(session.complete, na.rm = T),
                   .groups = "drop")
      p_id_stats %>% 
         select(n_unique, n_complete, n_include, starts_with("n"), mean_age, mean_GMS, everything()) %>% 
          set_names("Total N", "Completed", "Includes", "Females", "Males", "Other", "Rather not say", "Mean Age", "Mean GMS General") 
      })
   
    output$raw_data <- renderDataTable({
      check_data()
      #data <- apply_filters(master, input)
      data <- master
      data %>% 
           select(-p_id) %>% 
           mutate_if(is.numeric, round, 2) %>% 
           select(session.time_started, SEL.status, SEL.group, DEG.first_language, session.complete, DEG.age, DEG.gender, everything())
   }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
   
  output$univariate_plot <- renderPlot({
    check_data()
    #data <- apply_filters(master, input)
    data <- master
    var_info <- var_data %>% filter(variable == input$uv_variable)
    if(var_info$type == "numeric"){
      q <- univariate_plot_numeric(data, input$uv_variable, remove_na = T)
      } 
    else if (var_info$type == "categorial"){
      coord_flip <- n_distinct(data[[input$uv_variable]]) > 3
      q <- univariate_plot_categorial(data, input$uv_variable,  remove_na = T, coord_flip = coord_flip)
      }
    else {
      return()
      }
    q
   })

  output$bivariate_plot <- renderPlot({
    check_data()
    #data <- apply_filters(master, input)
    data <- master
       
    #browser()
    if(input$bv_variable1 == input$bv_variable2){
      return()
    }
    bivariate_plot_auto(data, input, var_data, remove_na = T)   
  })
   
    output$corr_tab <- renderTable({
      check_data()
      data <- apply_filters(master, input)
      vars <- get_parameters(data, input, var_data = var_data)
      if(vars$sub_type == "num-num" && input$bv_variable1 != input$bv_variable2) {
        get_correlations(data, input$bv_variable1, input$bv_variable2)   
      }
    },  caption = "Correlations", caption.placement = getOption("xtable.caption.placement", "top"))
   
    output$download_all_data_csv <- downloadHandler(
      filename = "PARALA_data.csv",
      content = function(file) {
        dec <- ifelse(input$dec, ",", ".") 
        write.table(master %>% mutate_if(is.logical, as.integer), 
                    file, 
                    row.names = FALSE, 
                    dec = dec, 
                    sep = ";", 
                    quote = T, 
                    fileEncoding = "utf-8")
      }
    )
    
}

# Run the application 
shinyApp(ui = ui_new, server = server)

