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
    result_dir <- "../earworms/output/results"
} else{
    result_dir <- "data/from_server"
}

setup_workspace(result_dir)

var_choices <- setdiff(names(master), c("p_id",
                                       "time_started", 
                                       "time_ended", 
                                       "pilot", 
                                       "complete", 
                                       "num_restarts", 
                                       "language", "DEG.gender", "DEG.age", 
                                       "PIT.num_items", "JAJ.num_items"))
var_types <- c("categorial", "numeric")[1 + map_lgl(var_choices, ~{(master[[.x]] %>% class())[1] == "numeric"})]
var_data <- tibble(variable = var_choices, type = var_types)



theme_set(get_default_theme())

get_intro_text <- function(){
  div(h3("Welcome to the Earworm & Working Memory Analysis App"), 
         p("This app allows you visualize and inspect the data from a study on earworms and working memory",
           "that was carried out by the Hochschule für Musik Trossingen and the Max Planck Institute for Empirical Aesthetics, Frankfurt/M., Germany"),
      p("Have fun!"),
      style = "width:50%;text-align:justify")
}

impressum <- function(){
    p(
        "Earworms & Working Memory Monitor  v0.2", 
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
        shiny::p("Selina Janetschek, Prof. Kai Stefan Lothwesen, Hochschule für Musik, Trossingen "), 
        shiny::tags$br(),
        shiny::tags$br(), 
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        shiny::tags$br(), 
        shiny::tags$br(),
        shiny::a(href = "https://github.com/klausfrieler/earworm_monitor", "On Github", target = "_blank"), 
        style = "font-size: 10pt; display: block"
    )
    
}

input_width <- 300

ui_new <-   
    shiny::shinyUI(
        navbarPage(
            title = "Earworms & Working Memory", 
            theme = shinytheme("spacelab"),
            id = "tabs",
            tabPanel(
                "Home",
                sidebarLayout(
                    sidebarPanel(
                      selectizeInput("study_filter", 
                                     "Filter:", 
                                     c("---", 
                                       "Music Students", 
                                       "No Music Students", "<30", 
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
            ),
            tabPanel(
                "Causal Network",
                sidebarLayout(
                    sidebarPanel(
                        # Input: Select information ----
                        selectizeInput("pc_variable", "Variables:", 
                                       num_predictors, 
                                       selected = c("JAJ.ability", "IMI.earworm_frequency", "GMS.general", "PIT.ability"),
                                       multiple = T), 
                        selectizeInput("pc_study_filter", 
                                       "Filter:", 
                                       c("---", 
                                         "Music Students", 
                                         "No Music Students", "<30", 
                                         "30+"), multiple = F), 
                        selectInput(inputId = "alpha", 
                                    label = "Alpha Level",
                                    choices = c(.05, .01, .001), selected = ".05",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "charge", 
                                    label = "Node Charge",
                                    choices = seq(1, 5)*(-30), selected = "-60",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "link_distance", 
                                    label = "Link Distance",
                                    choices = seq(1, 5)*20 + 20, selected = "100",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "font_size", 
                                    label = "Font Size",
                                    choices = seq(1, 10)*2 + 12, selected = "16",
                                    multiple = F, selectize = F),
                        selectInput(inputId = "opacity", 
                                    label = "Opacity",
                                    choices = seq(0, 1, .1), selected = "0.8",
                                    multiple = F, selectize = F),
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        forceNetworkOutput("pc_plot", width = "800px"),
                        htmlOutput("num_model_tab")
                        )
                )
            )))

apply_filters <- function(data, input){
  tabs <- input$tabs
  if(tabs == "Home"){
    filter_val <- input$study_filter
  }
  else if(tabs == "Causal Network"){
    filter_val <- input$pc_study_filter
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
    data <- data %>% filter(age_group == "<30")
    
  }
  else if(filter_val == "30+"){
    data <- data %>% filter(age_group == "30+")
    
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
         distinct(p_id, gender, age, GMS.general, music_studies, complete) %>% 
         summarise(n_female = sum(gender == "female", na.rm = T), 
                   n_male = sum(gender == "male", na.rm = T), 
                   n_other = sum(gender == "other", na.rm = T), 
                   n_not_say = sum(gender == "not_say", na.rm = T), 
                   n_music_students = sum(music_studies %in%  c("Music Student"), na.rm = T),
                   mean_age = mean(age, na.rm = T), 
                   mean_GMS = mean(GMS.general, na.rm = T), 
                   n_unique = n(),
                   n_complete = sum(complete, na.rm = T),
                   .groups = "drop")
      p_id_stats %>% 
         select(n_unique, n_complete, starts_with("n"), mean_age, mean_GMS, everything()) %>% 
          set_names("Total N", "Completed", "Females", "Males", "Other", "Rather not say", "Music Students", "Mean Age", "Mean GMS General") 
      })
   
    output$raw_data <- renderDataTable({
      check_data()
      data <- apply_filters(master, input)
     
      data %>% 
           select(-p_id, -num_restarts, -pilot,  -DEG.age, -DEG.gender) %>% 
           mutate_if(is.numeric, round, 2) %>% 
           select(time_started, time_ended, language, complete, age, gender, everything())
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
      data <- data %>% prepare_EWE_categorials(input$uv_variable, var_data)
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
    data1 <- prepare_EWE_categorials(data, input$bv_variable1, var_data)
    data2 <- prepare_EWE_categorials(data, input$bv_variable2, var_data) 
    data <- data1 %>% left_join(data2, by = "p_id") 
    bivariate_plot_auto(data, input, var_data, remove_na = T)   
  })
   
  output$pc_plot <- renderForceNetwork({
    check_data()
    data <- apply_filters(master, input)
    #data <- master
    if(length(input$pc_variable) < 2){
      return()
    }
    get_pc_graph(data %>% select(input$pc_variable), 
                 alpha = as.numeric(input$alpha),
                 charge = as.numeric(input$charge),
                 linkDistance = as.numeric(input$link_distance),
                 fontSize = as.numeric(input$font_size),
                 opacityNoHover = as.numeric(input$opacity)
                 )
    })
   
    output$corr_tab <- renderTable({
      check_data()
      data <- apply_filters(master, input)
      vars <- get_parameters(data, input, var_data = var_data)
      if(vars$sub_type == "num-num" && input$bv_variable1 != input$bv_variable2) {
        get_correlations(data, input$bv_variable1, input$bv_variable2)   
      }
    },  caption = "Correlations", caption.placement = getOption("xtable.caption.placement", "top"))
   
    output$model_tab <- renderUI({
       check_data()
       data <- apply_filters(master, input)
       lm_tab <- lm(scale(IMI.earworm_frequency) ~ 
                        scale(age) + 
                        scale(JAJ.ability) + 
                        scale(PIT.ability) + 
                        scale(GMS.general) +
                        scale(IMI.negative_valence) +
                        scale(IMI.earworm_duration)
    , 
                    data = data %>% mutate(music_studies = music_studies %>% factor() %>% fct_infreq())) %>% 
           sjPlot::tab_model()
       shiny::div(shiny::h4("Main Model"), shiny::HTML(lm_tab$knitr))
    })
    output$model_tab_stats <- renderTable({
      check_data()
      #data <- apply_filters(master, input)
      data <- master
      lm(IMI.earworm_frequency ~ age + JAJ.ability + PIT.ability + GMS.general + music_studies, data = data) %>% 
           broom::glance()
    },  caption = "Main Model Performance", caption.placement = getOption("xtable.caption.placement", "top"))

        
    output$num_model_tab <- renderUI({
      check_data()
      data <- apply_filters(master, input)
      #data <- master
      lm_tab <- get_model(data, predictors = input$pc_variable, output_format = "sj")
      shiny::div(shiny::h4("Causal Network Model Regression"), shiny::HTML(lm_tab$knitr))
    })
    
    output$download_all_data_csv <- downloadHandler(
      filename = "earworm_data.csv",
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

