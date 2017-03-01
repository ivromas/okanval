# 
# ************************************************************************
# Based on <http://opensource.org/licenses/MIT>
#   
# YEAR: 2017
# COPYRIGHT HOLDER: Ivan Romas
# 
# License: MIT
# 
# ************************************************************************
#   
#   Copyright (c) <YEAR>, <COPYRIGHT HOLDER>
#   
#   Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
#                                                             "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#   в
#   The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
# ************************************************************************
# 
# coding UTF-8
Sys.setlocale('LC_ALL','Russian')

library(shiny)
library(shinydashboard)
library(RPostgreSQL)
library(dplyr)
library(googleVis)
library(Gmisc)
library(reshape)
library(tidyr)
library(shinythemes)
library(shinyjs)
# Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_121\\jre")
library(ReporteRs)
library(shinyBS)


rm(list = ls())

#_________________________________________________________________________________________________________________________________________________
### Set soruce file adress ####
#_________________________________________________________________________________________________________________________________________________
current_folder <- "c:/_IR_F/okanval/"
# current_folder <- "C:/OkanVal/okanval-master/"
func_folder <- paste0(current_folder,file.path("func", "get_methods.R"))
conf_folder <- paste0(current_folder,"server/conf/config.R")
login_folder <- paste0(current_folder,"server/Login.R")
observ_folder <- paste0(current_folder, "server/observ_func.R")
reactive_qa_func_folder <- paste0(current_folder, "server/qa/reactive_func_qa.R")
reactive_qa_ui_folder <- paste0(current_folder, "server/qa/reactive_ui_qa.R")
reactive_eldrive_func_folder <- paste0(current_folder, "server/eldrive/elldrive_reactive_func.R")
reactive_eldrive_ui_folder <- paste0(current_folder, "server/eldrive/eldrive_reactive_ui.R")
reactive_init_ui_folder <- paste0(current_folder, "server/init_data/reactive_ui_init.R")
reactive_init_func_folder <- paste0(current_folder, "server/init_data/reactive_func_init.R")

source(conf_folder)
source(func_folder, encoding = "UTF-8")

#_________________________________________________________________________________________________________________________________________________
### Shiny UI ####
#_________________________________________________________________________________________________________________________________________________
ui <- dashboardPage(
  dashboardHeader(title = "OKANVAL web-UI demo",
    # Notification menu
    dropdownMenu(type = "messages",
                 messageItem("OKAN Team SW dep.", "mailto:romas@kan.su", icon =  icon("info-circle"), time = NULL,
                                               href = NULL)

                 )),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(id = "login_menu",
      menuItem("Login", tabName = "login", icon = icon("glyphicon glyphicon-log-in", lib = "glyphicon"),
               badgeColor = "light-blue")
      ),
    sidebarMenu( id = "main_menu",
          menuItem("Исходные данные", tabName = "init_data", icon = icon("home"),
                   badgeLabel = "1", badgeColor = "light-blue"),
          menuItem("Подбор привода", tabName = "el_drive", icon = icon("cogs"),
                   badgeLabel = "2", badgeColor = "light-blue"),
          menuItem("Материалы деталей", tabName = "det_n_mat", icon = icon("list"),
                   badgeLabel = "2", badgeColor = "light-blue"),
          menuItem("ТБ", tabName = "qa_tables", icon = icon("table"),
                   badgeLabel = "3", badgeColor = "light-blue"),
          useShinyjs()
      )
    ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "login",
              fluidPage(
                sidebarPanel(),
                mainPanel(
                ## Login module;
                box(width = 6,
                    title = h3("Please Log In"),
                    status="danger",
                    solidHeader = TRUE,
                    uiOutput("uiLogin"),
                    textOutput("pass")
                ))
              )),
      # First tab content
      tabItem(tabName = "init_data",
                fluidPage(
                  theme  = "custom.css",
                  box(width = 8, title = h3("Параметры клапана"), background = "light-blue",
                      column(6,
                           selectInput("select_valve", label = "Тип клапана", 
                                       choices = valve_list$valve_name_for_select,
                                       selected = "Клапан регулирующий",
                                       width = "70%")
                      ),
                      column(6,
                             htmlOutput("dynamic_select_valve")
                      ),
                      uiOutput("selected_valve_text_output")
                  ),
                  box(width = 4, background = "light-blue",
                         textInput("rv_drawing_number", "Введите обозначение чертежа деталей ",
                                   value = "RV-YYYYYY", width = "80%"),
                         h5("Обозначение чертежа деталей:"),
                         verbatimTextOutput("rv_draw_numb_disp")
                  ),
                  box(width = 12, background = "light-blue",
                      column(4,
                         selectInput("select_qa_type", label = h5("Класс безопасности"), 
                                     choices = qa_type_list$valve_qa_type_name, 
                                     selected = 1,
                                     width = "80%")),
                  column(4,
                         selectInput("dn_value", label = h5("Значение DN клапана"),
                                     choices = dn_value_list$dn_value,
                                     selected = 1,
                                     width = "80%")),
                  column(4,
                         selectInput("control_type", label = h5("Управляющий орган"),
                                     choices = control_type_list$control_type_def,
                                     selected = 1,
                                     width = "80%")),
                  column(4,
                    selectInput("select_tempr", label = h5("Т-ра рабочей среды выше 100?"), 
                                choices = tempr_list$tempr_value_more_than_100, 
                                selected = 1,
                                width = "80%")),
                  column(4,
                    selectInput("select_tempr_oper", label = h5("Т-ра окружающей среды во время эксплуатации выше 0?"), 
                                choices = tempr_oper_list$tempr_oper_value_more_than_20, 
                                selected = 1,
                                width = "100%")),
                  column(4,
                         uiOutput("dynamic_select_pressure"))
                  )
                )
              
      ),
      # Second tab content
      tabItem(tabName = "det_n_mat",
              fluidPage(
                box(width = 12,title = h3("Список деталей клапана"), background = "light-blue",
                    h4("Выберите материалы для каждой детали"),
                    htmlOutput("details_and_materials")
                ),
                box(width = 12, background = "light-blue",
                    h4("Выберите материал наплавки для деталей(при наличии)"),
                    htmlOutput("details_and_overlays")
                )
              )
      ),
      # Third tab content
      tabItem(tabName = "qa_tables",
              navbarPage("Составление таблиц ТБ",theme  = "custom.css",
                tabPanel("ТБ 1",
                         box(width = 12,
                             tags$head(tags$style(HTML("
                                           .shiny-output-error-validation {
                                           color: red;
                                           font-weight: bold;
                                           }
                                           "
                                                       ))
                                      ),
                             uiOutput("qa1_header"),
                             htmlOutput("qa_table"),
                             htmlOutput("text"),
                             downloadButton('downloadData', 'Скачать в *.csv'),
                             downloadButton('downloadDataDocx','Скачать в *.docx')
                         )
                ),
                tabPanel("ТБ 2",
                         box(width = 12,
                             useShinyjs(),
                             div(
                               id = "main",
                               tags$head(tags$style(HTML("
                                           .shiny-output-error-validation {
                                                         color: red;
                                                         font-weight: bold;
                                                         }
                                                         "
                               ))
                               ),
                               uiOutput("qa2_header"),
                               htmlOutput("qa_table2"),
                               htmlOutput("text_qa2"),
                               downloadButton('download_qa2', 'Скачать в *.csv'),
                               downloadButton('downloadDataDocx_qa2','Скачать в *.docx')
                              )

                           )
                         )
                )
              ),
      tabItem(tabName = "el_drive",
              fluidPage(
                box(width = 12,
                    title = h3("Подбор электропривода"), 
                    background = "light-blue",
                    column(width = 12, 
                           htmlOutput("el_drive_select")
                    ),
                    column(width = 12,
                           uiOutput("eldrive_print_name")
                    )
                )
              )
              )
      )
    )
  )

#_________________________________________________________________________________________________________________________________________________
### Shiny Server ####
#_________________________________________________________________________________________________________________________________________________
server <- function(input, output, session) {
  con <- okan_db_connect()
  #_________________________________________________________________________________________________________________________________________________
  ### Source files ####
  #_________________________________________________________________________________________________________________________________________________
  source(login_folder,  local = TRUE)
  # source(observ_folder, local = TRUE)
  eval(parse(observ_folder, encoding="UTF-8"))
  #_________________________________________________________________________________________________________________________________________________
  ### General selects ####
  #_________________________________________________________________________________________________________________________________________________
  eval(parse(reactive_init_ui_folder, encoding="UTF-8"))
  eval(parse(reactive_init_func_folder, encoding="UTF-8"))
  #_________________________________________________________________________________________________________________________________________________
  ### QA tables ####
  #_________________________________________________________________________________________________________________________________________________
  eval(parse(reactive_qa_func_folder, encoding="UTF-8"))
  eval(parse(reactive_qa_ui_folder, encoding="UTF-8"))
  #_________________________________________________________________________________________________________________________________________________
  ### Electric drive ####
  #_________________________________________________________________________________________________________________________________________________
  eval(parse(reactive_eldrive_func_folder, encoding="UTF-8"))
  eval(parse(reactive_eldrive_ui_folder, encoding="UTF-8"))

values <- reactiveValues(stem_force_min = 3330, stem_force_max = 180830,
                           stem_force_input_min = 3.33, stem_force_input_max = 180.83,
                           safety_factor_no = "нет", safety_factor_recomended = "20", safety_factor_low = "10",
                         torque_max = 1200, torque_min = 15, close_time_min = 5, close_time_max = 240)
  
  get_safety_factor <- function() {
    
    if (input$select_valve == "Клапан регулирующий" || input$select_valve == "Клапан запорный") {
      
      if (input$bellow == TRUE && length(input$bellow != 0)) {
        
        values[["safety_factor_recomended"]] <- 50
        values[["safety_factor_low"]] <- 20
        
      } else {
        
        values[["safety_factor_recomended"]] <- 20
        values[["safety_factor_low"]] <- 10
      }
      
    } else if (input$select_valve == "Задвижка") {
      
      values[["safety_factor_recomended"]] <- 15
      values[["safety_factor_low"]] <- 5
    }
  }
  
  
  get_stem_force_boundary <- function() {
    
    if (input$main_menu == "el_drive" && length(input$main_menu != 0)) {
      
      if (input$safety_factor_select == "нет") {
        
        safety_factor <- 1
        
      } else {
        
        safety_factor <- input$safety_factor_select %>% as.integer()

        safety_factor <- safety_factor / 100 + 1
        
      }
      
      L_min <- 1 / 2 / pi + 0.144 / 2 * 12
      
      stem_force_max_local <- 217000
      stem_force_min_local <- 4000
      # L_max <- 3 * 15.5 / 2 / pi + 0.144 / 2 * 800
      if (length(input$LE_module) != 0) {
        LE_module <- input$LE_module
      } else {
        LE_module <- FALSE
      }
      
      shinyjs::delay(500, {
        
        if (LE_module == TRUE) {
          stem_force_max_local <- 217000
          stem_force_min_local <- 4000
          # print("LE")
          
        } else if (input$reducer_checkbox == TRUE && length(input$reducer_checkbox) != 0 && input$el_drive_type == "SA") {
          
          stem_force_max_local <- 16000 * 1000 / L_min
          stem_force_min_local <- 120 * 1000 / L_min
          # print("gst")
          
        } else if (input$reducer_checkbox == TRUE && length(input$reducer_checkbox) != 0 && input$el_drive_type == "SAI") {
          
          stem_force_max_local <- 16000 * 1000 / L_min
          stem_force_min_local <- 2000 * 1000 / L_min
          # print("gsti")
          
        } else if (input$el_drive_type == "SA") {
          
          stem_force_max_local <- 8000 * 1000 / L_min
          stem_force_min_local <- 40 * 1000 / L_min
          # print("sa")
        } else if (input$el_drive_type == "SAI") {
          
          stem_force_max_local <- 6000 * 1000 / L_min
          stem_force_min_local <- 10 * 1000 / L_min
          # print("sai")
        } else if (input$el_drive_type == "SAR") {
          # print("sar")
          stem_force_max_local <- 4000 * 1000 / L_min
          stem_force_min_local <- 15 * 1000 / L_min
          
        } else if (input$el_drive_type == "SARI") {
          
          stem_force_max_local <- 3200 * 1000 / L_min
          stem_force_min_local <- 15 * 1000 / L_min
          # print("sari")
        }
        
        values$stem_force_max <- round(stem_force_max_local / safety_factor)
        values$stem_force_min <- round(stem_force_min_local / safety_factor)
        values$stem_force_input_max <- round(stem_force_max_local / 1000 / safety_factor, digits = 2)
        values$stem_force_input_min <- round(stem_force_min_local / 1000 / safety_factor, digits = 2)
        # print(values$stem_force_max)
        # print(values$stem_force_min)
      })
    }
  }
  
  
  get_torque_boundary <- function() {
    
    if (input$main_menu == "el_drive" && length(input$main_menu != 0)) {
      
      if (input$safety_factor_select == "нет") {
        
        safety_factor <- 1
        
      } else {
        
        safety_factor <- input$safety_factor_select %>% as.integer()
        
        safety_factor <- safety_factor / 100 + 1
        
      }
      
      torque_max_local <- 100
      torque_min_local <- 1200
      close_time_min_local <- 5
      close_time_max_local <- 240
      
      shinyjs::delay(500, {
        
        if (input$reducer_checkbox == TRUE && length(input$reducer_checkbox) != 0 && input$el_drive_type == "SA") {
          
          torque_max_local <- 16000
          torque_min_local <- 120
          # print("gst")
          
        } else if (input$reducer_checkbox == TRUE && length(input$reducer_checkbox) != 0 && input$el_drive_type == "SAI") {
          
          torque_max_local <- 16000
          torque_min_local <- 2000
          # print("gsti")
          
        } else if (input$el_drive_type == "SA") {
          
          torque_max_local <- 8000
          torque_min_local <- 10
          # print("sa")
        } else if (input$el_drive_type == "SAI") {
          
          torque_max_local <- 6000
          torque_min_local <- 10
          # print("sai")
        } else if (input$el_drive_type == "SAR") {
          # print("sar")
          torque_max_local <- 4000
          torque_min_local <- 15
          
        } else if (input$el_drive_type == "SARI") {
          
          torque_max_local <- 3200
          torque_min_local <- 15
          # print("sari")
        } else if (input$el_drive_type == "SG") {
          
          torque_max_local <- 1200
          torque_min_local <- 100
          close_time_min_local <- 4
          close_time_max_local <- 63
          # print("sari")
        }
        # print(torque_max_local)
        # print(torque_min_local)
        # print(safety_factor)
        
        values$close_time_min <- close_time_min_local
        values$close_time_max <- close_time_max_local
        values$torque_max <- round(torque_max_local / safety_factor)
        values$torque_min <- round(torque_min_local / safety_factor)
# 
#         print(values$torque_max)
#         print(values$torque_min)
      })
    }
  }
  

  observeEvent(input$select_el_drive_btn, {
    
    str <- ""
    
    if (input$force_input_type == "усилию на штоке") {
      
      get_stem_force_boundary()
  
  
      if (input$safety_factor_select == "нет") {
        
        safety_factor <- 1      
  
      } else {
        
        safety_factor <- input$safety_factor_select %>% as.integer()
        
        safety_factor <- safety_factor / 100 + 1
        
      }
      
   
      if (input$el_drive_type == "SA" && input$select_valve == "Задвижка") {
        torque_lower_lim <- 60
      } else if (input$el_drive_type == "SAI" && input$select_valve == "Задвижка") {
        torque_lower_lim <- 500
      }
  
      stem_stroke <- input$stem_stroke
      # from кН to H and adding safety factor to force
      stem_force <- as.integer(input$stem_force * 1000) * safety_factor
      # Шаг резьбы [мм]
      thread_pitch <- input$thread_pitch
      # Диаметр штока [мм]
      stem_diameter <- input$stem_diameter
      # Многозаходность
      multithread <- input$multithread
      # Пределы регулирования муфты ограничения кутящего момента
      torque <- stem_force * stem_diameter / 2 * (multithread * thread_pitch / 
                                                    (pi *  stem_diameter) + 0.144) / 1000
      
      torque <- round_any(torque,10, ceiling) %>% as.integer()
      
      if (input$select_valve == "Задвижка") {
        
        closeAlert(session, "reducer_corretion_alert")
        
        if (torque < torque_lower_lim && input$reducer_checkbox == TRUE) {
          
          # updateCheckboxInput(session, "reducer_checkbox", value = FALSE)
          createAlert(session,"reducer_corretion", alertId = "reducer_corretion_alert",
                      content = HTML("<b><p>При данном усилии на штоке редуктор не требуется</b></p>") ,
                      style = "warning", dismiss = FALSE, append = FALSE)
          
        } else if (torque > 6000 && input$reducer_checkbox == FALSE) {
          
          # updateCheckboxInput(session, "reducer_checkbox", value = TRUE)
          createAlert(session,"reducer_corretion", alertId = "reducer_corretion_alert",
                      content = HTML("<b><p>При данном усилии на штоке необходим редуктор</b></p>") ,
                      style = "warning", dismiss = FALSE, append = FALSE)
          
        }
        
      }
      
      
      if (is.na(input$close_time) || !is.numeric(input$close_time) || input$close_time < 5 ||  input$close_time > 240) {
        
        str <- paste0(str,"<p>   -времени закрытия</p>")
        
      } 
      
      if (is.na(input$stem_stroke) || !is.numeric(input$stem_stroke) ||
          input$stem_stroke < 10 || input$stem_stroke > 800) {
        
        str <- paste0(str,"<p>   -хода штока</p>")
        
      } 
      
      if (is.na(input$stem_force) || !is.numeric(input$stem_force) || stem_force >  values$stem_force_max ||
          stem_force < values$stem_force_min) {
        
        str <- paste0(str,"<p>   -максимального усилия на штоке</p>")
        
      } 
      
      if (input$LE_module == FALSE) {
        
        if (is.na(input$thread_pitch) || !is.numeric(input$thread_pitch) || input$thread_pitch < 1 || 
            input$thread_pitch > 15.5)  {
          
          str <- paste0(str,"<p>   -шага резьбы</p>")
          
        }
        
        if (is.na(input$stem_diameter) || !is.numeric(input$stem_diameter) || input$stem_diameter < 12 || 
            input$stem_diameter > 800) {
          
          str <- paste0(str,"<p>   -диаметра штока</p>")
          
        } 
        
        if (is.na(input$multithread) || !is.numeric(input$multithread) || input$multithread < 1 || 
            input$multithread > 3) {
          
          str <- paste0(str,"<p>   -многозаходиности</p>")
          
        } 
      }
      
      
      
      if (str != "") {
        
          str <- paste0('<p><i class="fa fa-times-circle-o fa-spin fa-2x" aria-hidden="true"></i><b>   Ошибка при задании</b></p>',
                        str)
          
          closeAlert(session, "incorrect_param_alert")
          
          createAlert(session, "incorrect_param", alertId = "incorrect_param_alert", content = HTML(str),
                      style = "error", dismiss = FALSE, append = FALSE)
  
      } else {
        
        closeAlert(session, "incorrect_param_alert")
        
        electric_drive_print_text <- reactive_get_el_drive_full_name()
        
        output$eldrive_print_name <-
          renderUI({
            if ((input$select_valve == "Клапан регулирующий" || input$select_valve == "Клапан запорный" ||
                 input$select_valve == "Задвижка") && input$control_type == "Электропривод") {
              headerPanel(tags$div(
                HTML(paste0("<strong>",'<font face="Bedrock" size="4", color="black">',
                            electric_drive_print_text,"</font>","</strong>"))
              ))
            } 
          })
        
      }
    } else if (input$force_input_type == "моменту") {
      
      get_torque_boundary()
      

      
      if (input$safety_factor_select == "нет") {
        
        safety_factor <- 1      
        
      } else {
        
        safety_factor <- input$safety_factor_select %>% as.integer()
        
        safety_factor <- safety_factor / 100 + 1
        
      }
      
      
      if (input$el_drive_type == "SA" && input$select_valve == "Задвижка") {
        torque_lower_lim <- 60
      } else if (input$el_drive_type == "SAI" && input$select_valve == "Задвижка") {
        torque_lower_lim <- 500
      }

      torque <- input$torque_input
      
      torque <- round_any(torque,10, ceiling) %>% as.integer()
      
      if (input$select_valve == "Задвижка") {
        
        closeAlert(session, "reducer_corretion_alert")
        
        if (torque < torque_lower_lim && input$reducer_checkbox == TRUE) {
          
          # updateCheckboxInput(session, "reducer_checkbox", value = FALSE)
          createAlert(session,"reducer_corretion", alertId = "reducer_corretion_alert",
                      content = HTML("<b><p>При данном усилии на штоке редуктор не требуется</b></p>") ,
                      style = "warning", dismiss = FALSE, append = FALSE)
          
        } else if (torque > 6000 && input$reducer_checkbox == FALSE) {
          
          # updateCheckboxInput(session, "reducer_checkbox", value = TRUE)
          createAlert(session,"reducer_corretion", alertId = "reducer_corretion_alert",
                      content = HTML("<b><p>При данном усилии на штоке необходим редуктор</b></p>") ,
                      style = "warning", dismiss = FALSE, append = FALSE)
          
        }
        
      }
      
      
      if (is.na(input$close_time) || !is.numeric(input$close_time) || input$close_time < 5 ||  input$close_time > 240) {
        
        str <- paste0(str,"<p>   -времени закрытия</p>")
        
      } 

      
      if (is.na(input$torque_input) || !is.numeric(input$torque_input) || torque >  values$torque_max ||
          torque < values$torque_min) {
        
        str <- paste0(str,"<p>   -момента</p>")
        
      } 
      
      if (str != "") {
        
        str <- paste0('<p><i class="fa fa-times-circle-o fa-spin fa-2x" aria-hidden="true"></i><b>   Ошибка при задании</b></p>',
                      str)
        
        closeAlert(session, "incorrect_param_alert")
        
        createAlert(session, "incorrect_param", alertId = "incorrect_param_alert", content = HTML(str),
                    style = "error", dismiss = FALSE, append = FALSE)
        
      } else {
        
        closeAlert(session, "incorrect_param_alert")
        
        electric_drive_print_text <- reactive_get_el_drive_full_name()
        
        output$eldrive_print_name <-
          renderUI({
            if ((input$select_valve == "Клапан регулирующий" || input$select_valve == "Клапан запорный" ||
                 input$select_valve == "Задвижка" || (input$select_valve == "Кран" || 
                 input$select_valve == "Затвор" && input$select_valve_full == "Затвор дисковый"))
                && input$control_type == "Электропривод") {
              headerPanel(tags$div(
                HTML(paste0("<strong>",'<font face="Bedrock" size="4", color="black">',
                            electric_drive_print_text,"</font>","</strong>"))
              ))
            } 
          })
        
      }
    }
    
  })
  
  
  reactive_get_el_drive <- reactive({
    
    if (input$safety_factor_select == "нет") {
      
      safety_factor <- 1
      
    } else {
      
      safety_factor <- input$safety_factor_select %>% as.integer()
      safety_factor <- safety_factor / 100 + 1

    }
    
    if (input$force_input_type == "усилию на штоке") {
      # from кН to H and adding safety factor to force
      stem_force <- as.integer(input$stem_force * 1000) * safety_factor
      
    } else {
      
      torque <- input$torque_input * safety_factor
      
    }
    
    if ((input$select_valve == "Кран" || 
        (input$select_valve == "Затвор" && input$select_valve_full == "Затвор дисковый"))) {
      
      time <- input$close_time
      x <- get_eldrive(con, type = input$el_drive_type, torque = torque, time = time)
      
      return(x)
    }
      
      
    # time to mitue value
    close_time <- input$close_time / 60
      # Шаг резьбы [мм]
      thread_pitch <- input$thread_pitch
      # Диаметр штока [мм]
      stem_diameter <- input$stem_diameter
      # Многозаходность
      multithread <- input$multithread
      
      stem_stroke <- input$stem_stroke
      
      
      if ((input$LE_module == TRUE && input$el_drive_type == "SAR")) {
        # speed in [mm per minute]
        speed <- stem_stroke / close_time
        
        x <- get_eldrive(con, type = "LE + SAR", speed = speed, stem_stroke = stem_stroke, stem_force = stem_force)
  
        return(x)
        
      } else {
        
        # частота вращения приводного вала
        nesessary_number_of_rotations <- stem_stroke / (close_time * thread_pitch * multithread)
        
        nesessary_number_of_rotations <- round_any(nesessary_number_of_rotations, 1, ceiling) %>% as.integer()
        # Пределы регулирования муфты ограничения кутящего момента
        if (input$force_input_type == "усилию на штоке") {
          
          torque <- stem_force * stem_diameter / 2 * (multithread * thread_pitch / 
                                                        (pi *  stem_diameter) + 0.144) / 1000
          
          torque <- round_any(torque,10, ceiling) %>% as.integer()
          
        } else {
          
          torque <- input$torque_input
          
        }
        
        closeAlert(session, alertId = "torque_info_alert")
        createAlert(session,"torque_info", alertId = "torque_info_alert",
                    content = HTML(paste0("<b><p>Расчётный момент составляет ",torque," Нм</b></p>")) ,
                    style = "info", dismiss = FALSE, append = FALSE)
        
        if (input$reducer_checkbox == TRUE) {
        
          if (input$el_drive_type == "SA") {
            
            reducer_type = "GST"
            
          } else {
            
            reducer_type = "GSTI"
            
          }
          
          reducer_list <- get_eldrive(con, type = reducer_type, torque = torque)
          
          if (is.data.frame(reducer_list) && nrow(reducer_list) == 0) {
            
            createAlert(session,"reducer_corretion", alertId = "reducer_corretion_alert",
                        content = HTML("<b><p>Ошибка 1</b></p>") ,
                        style = "warning", dismiss = FALSE, append = FALSE)
            x <- data.frame()
            return(x)
  
            
          } else {
          
            reducer_list$torque_to_eldrive <- torque  / reducer_list$reducer_coef_trans
            
            reducer_list$torque_to_eldrive <- round_any(reducer_list$torque_to_eldrive, 1, ceiling) %>% as.integer()
            
            reducer_list$nesessary_number_of_rotations <- nesessary_number_of_rotations * reducer_list$gear_attitude
            
            reducer_list$nesessary_number_of_rotations <- round_any(reducer_list$nesessary_number_of_rotations,
                                                                    1, ceiling) %>% as.integer()
            
          }
          
          for (i in 1:length(reducer_list$reducer_id)) {
            
            x <- get_eldrive(con, type = "SA(I) + GST(I)", torque = reducer_list$torque_to_eldrive[i], 
                             nesessary_number_of_rotations = reducer_list$nesessary_number_of_rotations[i],
                             reducer_id = reducer_list$reducer_id[i])
            if (is.data.frame(x) && nrow(x) != 0) {
              x$reducer_type <- reducer_list$reducer_type[i]
              x$reducer_gear_ratio <- reducer_list$gear_attitude[i]
              x$reducer_con_type <- reducer_list$reducer_con_type[i]
              x$reducer_price <- reducer_list$price[i]
  
            }
            
            if (i == 1 || nrow(full_x_list) == 0) {
              full_x_list <- x
            } else {
              full_x_list <- rbind(full_x_list, x)
            }
            
          }
          
          return(full_x_list)
        
        } else {
        
        
          x <- get_eldrive(con, type = input$el_drive_type, stem_stroke = stem_stroke, torque = torque, 
                         nesessary_number_of_rotations = nesessary_number_of_rotations)
        
        
          return(x)
        }
      }
    
  })
  
  
  reactive_get_el_drive_full_name <- reactive({
    
    x <- reactive_get_el_drive()
    
    if ( nrow(x) == 0) {
      str <- paste0("Введены невалидные исходные параметры")
      
    } else if ( !is.data.frame(x) ) {
      str <- paste0("Введены невалидные исходные параметры")
      
    } else {
      
      if (input$gold_plated_contacts == "Стандартные") {
        number <- ""
        price_gc <- ""
      } else {
        number <- "-G"
        price_gc <- " + 200"
      }


      if (input$limit_switches_type == "Одиночные" && input$intermediate_position_switches_type == "Сдвоенные") {
        str_part2 <- paste0("6",number,"-9.3",number,"-DUO")
        str_part_add_3 <- "030"
      } else if (input$limit_switches_type == "Сдвоенные" && input$intermediate_position_switches_type == "Одиночные") {
        str_part2 <- paste0("6",number,"-9.2",number,"-DUO")
        str_part_add_3 <- "200"
      } else if (input$limit_switches_type == "Одиночные" && input$intermediate_position_switches_type == "Одиночные") {
        str_part2 <- paste0("6",number,"-9",number,"-DUO")
        str_part_add_3 <- "010"
      } else {
        str_part2 <- paste0("6",number,"-9.4",number,"-DUO")
        str_part_add_3 <- "210"
      }

      
      if (input$el_drive_type == "SAR" || input$el_drive_type == "SA" || input$el_drive_type == "SG") {

        if (input$position_sensor == "Токовый(RWG)") {
          position_sensor <- "21.4/4"
          } else {
          position_sensor <- "12.E"
          }
        
        str_part3 <- paste0(position_sensor, "-S105", number,"-11-IP67-KS-TP104/",str_part_add_3)
      } else if (input$el_drive_type == "SARI" || input$el_drive_type == "SAI") {

        str_part3 <- paste0("12.E-SH-148", number,"-IP68-KSG-TPA00R0AE-0A0-000")
      }
      
      # LE module dependencies
      if (input$LE_module == TRUE && length(input$LE_module) != 0 && input$el_drive_type == "SAR") {
        str_part1 <- paste0(x$flange_fittings, "LE")
        str_part_last <- paste0("+", x$modul_type)
      } else if (length(input$reducer_checkbox) != 0 && input$reducer_checkbox == TRUE ) {
        str_part1 <- paste0(x$flange_fittings, "(B3)")
        str_part_last <- paste0("+", x$reducer_type, "(", x$reducer_gear_ratio, ":1)")
      } else {
        str_part1 <- paste0(x$flange_fittings, "(A)")
        str_part_last <- ""
      }

      # PRICE 
      if ("reducer_price" %in% names(x) == TRUE) {
        if (!is.na(x$reducer_price) &&  is.na(x$price)) {
          str_price <- paste0(" [цену данного привода следует уточнить у производителя, цена редуктора ", x$reducer_price, " евро за ед.]")
        } else if (!is.na(x$reducer_price) &&  !is.na(x$price)) {

          str_price <- paste0(" [цена данного исполнения ",x$price, price_gc," + ", x$reducer_price, " евро за ед.]")
        } else if (is.na(x$reducer_price) &&  !is.na(x$price)) {
          str_price <- paste0(" [цена данного привода ",x$price, price_gc," евро за ед., цену редуктора следует уточнить у производителя]")

        } else {
          str_price <- paste0(" [цену данного исполнения следует уточнить у производителя]")
        }
        
      } else if (is.na(x$price)) {
        
        str_price <- paste0(" [цену данного исполнения следует уточнить у производителя]")
        
      } else {
        

        str_price <- paste0(" [цена данного исполнения ",x$price, price_gc," евро за ед.]")
        
      }
      
      str <- paste0("<b>Указанным исходным данным соответствует привод ", x$eldrive_name,"-", 
                    str_part1,"-", "380/50/3", "-", x$rotation_speed, "-", "10.1-XX-",str_part2,"-", str_part3,
                    " ", x$rated_power, " кВт ", str_part_last,"</br>","<b>", str_price,"</br>")

    }
    return(str)
  })
  
 
  output$el_drive_select <-
    renderUI({
      if (input$select_valve == "Клапан регулирующий" && input$control_type == "Электропривод") {
        fluidPage(
          box(width = 12,
              background = "light-blue",
              column(width = 4,
                     radioButtons(inputId =  "el_drive_type","Тип привода", c("SAR", "SARI")),
                     radioButtons(inputId =  "force_input_type","Подбор привода по:", c("моменту", "усилию на штоке"))
              ),
              column(width = 8,
                     htmlOutput("safety_factor"),
                     htmlOutput("LE"),
                     htmlOutput("reducer_checkbox_gr")
              )
          ),
          column(width = 4,
                 htmlOutput("eldrive_param")
          ),
          column(width = 4,
                 htmlOutput("thread")
          ),
          column(width = 4, 
                 htmlOutput("contact_param")
          ),
          column(width = 12,
                 actionButton(inputId = "select_el_drive_btn", label = "Подбор электропривода по заданным параметрам",
                              icon = icon("check-square"))
          ),
          column(width = 4,
                 bsAlert("incorrect_param")
                 )
        )
      } else if (input$select_valve == "Клапан запорный" && input$control_type == "Электропривод") {
        
        fluidPage(
          box(width = 12,
              background = "light-blue",
              column(width = 4,
                     radioButtons(inputId =  "el_drive_type","Тип привода", c("SA", "SAI")),
                     radioButtons(inputId =  "force_input_type","Подбор привода по:", c("моменту", "усилию на штоке"))
              ),

              column(width = 8,
                     htmlOutput("safety_factor"),
                     htmlOutput("LE"),
                     hidden(htmlOutput("reducer_checkbox_gr"))
              )
          ),
          column(width = 4,
                 htmlOutput("eldrive_param")
          ),
          column(width = 4,
                 htmlOutput("thread")
          ),
          column(width = 4, 
                 htmlOutput("contact_param")
          ),
          column(width = 12,
                 actionButton(inputId = "select_el_drive_btn", label = "Подбор электропривода по заданным параметрам",
                              icon = icon("check-square"))
          ),
          column(width = 4,
                 bsAlert("incorrect_param")
          )
        )
      } else if (input$select_valve == "Задвижка" && input$control_type == "Электропривод") {
        
        fluidPage(
          box(width = 12,
              background = "light-blue",
              column(width = 4,
                     radioButtons(inputId =  "el_drive_type","Тип привода", c("SA", "SAI")),
                     radioButtons(inputId =  "force_input_type","Подбор привода по:", c("моменту", "усилию на штоке"))
              ),
              column(width = 8,
                     htmlOutput("safety_factor"),
                     htmlOutput("reducer_checkbox_gr"),
                     htmlOutput("LE")
              )
          ),
          column(width = 4,
                 htmlOutput("eldrive_param")
          ),
          column(width = 4,
                 htmlOutput("thread")
          ),
          column(width = 4, 
                 htmlOutput("contact_param")
          ),
          column(width = 12,
                 actionButton(inputId = "select_el_drive_btn", label = "Подбор электропривода по заданным параметрам",
                              icon = icon("check-square"))
          ),
          column(width = 4,
                 bsAlert("incorrect_param"),
                 bsAlert("reducer_corretion")
                 
          )
        )
          
      } else if ((input$select_valve == "Кран" || 
                  (input$select_valve == "Затвор" && input$select_valve_full == "Затвор дисковый"))
                 && input$control_type == "Электропривод") {
        
        fluidPage(
          box(width = 12,
              background = "light-blue",
              column(width = 4,
                     radioButtons(inputId =  "el_drive_type","Тип привода", c("SG")),
                     radioButtons(inputId =  "force_input_type","Подбор привода по:", c("моменту"))
              ),
              column(width = 8,
                     htmlOutput("safety_factor"),
                     htmlOutput("reducer_checkbox_gr"),
                     htmlOutput("LE")
              )
          ),
          column(width = 4,
                 fluidPage(
                  numericInput("close_time","Время закрытия [сек]" ,value = 60,
                                min = values$close_time_min, max = values$close_time_max, step = 5, width = "100%"),
                  bsTooltip(id = "close_time", title = paste0("от ", values$close_time_min, 
                                                               " до ", values$close_time_max, "с"),
                            placement = "left", trigger = "focus"),
                  htmlOutput("isolated_stem_force"),
                  bsAlert("torque_info")
                  )
          ),
          column(width = 4, 
                 htmlOutput("contact_param")
          ),
          column(width = 12,
                 actionButton(inputId = "select_el_drive_btn", label = "Подбор электропривода по заданным параметрам",
                              icon = icon("check-square"))
          ),
          column(width = 4,
                 bsAlert("incorrect_param"),
                 bsAlert("reducer_corretion")
                 
          )
        )
        
      } else {
        headerPanel(tags$div(
          HTML(paste0("<strong>",'<font face="Bedrock" size="4">',"Электропривод не требуется","</font>","</strong>"))
        ))
      }
    })
  
  
  output$eldrive_param <- 
    renderUI({
        fluidPage(
            numericInput("close_time","Время закрытия [сек]" ,value = 60,
                         min = values$close_time_min, max = values$close_time_max, step = 5, width = "100%"),
            numericInput("stem_stroke", "Ход штока [мм]",value = 100
                         , min = 10, max = 800, step = 100, width = "100%"),
          bsTooltip(id = "close_time", title = paste0("от ", values$close_time_min, 
                                                      "до ", values$close_time_max, "с"), 
                    placement = "left", trigger = "focus"),
          bsTooltip(id = "stem_stroke", title = "от 10 до 800 мм", 
                    placement = "left", trigger = "focus"),
          htmlOutput("isolated_stem_force"),
          bsAlert("torque_info")
        
      )
    })
  
      
  output$isolated_stem_force <- renderUI({
    if (input$force_input_type == "усилию на штоке") {
      verticalLayout(
        numericInput("stem_force", "Максимальное усилие на штоке [кН]", value = 33,
                     min = values$stem_force_input_min, max = values$stem_force_input_max,
                     step = 0.1, width = "100%"),
        bsAlert("torque_info"),
        bsTooltip(id = "stem_force",  title = paste0("от ", values$stem_force_input_min," до ", 
                                                     values$stem_force_input_max, " кН"), 
                  placement = "left", trigger = "focus"),
        bsPopover(id = "stem_force",
                  title = HTML(paste0('<font color="black">При изменении типа электропривода и пр.',
                               'границы макс/мин значения пересчитываются после подбора электропривода</font>')),
                  content = "", trigger = "focus", options = NULL)
      )
    } else {
      verticalLayout(
      numericInput("torque_input", "Крутящий момент [Нм]",value = 50
                   , min = values$torque_min, max = values$torque_max, step = 10, width = "100%"),
      bsAlert("torque_info"),
      bsTooltip(id = "torque_input",  title = paste0("от ", values$torque_min," до ", 
                                                   values$torque_max, " Нм"), 
                placement = "left", trigger = "focus"),
      bsPopover(id = "torque_input",
                title = HTML(paste0('<font color="black">При изменении типа электропривода и пр.',
                                    'границы макс/мин значения пересчитываются после подбора электропривода</font>')),
                content = "", trigger = "focus", options = NULL)
      )
    }
  })
  
  
  output$LE <- 
    renderUI({
      if (input$el_drive_type == "SAR" && input$force_input_type == "усилию на штоке") {
        verticalLayout(
          checkboxInput(inputId = "LE_module", h5("LE-модуль"), value = FALSE),
          bsPopover(id = "LE_module", title = HTML('<font color="black">Обновите значение максимального усилия на штоке</font>'),
                    content = "", placement = "left", trigger = "focus", options = NULL)
        )
      } else {
        hidden(checkboxInput(inputId = "LE_module", h5("LE-модуль"), value = FALSE))
      }
    })
  
  output$reducer_checkbox_gr <- 
    renderUI({
      if (input$select_valve == "Задвижка") {
        verticalLayout(
          checkboxInput(inputId = "reducer_checkbox", h5("Использовать редуктор"), value = FALSE),
          bsPopover(id = "reducer_checkbox", title = HTML('<font color="black">Обновите значение максимального усилия на штоке</font>'),
                content = "", placement = "left", trigger = "focus", options = NULL)
        )
      } else {
        hidden( checkboxInput(inputId = "reducer_checkbox", h5("Использовать редуктор"), value = FALSE))
      }      

    })
  
  output$safety_factor <-
    renderUI({
      fluidPage(
        selectInput("safety_factor_select", label = "Запас по моменту [ % ]", 
                    choices = c(values$safety_factor_no, values$safety_factor_recomended, values$safety_factor_low),
                    selected = values$safety_factor_recomended, width = "40%"),
        bsTooltip(id = "safety_factor_select",title = 
                    paste0("Рекомендуемый запас по моменту ",values$safety_factor_recomended,"%"),
                    placement = "left", trigger = "focus")
      )

    })

  output$thread <-
    renderUI({
      if (input$LE_module == FALSE || input$el_drive_type == "SARI" || length(input$LE_module) == 0) {
          fluidPage(
                 numericInput("thread_pitch", "Шаг резьбы [мм]",value = 8, min = 1.0, max = 15.5,
                              width = "100%", step = 0.1),
                 numericInput("stem_diameter", "Диаметр штока [мм]",value = 20, min = 12, max = 800,
                              width = "100%", step = 0.1),
                 numericInput("multithread", "Многозаходность",value = 2, min = 1, max = 3, width = "100%"),
          bsTooltip(id = "thread_pitch", title = "от 1,0 до 15,5 мм", 
                    placement = "left", trigger = "focus"),
          bsTooltip(id = "stem_diameter", title = "от 12 до 800 мм", 
                    placement = "left", trigger = "focus"),
          bsTooltip(id = "multithread", title = "от 1 до 3", 
                    placement = "left", trigger = "focus")
        )
      }
    })
  
  output$contact_param <-
    renderUI({
      fluidPage(
        selectInput("gold_plated_contacts", label = h5("Тип контактов"), 
                    choices = c("Стандартные", "Позолоченые"), 
                    selected = 1,
                    width = "100%"),
        selectInput("limit_switches_type", label = h5("Тип концевых выключателей"), 
                    choices = c("Одиночные", "Сдвоенные"), 
                    selected = 1,
                    width = "100%"),
        selectInput("intermediate_position_switches_type", label = h5("Тип промежуточных выключателей"), 
                    choices = c("Одиночные", "Сдвоенные"), 
                    selected = 1,
                    width = "100%"),
        if (input$el_drive_type == "SAR"  || input$el_drive_type == "SA" || input$el_drive_type == "SG") {
          selectInput("position_sensor", label = h5("Датчик положения"), 
                      choices = c("Токовый(RWG)", "Потенциометр"), 
                      selected = 1,
                      width = "100%")
        }
      )
    })
  
}
#_________________________________________________________________________________________________________________________________________________
### Shiny App ####
#_________________________________________________________________________________________________________________________________________________
# 
# options(shiny.port = 7775)
# options(shiny.host = "192.168.1.157")
# 
# options(shiny.port = 6545)
# options(shiny.host = "192.168.1.59") 
shinyApp(ui, server)

