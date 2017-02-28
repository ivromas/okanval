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

