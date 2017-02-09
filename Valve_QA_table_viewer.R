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

# coding UTF-8


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

rm(list = ls())
# Sys.setlocale("LC_CTYPE", "en_US.UTF-8") 
current_folder <- "c:/_IR_F/okanval/"
# current_folder <- "C:/OkanVal/okanval_current_script/"
func_folder <- paste0(current_folder,file.path("func", "get_methods.R"))
conf_folder <- paste0(current_folder,"server/conf/config.R")
login_folder <- paste0(current_folder,"server/Login.R")
observ_folder <- paste0(current_folder, "server/observ_func.R")
reactive_qa_folder <- paste0(current_folder, "server/qa/reactive_func.R")
reactive_ui_qa_folder <- paste0(current_folder, "server/qa/reactive_ui.R")
reactive_eldrive_folder <- paste0(current_folder, "server/eldrive/elldrive_reactive_func.R")
reactive_ui_eldrive_folder <- paste0(current_folder, "server/eldrive/eldrive_reactive_ui.R")
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
  
  source(login_folder,  local = TRUE)
  source(observ_folder, local = TRUE)
  source(reactive_qa_folder, encoding = 'UTF-8',local = TRUE)
  source(reactive_ui_qa_folder, encoding = 'UTF-8',local = TRUE)
  source(reactive_eldrive_folder,encoding = 'UTF-8',local = TRUE)
  source(reactive_ui_eldrive_folder,encoding = 'UTF-8',local = TRUE)
  # source('C:/_IR_F/okanval/server/eldrive/eldrive_reactive_ui.R', encoding = 'UTF-8')
  
  electric_drive_print_text <- eventReactive(
    input$select_el_drive_btn, reactive_get_el_drive_full_name()
    )
  
  
  SELECTED_VALVE <- reactive({
    valve_general <- input$select_valve
    str <- ""
    if (valve_general == "Задвижка" || valve_general == "Затвор" || valve_general == "Кран" ||
        valve_general == "Клапан обратный") {
      return(input$select_valve_full)
    } else if (valve_general == "Клапан запорный") {
      if (input$bellow == TRUE)
      {
        valve_bellow_id <- 1
      } else {
        valve_bellow_id <- 2
      }
      
      if (input$cone == TRUE) {
        vale_type_by_socet <- 1
      } else {
        vale_type_by_socet <- 2
      }
      str <- paste0("SELECT
                    valve.valve_name
                    FROM 
                    public.valve, 
                    public.valve_bellow_type, 
                    public.valve_id_to_socet, 
                    public.valve_to_bellow, 
                    public.valve_type
                    WHERE 
                    valve_bellow_type.valve_bellow_id = valve_to_bellow.valve_bellow_id AND
                    valve_id_to_socet.valve_id = valve.valve_id AND
                    valve_to_bellow.valve_id = valve.valve_id AND
                    valve_type.valve_type_by_socet = valve_id_to_socet.valve_type_by_socet AND
                    valve_bellow_type.valve_bellow_id =", valve_bellow_id ," AND 
                    valve_type.valve_type_by_socet =" ,vale_type_by_socet ,"AND
                    valve.valve_id BETWEEN 14 AND 17;")
      x <- dbGetQuery(con,str)
      Encoding(x$valve_name) <- "UTF-8"
      return(x$valve_name)
      
    } else if (valve_general == "Клапан регулирующий") {
      if (input$bellow == TRUE)
      {
        valve_bellow_id <- 1
      } else {
        valve_bellow_id <- 2
      }
      
      if (input$cone == TRUE) {
        vale_type_by_socet <- 1
      } else {
        vale_type_by_socet <- 2
      }
      
      if (input$plug == TRUE) {
        plug_type_id <- 1
      } else {
        plug_type_id <- 2
      }
      str <- paste0("SELECT 
                    valve.valve_name
                    FROM 
                    public.valve, 
                    public.valve_to_bellow, 
                    public.valve_to_plug, 
                    public.valve_type, 
                    public.valve_id_to_socet, 
                    public.valve_bellow_type, 
                    public.valve_plug_type
                    WHERE 
                    valve_to_bellow.valve_bellow_id = valve_bellow_type.valve_bellow_id AND
                    valve_to_bellow.valve_id = valve.valve_id AND
                    valve_to_plug.valve_id = valve.valve_id AND
                    valve_id_to_socet.valve_id = valve.valve_id AND
                    valve_id_to_socet.valve_type_by_socet = valve_type.valve_type_by_socet AND
                    valve_plug_type.plug_type_id = valve_to_plug.plug_type_id AND
                    valve.valve_id <= 8 AND
                    valve_bellow_type.valve_bellow_id =", valve_bellow_id, " AND 
                    valve_type.valve_type_by_socet =", vale_type_by_socet, " AND
                    valve_plug_type.plug_type_id =", plug_type_id, ";
                    ")
      x <- dbGetQuery(con,str)
      Encoding(x$valve_name) <- "UTF-8"
      return(x$valve_name)
    }
  })
  
  reactive_get_header_of_qa_table <- reactive({
    validate(
      need(input$material_1 != "", {message = "УКАЖИТЕ МАТЕРИАЛЫ ДЛЯ ДЕТАЛЕЙ ВО ВКЛАДКЕ 'Материалы деталей'"
      }),
      need(input$material_2 != "",{shinyjs::disable( "downloadDataDocx")
        shinyjs::disable( "downloadData")})
    )
    shinyjs::enable( "downloadDataDocx")
    shinyjs::enable( "downloadData")
    code <- reactive_get_valve_code()
    
    x <- paste0("Таблица контроля качества основных материалов изделия ", get_valve_input_info(con, SELECTED_VALVE(), type = "type_def")
                , ", номер чертежа ", code, " СБ, классификационное обозначение ", input$select_qa_type, " по НП-068-05")
    return(x)
  })
  
  
  reactive_get_header_of_qa2_table <- reactive({
    validate(
      need(input$material_1 != "", {message = "УКАЖИТЕ МАТЕРИАЛЫ ДЛЯ ДЕТАЛЕЙ ВО ВКЛАДКЕ 'Материалы деталей'"
        }),
      need(input$material_2 != "",{shinyjs::disable( "downloadDataDocx_qa2")
        shinyjs::disable( "download_qa2")})
    )
    shinyjs::enable( "downloadDataDocx_qa2")
    shinyjs::enable( "download_qa2")
    
    code <- reactive_get_valve_code()
    
    x <- paste0("Таблица контроля качества сварных швов изделия ", get_valve_input_info(con, SELECTED_VALVE(), type = "type_def")
                , ", номер чертежа ", code, " СБ, классификационное обозначение ", input$select_qa_type, " по НП-068-05")
    return(x)
  })
  
  
  reactive_get_el_drive <- reactive({
    
    if (input$bellow == TRUE) {
      safety_factor <- 1.5
    } else {
      safety_factor <- 1.2
    }
    
    # time to mitue value
    close_time <- input$close_time / 60
    stem_stroke <- input$stem_stroke
    # adding safety factor to force
    stem_force <- input$stem_force * safety_factor
    
    if ((input$LE_module == TRUE)) {
      # speed in [mm per minute]
      speed <- stem_stroke / close_time
      x <- get_eldrive(con, type = "LE + SAR", speed = speed, stem_stroke = stem_stroke, stem_force = stem_force)
      Encoding(x) <- "UTF-8"
      return(x)
    } else {
      # Шаг резьбы [мм]
      thread_pitch <- input$thread_pitch
      # Диаметр штока [мм]
      stem_diameter <- input$stem_diameter
      # Многозаходность
      multithread <- input$multithread
      # частота вращения приводного вала
      nesessary_number_of_rotations <- stem_stroke / (close_time * thread_pitch * multithread)
      # Пределы регулирования муфты ограничения кутящего момента
      torque <- stem_force * stem_diameter / 2 * (nesessary_number_of_rotations * thread_pitch / 
                                                     (pi *  stem_diameter) + 0.144) / 1000
      torque <- round(torque) %>% as.integer()
      
      x <- get_eldrive(con, type = input$el_drive_type, stem_stroke = stem_stroke, torque = torque, 
                       nesessary_number_of_rotations = nesessary_number_of_rotations)
      return(x)
    }
  })
  
  
  reactive_get_el_drive_full_name <- reactive({
    
    x <- reactive_get_el_drive()
    
    if ( nrow(x) == 0) {
      str <- paste0("Введены невалидные исходные параметры")
      
    } else {
      
      if (input$gold_plated_contacts == "Стандартные") {
        number <- ""
      } else {
        number <- "G"
      }
      # LE module dependencies
      if (input$LE_module == FALSE) {
        str_part1 <- paste0(x$flange_fittings, "A")
        str_part_last <- ""
      } else {
        str_part1 <- paste0(x$flange_fittings, "LE")
        str_part_last <- paste0("+", x$modul_type)
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

      
      if (input$el_drive_type == "SAR") {
        str_part3 <- paste0("21/4-S105-11-IP67-KS-TP104/",str_part_add_3)
      } else if (input$el_drive_type == "SARI") {
        str_part3 <- paste0("12.E-SH-148-IP68-KSG-TPA00R0AE-0A0-000")
      }
      
      str <- paste0("Указанным исходным данным соответствует привод ", x$eldrive_name,"-", str_part1,"-", "380/50*3", "-", x$rotation_speed, "-", 
                    "10.1-XX-",str_part2,"-",str_part3,str_part_last )
    }
    return(str)
  })
  
  
  output$qa1_header <- 
    renderUI({
    str <- reactive_get_header_of_qa_table()
    Encoding(str) <- "UTF-8"
    headerPanel(tags$div(
      HTML(paste0("<strong>",'<font face="Bedrock" size="4">',str,"</font>","</strong>"))
    ))
  })
  
  output$qa2_header <- 
    renderUI({
    if ( SELECTED_VALVE() != "Кран шаровый" ) {
      str <- reactive_get_header_of_qa2_table()
      headerPanel(tags$div(
        HTML(paste0("<strong>",'<font face="Bedrock" size="4">',str,"</font>","</strong>"))
      ))
    }else{
      headerPanel(h4("ТБ2 не требуется"))
    }
  })
  
  
  output$dynamic_select_pressure <-
    renderUI({
      current_qa_type <- input$select_qa_type
      if(current_qa_type == "2ВIIIс" || current_qa_type == "3СIIIс"){
        selectInput("select_pressure", label = h5("Класс давления по ANSI корпуса"),
                    choices = pressure_list$pressure_type[2],
                    selected = 1,
                    width = "80%")
      }else {
        selectInput("select_pressure", label = h5("Класс давления по ANSI корпуса"),
                    choices = pressure_list$pressure_type,
                    selected = 1,
                    width = "80%")
      }
      
    })
  
  
  output$dynamic_select_valve <-
    renderUI({
      valve_general <- input$select_valve
      if (valve_general == "Задвижка" || valve_general == "Затвор" || valve_general == "Кран" ||
          valve_general == "Клапан обратный") {
        x <- get_valve_list(con, type = "part", valve_general_name = input$select_valve)
        selectInput("select_valve_full", label = h5("Клапан"),
                    choices = x$valve_name,
                    selected = 1,
                    width = "80%")
      } else if (valve_general == "Клапан запорный") {
        fluidPage(
          checkboxInput("bellow", "C сильфоном", value = FALSE, width = NULL),
          checkboxInput("cone", "С перех.патрубком", value = FALSE, width = NULL)
        )
      } else if (valve_general == "Клапан регулирующий") {
        fluidPage(
          checkboxInput("bellow", "C сильфоном", value = FALSE, width = NULL),
          checkboxInput("cone", "С перех.патрубком", value = FALSE, width = NULL),
          checkboxInput("plug", "C разгруженным золотником", value = FALSE, width = NULL)
        )
      }
      
    })
  
  output$selected_valve_text_output <-
    renderUI({
     
    if (SELECTED_VALVE() == "Задвижка клиновая") {
      str <- paste0("Выбрана ",tolower(SELECTED_VALVE()))
    } else{
      str <- paste0("Выбран ",tolower(SELECTED_VALVE()))
    }
    headerPanel(tags$div(
      HTML(paste0("<strong>",'<font face="Bedrock" size="4">',str,"</font>","</strong>"))
    ))
  })
  
  output$el_drive_select <-
    renderUI({
      if (input$select_valve == "Клапан регулирующий" && input$control_type == "Электропривод") {
        fluidPage(
          box(width = 12,
              background = "light-blue",
              column(width = 4,
                     radioButtons(inputId =  "el_drive_type","Тип привода", c("SAR", "SARI"))
              ),
              column(width = 8,
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
          )
        )
      } else{
        headerPanel(tags$div(
          HTML(paste0("<strong>",'<font face="Bedrock" size="4">',"Электропривод не требуется","</font>","</strong>"))
        ))
      }
    })
  
  output$eldrive_print_name <-
    renderUI({
      if (input$select_valve == "Клапан регулирующий" && input$control_type == "Электропривод") {
      headerPanel(tags$div(
        HTML(paste0("<strong>",'<font face="Bedrock" size="4", color="black">',
                    electric_drive_print_text(),"</font>","</strong>"))
      ))
      }
    })
  
  output$eldrive_param <- 
    renderUI({
      fluidPage(
        numericInput("close_time","Время закрытия [сек]" ,value = 30, min = 5, max = 500, step = 5, width = "100%"),
        numericInput("stem_stroke", "Ход штока [мм]",value = 10, min = 10, max = 800, step = 10, width = "100%"),
        numericInput("stem_force", "Максимальное усилие на штоке [Н]",value = 3400, min = 3400, max = 144000, 
                     step = 500, width = "100%")
      )
    })
  output$LE <- 
    renderUI({
      if (input$el_drive_type == "SAR") {
        checkboxInput(inputId = "LE_module", h5("LE-модуль"), value = FALSE)
      }
    })
  
  output$thread <-
    renderUI({
      if (input$LE_module == FALSE || input$el_drive_type == "SARI") {
        fluidPage(
          numericInput("thread_pitch", "Шаг резьбы [мм]",value = 5, min = 1, max = 15, width = "100%"),
          numericInput("stem_diameter", "Диаметр штока [мм]",value = 12, min = 12, max = 800, width = "100%"),
          numericInput("multithread", "Многозаходность",value = 1, min = 1, max = 5, width = "100%")
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
                    width = "100%")
      )
    })
  
  
  
  output$text <-
    renderText({
      HTML(paste0(
        "<p><b>Обозначения:</b></p>
        <p>РГК  - радиографический контроль;</p>
        <p>УЗК  - ультразвуковой контроль;</p>
        <p>МПД  - магнитопорошковый контроль;</p>
        <p>+   - контроль производится;</p>
        <p>-   - контроль не производится;</p>
        <p>+c  - результаты испытаний подтверждаются сертификатом.</p>",
        reactive_get_definition_of_designations()),
        "<p> </p>"
      )
    })
  
  output$text_qa2 <-
    renderText({
      if (SELECTED_VALVE() != "Кран шаровый") {
      HTML(paste0(
        "<p><b>Обозначения:</b></p>
        <p>ВК   - входной контроль;</p>
        <p>ВиК  - визуальный и измерительный контроль;</p>
        <p>РГК  - радиографический контроль;</p>
        <p>УЗК  - ультразвуковой контроль;</p>
        <p>МПД  - магнитопорошковый контроль;</p>
        <p>+   - контроль производится;</p>
        <p>-   - контроль не производится;</p>
        <p>+c  - результаты испытаний подтверждаются сертификатом;</p>",
        reactive_get_definition_of_designations_for_qa2()),
        "<p> </p>"
      )
      }
    })

  output$download_qa2 <- downloadHandler(
    filename = function() {
      paste0('QA2_table_', Sys.Date(), '.csv')
    },
    content = function(file) {
      data <- reactiive_get_welding_and_overaly_table()
      header <- paste0(reactive_get_header_of_qa2_table(),"\n")
      bottom <- paste0(
        "Обозначения:
ВК   - входной контроль;
ВиК  - визуальный и измерительный контроль;
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
 +   - контроль производится;
 +c  - результаты испытаний подтверждаются сертификатом;",
        "\n",
        reactive_get_definition_of_designations_for_qa2_file())
        
      cat(header, file=file, append = TRUE, sep =";" )
      write.table(data, file=file, append=TRUE, sep=';', row.names = FALSE, quote = TRUE)
      cat(bottom, file=file, append = TRUE, sep =";" )
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('QA_table_', Sys.Date(), '.csv')
    },
    content = function(file) {
      data <- reactive_get_oper_table()
      header <- paste0(reactive_get_header_of_qa_table(),"\n")
      bottom <- paste0(
        "Обозначения:
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
 +   - контроль производится;
 -   - контроль не производится;
 +c  - результаты испытаний подтверждаются сертификатом;",
        "\n",
        reactive_get_definition_of_designations_for_file())
      cat(header, file=file, append = TRUE, sep =";" )
      write.table(data, file=file, append=TRUE, sep=';', row.names = FALSE)
      cat(bottom, file=file, append = TRUE, sep =";" )
    }
  )
  
  output$downloadDataDocx <- downloadHandler(
    filename = function() {
      paste0('QA_table_', Sys.Date(), '.docx')
    },
    content = function(file) {
      data <- reactive_get_oper_table()
      parprop <- parProperties(padding = 2)
      cellprop <- cellProperties( text.direction = "btlr" )
      data_header <- colnames(data)
      datadata <- FlexTable(data, header.columns = FALSE) %>% 
        addHeaderRow( value = c("", "","","Наименование операции"), colspan = c( 1,1,1, 24),
                      par.properties = parprop, text.properties = textNormal() ) %>% 
        addHeaderRow( value=data_header, cell.properties = cellprop, text.properties = textNormal() )
      header <- paste0(reactive_get_header_of_qa_table(),"\n")
      bottom <- paste0(
        "Обозначения:
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
+   - контроль производится;
-   - контроль не производится;
+c  - результаты испытаний подтверждаются сертификатом;",
"\n",
        reactive_get_definition_of_designations_for_file(),
"

Настоящую таблицу рассматривать совместно с ОСТ 108.004.10 и комплектом конструкторской документации."
        )
      doc <- docx(  ) %>% addParagraph(header) %>% addFlexTable( datadata ) %>% addParagraph(bottom) 
      writeDoc(doc, file=file )
    }
  )
  output$downloadDataDocx_qa2 <- downloadHandler(
    filename = function() {
      paste0('QA2_table_', Sys.Date(), '.docx')
    },
    content = function(file) {
      data <- reactiive_get_welding_and_overaly_table()
      header <- paste0(reactive_get_header_of_qa2_table(),"\n")
      bottom <- paste0(
        "Обозначения:
ВК   - входной контроль;
ВиК  - визуальный и измерительный контроль;
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
 +   - контроль производится;
 +c  - результаты испытаний подтверждаются сертификатом;",
        "\n",
        reactive_get_definition_of_designations_for_qa2_file(),
"

Настоящую таблицу рассматривать совместно с ОСТ 108.004.10 и комплектом конструкторской документации."
        )
      cellprop <- cellProperties( text.direction = "btlr" )
      parprop <- parProperties(padding = 2)
      data_header <- colnames(data)
      datadata <- FlexTable(data, header.columns = FALSE) %>% 
        addHeaderRow( value = c("", "","","","","","Наименование операции"), colspan = c( 1,1,1,1,1,1, 19),
                      par.properties = parprop, text.properties = textNormal() ) %>% 
        addHeaderRow( value=data_header, cell.properties = cellprop, text.properties = textNormal() )
      doc <- docx(  ) %>% addParagraph(header) %>% addFlexTable( datadata ) %>% addParagraph(bottom) 
      writeDoc(doc, file=file )
    }
  )
  
}
#_________________________________________________________________________________________________________________________________________________
### Shiny App ####
#_________________________________________________________________________________________________________________________________________________
# options(shiny.port = 7775)
# options(shiny.host = "192.168.1.157")
# 
# options(shiny.port = 6545)
# options(shiny.host = "192.168.1.59")
# 
shinyApp(ui, server)

