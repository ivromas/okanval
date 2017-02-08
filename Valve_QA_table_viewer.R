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

current_folder <- "c:/_IR_F/okanval/"
# current_folder <- "C:/OkanVal/okanval_current_script/"
func_folder <- paste0(current_folder,file.path("func", "get_methods.R"))
source(func_folder)
login_folder <- paste0(current_folder,"server/Login.R")

#_________________________________________________________________________________________________________________________________________________
### Shiny UI ####
### 
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

  source(login_folder,  local = TRUE)

  # observe({
  #   toggle("main_menu"
  #          )})

  observe({
    if (USER$Logged == TRUE) {
      shinyjs::useShinyjs()
      toggle("main_menu")
      toggle("login_menu")
      updateTabItems(session, inputId = "main_menu", selected = "init_data")
      }
  })
    
  observe({if(input$select_valve != "Кран") {
      shinyjs::enable( "downloadDataDocx_qa2")
      shinyjs::enable( "download_qa2")
    } else{
      shinyjs::disable( "downloadDataDocx_qa2")
      shinyjs::disable( "download_qa2")
    }
    })
  
  observeEvent(input$close_time, {
    if (input$close_time < 5) {
      updateNumericInput(session, "close_time", value = 5)
    } else if (input$close_time > 500) {
      updateNumericInput(session, "close_time", value = 500)
    }
  })
  
  observeEvent(input$stem_stroke, {
    if (input$stem_stroke < 10) {
      updateNumericInput(session, "stem_stroke", value = 10)
    } else if (input$stem_stroke > 800) {
      updateNumericInput(session, "stem_stroke", value = 800)
    }
  })
  
  observeEvent(input$stem_force, {
    if (input$stem_force < 3400) {
      updateNumericInput(session, "stem_force", value = 3400)
    } else if (input$stem_force > 144000) {
      updateNumericInput(session, "stem_force", value = 144000)
    }
  })
  
  observeEvent(input$thread_pitch, {
    if (input$thread_pitch < 1) {
      updateNumericInput(session, "thread_pitch", value = 1)
    } else if (input$thread_pitch > 15) {
      updateNumericInput(session, "thread_pitch", value = 15)
    }
  })
  
  observeEvent(input$stem_diameter, {
    if (input$stem_diameter < 12) {
      updateNumericInput(session, "stem_diameter", value = 12)
    } else if (input$stem_diameter > 800) {
      updateNumericInput(session, "stem_diameter", value = 800)
    }
  })
  
  observeEvent(input$multithread, {
    if (input$multithread < 1) {
      updateNumericInput(session, "multithread", value = 1)
    } else if (input$multithread > 5) {
      updateNumericInput(session, "multithread", value = 5)
    }
  })
  
  electric_drive_print_text <- eventReactive(
    input$select_el_drive_btn, reactive_get_el_drive_full_name()
    )

  con <- okan_db_connect()
  
  
  reactive_get_oper_table <- reactive({
    
    dataframe_to_be_retuned <- get_distinct_names_of_qa_operations(con,type = "QA 1")
    qa_type <- input$select_qa_type
    tempr <- input$select_tempr
    tempr_oper <- input$select_tempr_oper
    pressure <- input$select_pressure
    valve_name <- SELECTED_VALVE()
    detail_list <- get_detial_list(con, SELECTED_VALVE())
    # drawing_number_of_detail <- data.frame(names = 0)
    drawing_number_of_detail <- data.frame(names=as.character(seq(length(detail_list$detail_name_rus))),
                                           stringsAsFactors=FALSE) 
    drawing_number_of_detail$details <- detail_list$detail_name_rus
    # drawing_number_of_detail$name <- x$valve_id
    for(i in 1 : length(detail_list$detail_name_rus)) {
      name <- paste0("material_", i)
      material <- input[[name]]
      material_sep_id <- get_material_input_info(con, material, type = "material type separate")
      detail = detail_list$detail_name_rus[i]
      x <- get_qa_operations_for_detail(con, valve_name, qa_type, tempr, tempr_oper, pressure, detail, material_sep_id)
      
      if(is.data.frame(x)){
        drawing_number_of_detail$names[i] <-
          paste0(reactive_get_rv_drawing_number(), "-",get_detail_input_info(con, detail, type = "drawing name"))
        names(x)[names(x) == 'operation_name_4table'] <- paste0(detail, "/", material)
        dataframe_to_be_retuned <- inner_join(dataframe_to_be_retuned, x, by = "operation_order")
        rm(x, material, name, detail)
      }else{
        del <-  which(drawing_number_of_detail$details == detail_list$detail_name_rus[i])
        drawing_number_of_detail <- drawing_number_of_detail[-c(del),]
        rm(material, name, detail)
      }
      
    }
    
    # transponse output data
    dataframe_to_be_retuned.t <- t(dataframe_to_be_retuned) %>% as.data.frame()
    colnames(dataframe_to_be_retuned.t) <- as.character(unlist(dataframe_to_be_retuned.t["operation_name_particular", ]))
    dataframe_to_be_retuned.t <- dataframe_to_be_retuned.t[- c(2), ]
    dataframe_to_be_retuned.t <- add_rownames(dataframe_to_be_retuned.t, "Detail/Material")
    dataframe_to_be_retuned.t$`Detail/Material`[c(1,2)] <- " / "
    # split detail and material to seperate columns
    x<-strsplit(dataframe_to_be_retuned.t$`Detail/Material`,"/")
    x<- as.data.frame(x)
    x.t <- t(x)
    x <- as.data.frame(x.t)
    dataframe_to_be_retuned.t$`Деталь` <- x$V1
    dataframe_to_be_retuned.t$`Материал` <- x$V2
    dataframe_to_be_retuned.t$`Detail/Material` <- NULL
    
    y <- data.frame(" ", " ")
    colnames(y) <- colnames(drawing_number_of_detail)
    drawing_number_of_detail <- rbind(y,drawing_number_of_detail)
    drawing_number_of_detail <- rbind(y,drawing_number_of_detail)
    dataframe_to_be_retuned.t$`Обозначение чертежа детали` <- drawing_number_of_detail$names
    dataframe_to_be_retuned.t <- dataframe_to_be_retuned.t[, c(25,26,27,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
    dataframe_to_be_retuned.t[] <- lapply(dataframe_to_be_retuned.t, as.character)
    dataframe_to_be_retuned.t[1,] <- c(1:ncol(dataframe_to_be_retuned.t))
    return(dataframe_to_be_retuned.t)
    
  })
  
  
  reactive_get_definition_of_designations <- reactive({
    qa_frame <- reactive_get_oper_table()
    qa_frame <- qa_frame[-c(1,2,3)]
    operation_name_4table <- unlist(qa_frame)
    qa_frame <- as.data.frame(operation_name_4table)
    qa_frame$operation_name_4table <- as.character(qa_frame$operation_name_4table)
    qa_frame <- qa_frame %>% distinct(operation_name_4table)
    op_names <- dbGetQuery(con, "SELECT DISTINCT list_of_operations.operation_name_4table_definition,
                           list_of_operations.operation_name_4table,
                           list_of_operations.operation_name_4definition
                           FROM	list_of_operations
                           WHERE 	list_of_operations.operation_id NOT BETWEEN 50 AND 103 AND
                           list_of_operations.operation_name_4table != '+' AND
                           list_of_operations.operation_name_4table != '-' AND
                           list_of_operations.operation_name_4table != '+c' AND
                           list_of_operations.operation_name_4table !=  'NULL';")
    Encoding(op_names$operation_name_4table_definition) <- "UTF-8"
    Encoding(op_names$operation_name_4table) <- "UTF-8"
    defenition_df <- inner_join(op_names, qa_frame, by = "operation_name_4table" )
    defenition_df <- na.omit(defenition_df)
    defenition_df <- defenition_df[-2]
    defenition_df <- distinct(defenition_df)
    print_string <- ""
    for( i in 1:length(defenition_df$operation_name_4definition)) {
      print_string <- paste0(print_string, "<p>", defenition_df$operation_name_4definition[i], " - ", defenition_df$operation_name_4table_definition[i], "; </p>")
    }
    return(print_string)
  })
  
  
  reactive_get_definition_of_designations_for_file <- reactive({
    qa_frame <- reactive_get_oper_table()
    qa_frame <- qa_frame[-c(1,2,3)]
    operation_name_4table <- unlist(qa_frame)
    qa_frame <- as.data.frame(operation_name_4table)
    qa_frame$operation_name_4table <- as.character(qa_frame$operation_name_4table)
    qa_frame <- qa_frame %>% distinct(operation_name_4table)
    op_names <- dbGetQuery(con, "SELECT DISTINCT list_of_operations.operation_name_4table_definition,
                           list_of_operations.operation_name_4table,
                           list_of_operations.operation_name_4definition
                           FROM	list_of_operations
                           WHERE 	list_of_operations.operation_id NOT BETWEEN 50 AND 103 AND
                           list_of_operations.operation_name_4table != '+' AND
                           list_of_operations.operation_name_4table != '-' AND
                           list_of_operations.operation_name_4table != '+c' AND
                           list_of_operations.operation_name_4table !=  'NULL';")
    Encoding(op_names$operation_name_4table_definition) <- "UTF-8"
    Encoding(op_names$operation_name_4table) <- "UTF-8"
    defenition_df <- inner_join(op_names, qa_frame, by = "operation_name_4table" )
    defenition_df <- na.omit(defenition_df)
    defenition_df <- defenition_df[-2]
    defenition_df <- distinct(defenition_df)
    print_string <- ""
    for( i in 1:length(defenition_df$operation_name_4definition)) {
      print_string <- paste0(print_string, defenition_df$operation_name_4definition[i], " - ", defenition_df$operation_name_4table_definition[i], "; \n")
    }
    return(print_string)
  })
  
  
  reactive_get_definition_of_designations_for_qa2 <- reactive({
    qa2_frame <- reactiive_get_welding_and_overaly_table()
    qa2_frame <- qa2_frame[-c(1,2),]
    welding_frame <- qa2_frame[c(1,6)]
    qa2_frame <- qa2_frame[-c(1,2,3,4,5,6)]
    operation_name_4table <- unlist(qa2_frame)
    qa2_frame <- as.data.frame(operation_name_4table)
    qa2_frame$operation_name_4table <- as.character(qa2_frame$operation_name_4table)
    qa2_frame <- qa2_frame %>% distinct(operation_name_4table)
    op_names <- dbGetQuery(con, "SELECT DISTINCT list_of_operations.operation_name_4table_definition,
		                       list_of_operations.operation_name_4table,
                           list_of_operations.operation_name_4definition,
                           list_of_operations.operation_name_def_order
                           FROM	list_of_operations
                           WHERE 	list_of_operations.operation_id  BETWEEN 50 AND 103 AND
                           list_of_operations.operation_name_4table != '+' AND
                           list_of_operations.operation_name_4table != '-' AND
                           list_of_operations.operation_name_4table != '+c' AND
                           list_of_operations.operation_name_4table !=  'NULL';")
    welding_names <- dbGetQuery(con,  "SELECT DISTINCT detail_for_con.welding_type,
                                        detail_for_con.welding_type_def
                                       FROM detail_for_con")
    Encoding(welding_names$welding_type_def) <- "UTF-8"
    Encoding(op_names$operation_name_4table_definition) <- "UTF-8"
    Encoding(op_names$operation_name_4table) <- "UTF-8"
    colnames(welding_frame)[colnames(welding_frame) == "Способ сварки/наплавки"] <- "welding_type"
    t <- which(welding_frame$welding_type == "GTAW+SMAW")
    defenition_df <- inner_join(op_names, qa2_frame, by = "operation_name_4table" )
    defenition_df <- na.omit(defenition_df)
    defenition_df <- defenition_df[order(defenition_df$operation_name_def_order),] 
    defenition_df <- defenition_df[-c(2,4)]
    defenition_df <- distinct(defenition_df)
    print_string <- ""
    if(!is.na(defenition_df$operation_name_4definition[1])){
      for( i in 1:length(defenition_df$operation_name_4definition)) {
        print_string <- paste0(print_string, "<p>", defenition_df$operation_name_4definition[i], " - ", defenition_df$operation_name_4table_definition[i], "; </p>")
      }
    }else{
      
    }
    
    if(length(t) > 0){
      definition_welding <- welding_names$welding_type_def[welding_names$welding_type == "GTAW+SMAW"]
      print_string <- paste0(print_string,"<p>", definition_welding,"; </p>")
    }else{
      definition_welding <- inner_join(welding_names,welding_frame, by = "welding_type")
      definition_welding <- definition_welding[2]
      definition_welding <- distinct(definition_welding)
      length(definition_welding$welding_type_def)
      for(i in 1:length(definition_welding$welding_type_def)){
        print_string <- paste0(print_string, "<p>", definition_welding$welding_type_def[i],"; </p>")
      }
    }
    return(print_string)
  })
  
  
  reactive_get_definition_of_designations_for_qa2_file <- reactive({
    qa2_frame <- reactiive_get_welding_and_overaly_table()
    welding_frame <- qa2_frame[c(1,6)]
    qa2_frame <- qa2_frame[-c(1,2,3,4,5,6)]
    operation_name_4table <- unlist(qa2_frame)
    qa2_frame <- as.data.frame(operation_name_4table)
    qa2_frame$operation_name_4table <- as.character(qa2_frame$operation_name_4table)
    qa2_frame <- qa2_frame %>% distinct(operation_name_4table)
    op_names <- dbGetQuery(con, "SELECT DISTINCT list_of_operations.operation_name_4table_definition,
                           list_of_operations.operation_name_4table,
                           list_of_operations.operation_name_4definition,
                           list_of_operations.operation_name_def_order
                           FROM	list_of_operations
                           WHERE 	list_of_operations.operation_id  BETWEEN 50 AND 103 AND
                           list_of_operations.operation_name_4table != '+' AND
                           list_of_operations.operation_name_4table != '-' AND
                           list_of_operations.operation_name_4table != '+c' AND
                           list_of_operations.operation_name_4table !=  'NULL';")
    welding_names <- dbGetQuery(con,  "SELECT DISTINCT detail_for_con.welding_type,
                                detail_for_con.welding_type_def
                                FROM detail_for_con")
    Encoding(welding_names$welding_type_def) <- "UTF-8"
    Encoding(op_names$operation_name_4table_definition) <- "UTF-8"
    Encoding(op_names$operation_name_4table) <- "UTF-8"
    colnames(welding_frame)[colnames(welding_frame) == "Способ сварки/наплавки"] <- "welding_type"
    t <- which(welding_frame$welding_type == "GTAW+SMAW")
    defenition_df <- inner_join(op_names, qa2_frame, by = "operation_name_4table" )
    defenition_df <- na.omit(defenition_df)
    defenition_df <- defenition_df[order(defenition_df$operation_name_def_order),] 
    defenition_df <- defenition_df[-c(2,4)]
    defenition_df <- distinct(defenition_df)
    print_string <- ""
    if(!is.na(defenition_df$operation_name_4definition[1])){
      for( i in 1:length(defenition_df$operation_name_4definition)) {
        print_string <- paste0(print_string, defenition_df$operation_name_4definition[i], " - ", defenition_df$operation_name_4table_definition[i], "; \n")
      }
    }else{
      
    }
    
    if(length(t) > 0){
      definition_welding <- welding_names$welding_type_def[welding_names$welding_type == "GTAW+SMAW"]
      print_string <- paste0(print_string, definition_welding,"; \n")
    }else{
      definition_welding <- inner_join(welding_names,welding_frame, by = "welding_type")
      definition_welding <- definition_welding[2]
      definition_welding <- distinct(definition_welding)
      for(i in 1:length(definition_welding$welding_type_def)){
        print_string <- paste0(print_string,  definition_welding$welding_type_def[i],"; \n")
      }
    }
    return(print_string)
  })
  
  
  reactive_get_valve_code <- reactive({
    qa_type <- input$select_qa_type
    select <- paste0("SELECT 
                     dn_4_code
                     FROM 
                     public.dn
                     WHERE 
                     dn.dn_value = '", input$dn_value,"';
                     ")
    dn <- dbGetQuery(con,select)
    dn <- dn$dn_4_code[1]
    valve_type <- get_valve_input_info(con, SELECTED_VALVE(), type = "type")
    material <- input$material_1
    material_type <- get_material_input_info(con, input$material_1, type = "body material type")
    qa_type <- get_qa_input_info(con, input$select_qa_type, type = "qa type name")
    control_type <- get_valve_input_info(con, SELECTED_VALVE(), type = "control_type")
    control_valve_type <- get_control_type_info(con, input$control_type, type = "4table")
    x <- paste0("OK.", dn, ".A",  valve_type, ".", material_type, qa_type, control_type, control_valve_type)
    return(x)
  })
  
  
  reactive_get_header_of_qa_table <- reactive({
    code <- reactive_get_valve_code()
    
    x <- paste0("Таблица контроля качества основных материалов изделия ", get_valve_input_info(con, SELECTED_VALVE(), type = "type_def")
                , ", номер чертежа ", code, " СБ, классификационное обозначение ", input$select_qa_type, " по НП-068-05")
    return(x)
  })
  
  
  reactive_get_header_of_qa2_table <- reactive({
    code <- reactive_get_valve_code()
    
    x <- paste0("Таблица контроля качества сварных швов изделия ", get_valve_input_info(con, SELECTED_VALVE(), type = "type_def")
                , ", номер чертежа ", code, " СБ, классификационное обозначение ", input$select_qa_type, " по НП-068-05")
    return(x)
  })
  
  
  reactive_get_rv_drawing_number <- reactive({
    dynamic_part <- input$rv_drawing_number
    x <- dynamic_part
    return(x)
  })
  
  
  reactiive_get_welding_and_overaly_table <- reactive({
    qa_type_name <- input$select_qa_type
    qa_type_welding <- get_qa_input_info(con, qa_type_name, type = "qa welding")
    tempr_name <- input$select_tempr
    valve_name <- SELECTED_VALVE()
    dataframe_to_be_retuned <- get_distinct_names_of_qa_operations(con,type = "QA 2")

    
    materials <- reactive_get_oper_table()
    materials <- materials[c(1,2,3)]
    materials$Деталь <- as.character(materials$Деталь)
    materials$Материал <- as.character(materials$Материал)
 
    details_for_welding_list <- get_welding_and_overlay_detail_list(con, valve_name)
    if("Корпус + патрубок" %in% details_for_welding_list$detail_4con_name){
      row_to_keep = which(details_for_welding_list$detail_4con_name != "Перех.патрубок")
      details_for_welding_list <- details_for_welding_list[row_to_keep,]
    }
    if(is.data.frame(details_for_welding_list)){
      details_for_welding_list$`Кат.сварных соединений` <- details_for_welding_list$detail_name_rus_welding
      yy <- which(!is.na(details_for_welding_list$`Кат.сварных соединений`))
      yyy <- which(is.na(details_for_welding_list$`Кат.сварных соединений`))
      details_for_welding_list$`Кат.сварных соединений`[yy] <- qa_type_welding
      details_for_welding_list$`Кат.сварных соединений`[yyy] <- "-"
      detail_list2 <- get_overlay_detail_list(con, SELECTED_VALVE())
      
      if(is.data.frame(detail_list2)){
        overlay_detail_list <- get_overlay_detail_list(con, valve_name)
        overlay_detail_list <- overlay_detail_list[(-c(2))]
        overlay_detail_list$input_overlay_type <- "0"
        
        for(i in 1 : length(overlay_detail_list$detail_4con_name)){
          detail_name <- overlay_detail_list$detail_4con_name[i]
          name <- paste0("overlay_", i)
          input_overlay_type <- input[[name]]
          overlay_detail_list$input_overlay_type[i] <- input_overlay_type
        }
        details_for_welding_list <- left_join(details_for_welding_list, overlay_detail_list,
                                               by = "detail_4con_name")
      }      
      x <- which(details_for_welding_list$input_overlay_type == "Отсутствует")
      x <- details_for_welding_list[x,]
      details_for_welding_list <- anti_join(details_for_welding_list,x, by = "input_overlay_type")
      
      details_for_welding_list$`Материал` <- "0"
      details_for_welding_list$`Обозначение чертежа деталей` <- "0"
      for(i in 1 : length(details_for_welding_list$detail_name_rus)) {
        if(!is.na(details_for_welding_list$detail_name_rus_welding[i])){
          x1 <- which(details_for_welding_list$detail_name_rus_welding[i] == materials$Деталь)
          xx1 <- materials[x1,]
          x2 <- which(details_for_welding_list$detail_name_rus[i] == materials$Деталь)
          xx2 <- materials[x2,]
          det_materilal <- paste0(xx1$Материал[1], " + ", xx2$Материал[1])
          det_designation <- paste0(xx1$`Обозначение чертежа детали`[1], " & ", xx2$`Обозначение чертежа детали`[1])
          material_of_current_detail <- xx2$Материал[1]
        }else{
          x2 <- which(details_for_welding_list$detail_name_rus[i] == materials$Деталь)
          xx2 <- materials[x2,]
          det_materilal <- paste0(xx2$Материал[1], " + ", details_for_welding_list$input_overlay_type[i])
          det_designation <- paste0(xx2$`Обозначение чертежа детали`[1])
          material_of_current_detail <- xx2$Материал[1]
        }
        details_for_welding_list$Материал[i] <- det_materilal
        details_for_welding_list$`Обозначение чертежа деталей`[i] <- det_designation
        
        material_type_separate_id <- get_material_input_info(con,material_of_current_detail,
                                      type = "material type separate")
        connection_type_id <- get_conncetion_type_info(con, details_for_welding_list$detail_4con_name[i],
                                                       type = "id")
        detail_4con_name_current <- details_for_welding_list$detail_4con_name[i]
        x <- get_qa2_operations_for_detail(con, qa_type_name, tempr_name, connection_type_id, material_type_separate_id, detail_4con_name_current)
        colnames(x)[colnames(x) == "operation_name_4table"] <- detail_4con_name_current
        dataframe_to_be_retuned <- inner_join(dataframe_to_be_retuned, x, by = "operation_order")
      }
      
      dataframe_to_be_retuned.t <- t(dataframe_to_be_retuned) %>% as.data.frame()
      colnames(dataframe_to_be_retuned.t) <- as.character(unlist(dataframe_to_be_retuned.t["operation_name_particular", ]))
      dataframe_to_be_retuned.t <- dataframe_to_be_retuned.t[- c( 2), ]
      dataframe_to_be_retuned.t <- add_rownames(dataframe_to_be_retuned.t, "Деталь")
      dataframe_to_be_retuned.t$Деталь[c(1)] <- "1"
      dataframe_to_be_retuned.t$Деталь[c(2)] <- "2"
      names(details_for_welding_list)[names(details_for_welding_list)=="detail_4con_name"] <- "Деталь"
      names(details_for_welding_list)[names(details_for_welding_list)=="number_of_welds"] <- "Кол-во сварных швов"
      names(details_for_welding_list)[names(details_for_welding_list)=="welding_type"] <- "Способ сварки/наплавки"
      details_for_welding_list <- details_for_welding_list[-c(2,3,4,8)]
      y <- data.frame("2", " "," "," "," "," ")
      colnames(y) <- colnames(details_for_welding_list)
      details_for_welding_list <- rbind(y,details_for_welding_list)
      y <- data.frame("1", " "," "," "," "," ")
      colnames(y) <- colnames(details_for_welding_list)
      details_for_welding_list <- rbind(y,details_for_welding_list)
      details_for_welding_list[] <- lapply(details_for_welding_list, as.character)
      
      dataframe_to_be_retuned.t <- inner_join(dataframe_to_be_retuned.t, details_for_welding_list, 
                                              by = "Деталь")
      dataframe_to_be_retuned.t <- dataframe_to_be_retuned.t[, c(1,24,25,22,23,21,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
      dataframe_to_be_retuned.t$Деталь[c(1,2)] <- " "
      dataframe_to_be_retuned.t[] <- lapply(dataframe_to_be_retuned.t, as.character)
      dataframe_to_be_retuned.t[1,] <- c(1:ncol(dataframe_to_be_retuned.t))
      
      return(dataframe_to_be_retuned.t)
      
    }
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
      # TODO написать метод возращающий 1 клапан по условиям см. get_valve_list
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
  
  
  output$qa1_header <- renderUI({
    str <- reactive_get_header_of_qa_table()
    Encoding(str) <- "UTF-8"
    headerPanel(tags$div(
      HTML(paste0("<strong>",'<font face="Bedrock" size="4">',str,"</font>","</strong>"))
    ))
  })
  
  output$qa2_header <- renderUI({
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
  
  output$details_and_materials <- 
    renderUI({
      detail_list <- get_detial_list(con, SELECTED_VALVE())
      lapply(1:length(detail_list$detail_name_rus), function(i) {
        detail_current = detail_list$detail_name_rus[i]
        Encoding(detail_current) <- "UTF-8"
        material_4_detail <- get_material_list(con, detail_current, SELECTED_VALVE())
        column(6,
               selectInput(paste0("material_",i), label = paste0(detail_current," ,материал:"),
                           # selectInput(paste0("material_",i), label = paste0("material_",i),
                           choices = material_4_detail$material_name,
                           selected = 1)
        )
      })
    })
  
  output$selected_valve_text_output <- renderUI({
     
    if (SELECTED_VALVE() == "Задвижка клиновая") {
      str <- paste0("Выбрана ",tolower(SELECTED_VALVE()))
    } else{
      str <- paste0("Выбран ",tolower(SELECTED_VALVE()))
    }
    headerPanel(tags$div(
      HTML(paste0("<strong>",'<font face="Bedrock" size="4">',str,"</font>","</strong>"))
    ))
  })
  
  output$details_and_overlays <-
    renderUI({
      detail_list2 <- get_overlay_detail_list(con, SELECTED_VALVE())
      if(is.data.frame(detail_list2)){
        lapply(1:length(detail_list2$detail_4con_name), function(i) {
          detail_current = detail_list2$detail_name_rus[i]
          Encoding(detail_current) <- "UTF-8"
          material_4_detail <- get_overlay_list(con)
          column(6,
                 selectInput(paste0("overlay_",i), label = paste0(detail_current," ,наплавка:"),
                             # selectInput(paste0("material_",i), label = paste0("material_",i),
                             choices = material_4_detail$overlay_type_name,
                             selected = 1)
          )
        })
      }
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
  
  output$eldrive_param <- 
    renderUI({
      fluidPage(
        numericInput("close_time", "Время закрытия [сек]",value = 30, min = 5, max = 500, step = 5, width = "100%"),
        numericInput("stem_stroke", "Ход штока [мм]",value = 10, min = 10, max = 800, step = 10, width = "100%"),
        numericInput("stem_force", "Максимальное усилие на штоке [Н]",value = 3400, min = 3400, max = 144000, 
                     step = 500, width = "100%")
      )
    })
  
  output$eldrive_print_name <-
      renderUI({
          headerPanel(tags$div(
            HTML(paste0("<strong>",'<font face="Bedrock" size="4", color="black">',
                        electric_drive_print_text(),"</font>","</strong>"))
          ))
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
  
  
  output$rv_draw_numb_disp <-
    renderText({paste0(input$rv_drawing_number,"-XX")})
  
  output$qa_table <-
    renderGvis({
      # TODO add normal progressbar
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Создание новой таблицы ТБ", value = 0.1)
      Sys.sleep(0.8)
      progress$set(message = "Создание новой таблицы ТБ", value = 0.4)
      x <- reactive_get_oper_table()
      # renderTable(reactive_get_oper_table())
      gvisTable(reactive_get_oper_table(), options=list(frozenColumns = 2, page = 'enable'))
      # progress$inc(1)
    })
  
  output$qa_table2 <-
    renderGvis({
      # TODO add normal progressbar
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Создание новой таблицы ТБ2", value = 0.3)
      Sys.sleep(0.8)
      progress$set(message = "Создание новой таблицы ТБ2", value = 0.5)
      if(SELECTED_VALVE() != "Кран шаровый"){
        gvisTable(reactiive_get_welding_and_overaly_table(), options=list(frozenColumns = 2, allowHtml = TRUE, showRowNumber = FALSE
                                                                          # cssClassNames = "{headerRow: 'myTableHeadrow'}", alternatingRowStyle = FALSE
        ))
      }
        # progress$inc(1)
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

