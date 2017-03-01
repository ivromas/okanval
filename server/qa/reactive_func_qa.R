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
# Source file with functions that are needed to create QA1 and QA2 tables
# including *.docx and *.csv files
# 
# 
# coding UTF-8
#
Sys.setlocale('LC_ALL','Russian')

reactive_get_oper_table <- reactive({
  # error_str <- "Укажите материалы деталей"
  # Encoding(error_str) <- "UTF-8"
  validate(
    need(input$material_1 != "", "")
    )
    
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


reactiive_get_welding_and_overaly_table <- reactive({
  validate(
    need(input$material_1 != "", "")
  )
  
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


reactive_get_rv_drawing_number <- reactive({
  dynamic_part <- input$rv_drawing_number
  x <- dynamic_part
  return(x)
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


