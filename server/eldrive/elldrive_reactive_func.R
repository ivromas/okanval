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

values <- reactiveValues(stem_force_min = 3330, stem_force_max = 180830,
                         stem_force_input_min = 3.33, stem_force_input_max = 180.83,
                         safety_factor_no = "нет", safety_factor_recomended = "20", safety_factor_low = "10" )


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

reactive_get_el_drive <- reactive({
  
  if (input$safety_factor_select == "нет") {
    
    safety_factor <- 1
    
  } else {
    
    safety_factor <- input$safety_factor_select %>% as.integer()
    safety_factor <- safety_factor / 100 + 1
    
  }
  
  # time to mitue value
  close_time <- input$close_time / 60
  
  stem_stroke <- input$stem_stroke
  # from кН to H and adding safety factor to force
  stem_force <- as.integer(input$stem_force * 1000) * safety_factor
  # Шаг резьбы [мм]
  thread_pitch <- input$thread_pitch
  # Диаметр штока [мм]
  stem_diameter <- input$stem_diameter
  # Многозаходность
  multithread <- input$multithread
  
  if ((input$LE_module == TRUE && input$el_drive_type == "SAR")) {
    # speed in [mm per minute]
    speed <- stem_stroke / close_time
    
    x <- get_eldrive(con, type = "LE + SAR", speed = speed, stem_stroke = stem_stroke, stem_force = stem_force)
    
    return(x)
    
  } else {
    
    # time to mitue value
    close_time <- input$close_time / 60
    
    stem_stroke <- input$stem_stroke
    # from кН to H and adding safety factor to force
    stem_force <- as.integer(input$stem_force * 1000) * safety_factor
    # Шаг резьбы [мм]
    thread_pitch <- input$thread_pitch
    # Диаметр штока [мм]
    stem_diameter <- input$stem_diameter
    # Многозаходность
    multithread <- input$multithread
    
    # частота вращения приводного вала
    nesessary_number_of_rotations <- stem_stroke / (close_time * thread_pitch * multithread)
    
    nesessary_number_of_rotations <- round_any(nesessary_number_of_rotations, 1, ceiling) %>% as.integer()
    # Пределы регулирования муфты ограничения кутящего момента
    torque <- stem_force * stem_diameter / 2 * (multithread * thread_pitch / 
                                                  (pi *  stem_diameter) + 0.144) / 1000
    
    torque <- round_any(torque,10, ceiling) %>% as.integer()
    
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
      price_gc <- 0
      price_gc_string <- ""
    } else {
      number <- "-G"
      price_gc <- 200
      price_gc_string <- paste0(" + ", price_gc)
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
    
    
    if (input$el_drive_type == "SAR" || input$el_drive_type == "SA") {
      
      if (input$position_sensor == "Токовый(RWG)") {
        position_sensor <- "21.4/4"
      } else {
        position_sensor <- "12.E"
        price_gc <- price_gc + 150
        price_gc_string <- paste0(" + ", price_gc)
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
