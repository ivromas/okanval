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

#
Sys.setlocale('LC_ALL','Russian')


# observe({
#   toggle("main_menu"
#          )}, suspended = FALSE)
#          
#          
observeEvent(input$select_el_drive_btn, {
  
  str <- ""
  
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
})







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

observeEvent(input$select_valve, {
  get_safety_factor()
})

observeEvent(input$bellow, {
  get_safety_factor()
})

observeEvent(input$safety_factor_select,{
  get_safety_factor()
})


observeEvent(input$el_drive,{
             updateCheckboxInput(session, "LE_module", value = FALSE)
             updateCheckboxInput(session, "reducer_checkbox", value = FALSE)
             get_stem_force_boundary()
              }
             )

observeEvent(input$reducer_checkbox,{
             get_stem_force_boundary() }
)

observeEvent(input$LE_module,{
             get_stem_force_boundary() }
)
