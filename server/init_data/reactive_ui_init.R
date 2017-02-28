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
# some dynamic UI part of project (for QA tables)
# coding UTF-8
# 
Sys.setlocale('LC_ALL','Russian')

output$rv_draw_numb_disp <-
  renderText({paste0(input$rv_drawing_number,"-XX")})
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
    headerPanel(tags$div( id = "header_panel_vale",
                          HTML(paste0("<strong>",'<font face="Bedrock" size="4">',str,"</font>","</strong>"))
    ))
    
  })
