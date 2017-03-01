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
                   radioButtons(inputId =  "el_drive_type","Тип привода", c("SA", "SAI"))
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
                   radioButtons(inputId =  "el_drive_type","Тип привода", c("SA", "SAI"))
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
                   min = 5, max = 300, step = 5, width = "100%"),
      numericInput("stem_stroke", "Ход штока [мм]",value = 100
                   , min = 10, max = 800, step = 100, width = "100%"),
      bsTooltip(id = "close_time", title = "от 5 до 240 с", 
                placement = "left", trigger = "focus"),
      bsTooltip(id = "stem_stroke", title = "от 10 до 800 мм", 
                placement = "left", trigger = "focus"),
      htmlOutput("isolated_stem_force"),
      bsAlert("torque_info")
      
    )
  })

output$isolated_stem_force <-
  renderUI({
    verticalLayout(
      numericInput("stem_force", "Максимальное усилие на штоке [кН]", value = 33,
                   min = values$stem_force_input_min, max = values$stem_force_input_max,
                   step = 0.1, width = "100%"),
      bsTooltip(id = "stem_force",  title = paste0("от ", values$stem_force_input_min," до ", 
                                                   values$stem_force_input_max, " кН"), 
                placement = "left", trigger = "focus"),
      bsPopover(id = "stem_force",
                title = HTML(paste0('<font color="black">При изменении типа электропривода и т.п.',
                                    'границы макс/мин значения пересчитываются после подбора электропривода</font>')),
                content = "", trigger = "focus", options = NULL)
    )
  })

output$LE <- 
  renderUI({
    if (input$el_drive_type == "SAR") {
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
        numericInput("stem_diameter", "Диаметр штока [мм]",value = 25.4, min = 12, max = 800,
                     width = "100%", step = 0.1),
        numericInput("multithread", "Многозаходность",value = 1, min = 1, max = 3, width = "100%"),
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
      if (input$el_drive_type == "SAR"  || input$el_drive_type == "SA" ) {
        selectInput("position_sensor", label = h5("Датчик положения"), 
                    choices = c("Токовый(RWG)", "Потенциометр"), 
                    selected = 1,
                    width = "100%")
      }
    )
  })
