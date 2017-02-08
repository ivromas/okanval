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

Sys.setlocale('LC_ALL', 'Russian')



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
