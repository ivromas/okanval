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
# observe({
#   toggle("main_menu"
#          )}, suspended = FALSE)

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




# observeEvent(input$close_time,
#              shinyjs::delay(1500,{closeAlert(session, "close_time_alert")
#                             if (is.na(input$close_time) || !is.numeric(input$close_time) ) {
#                               
#                               createAlert(session, "close_time_alert",
#                                           content = HTML('<i class="fa fa-times" aria-hidden="true"></i>'), style = "error", dismiss = FALSE)
#                             } else if (input$close_time < 5) {
#                               
#                               createAlert(session, "close_time_alert", 
#                                           content = HTML('<i class="fa fa-times" aria-hidden="true"></i>'), style = "error", dismiss = FALSE)
#                             } else if (input$close_time > 500) {
#                               
#                               createAlert(session, "close_time_alert",
#                                           content = HTML('<i class="fa fa-times" aria-hidden="true"></i>'), style = "error", dismiss = FALSE)
#                             } else {
#                               closeAlert(session, "close_time_alert")
#                             }
#   })
# )

# observeEvent(input$stem_stroke,
#              shinyjs::delay(1000, {
#                             closeAlert(session, "stem_stroke_alert")
#                             if (is.na(input$stem_stroke) || !is.numeric(input$stem_stroke)) {
#                               createAlert(session, "stem_stroke_alert",
#                                           content = HTML('<i class="fa fa-times" aria-hidden="true"></i>'), style = "error", dismiss = FALSE)
#                             } else if (input$stem_stroke < 10) {
#                               createAlert(session, "stem_stroke_alert",
#                                           content = HTML('<i class="fa fa-times" aria-hidden="true"></i>'), style = "error", dismiss = FALSE)
#                             } else if (input$stem_stroke > 800) {
#                               createAlert(session, "stem_stroke_alert",
#                                           content = HTML('<i class="fa fa-times" aria-hidden="true"></i>'), style = "error", dismiss = FALSE)
#                             } else {
#                               closeAlert(session, "stem_stroke_alert")
#                             }
#   })
# )

# observeEvent(input$stem_force,
#              shinyjs::delay(1000,
#                             if (is.na(input$stem_force) || !is.numeric(input$stem_force)) {
#                               updateNumericInput(session, "stem_force", value = 3400)
#                             } else if (input$stem_force < 3400) {
#                               updateNumericInput(session, "stem_force", value = 3400)
#                             } else if (input$stem_force > 144000) {
#                               updateNumericInput(session, "stem_force", value = 144000)
#                             }
#                           )
# )

# observeEvent(input$thread_pitch,
#              shinyjs::delay(1000,
#                             if (is.na(input$thread_pitch) || !is.numeric(input$thread_pitch)) {
#                               updateNumericInput(session, "thread_pitch", value = 1)
#                             } else if (input$thread_pitch < 1) {
#                               updateNumericInput(session, "thread_pitch", value = 1)
#                             } else if (input$thread_pitch > 15) {
#                               updateNumericInput(session, "thread_pitch", value = 15)
#                             }
#              )
# )

# observeEvent(input$stem_diameter, 
#              shinyjs::delay(1000,
#                             if (is.na(input$stem_diameter) || !is.numeric(input$stem_diameter)) {
#                               updateNumericInput(session, "stem_diameter", value = 12)
#                             } else if (input$stem_diameter < 12) {
#                               updateNumericInput(session, "stem_diameter", value = 12)
#                             } else if (input$stem_diameter > 800) {
#                               updateNumericInput(session, "stem_diameter", value = 800)
#                             }
#              )
# )

# observeEvent(input$multithread,
#              shinyjs::delay(1000,
#                             if (is.na(input$multithread) || !is.numeric(input$multithread)) {
#                               updateNumericInput(session, "multithread", value = 1)
#                             } else if (input$multithread < 1) {
#                               updateNumericInput(session, "multithread", value = 1)
#                             } else if (input$multithread > 5) {
#                               updateNumericInput(session, "multithread", value = 3)
#                             }
#              )
# )

# observe(input$el_drive_type,
#              {
#                updateCheckboxInput(session, "LE", value = FALSE)
#              })
