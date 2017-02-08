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

observe({
  toggle("main_menu"
         )}, suspended = FALSE)

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
