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
