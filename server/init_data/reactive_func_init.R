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

