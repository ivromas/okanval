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


library(shiny)
library(shinydashboard)
library(RPostgreSQL)
library(dplyr)
library(googleVis)
library(Gmisc)
library(reshape)
library(tidyr)
library(shinythemes)
library(shinyjs)
# Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_121\\jre")
library(ReporteRs)


rm(list = ls())

#_________________________________________________________________________________________________________________________________________________
### Usful functions ####
#_________________________________________________________________________________________________________________________________________________
okan_db_connect <- function() {
  pw <- {
    "root"
  }
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "qa_db",
                   host = "okanval.okan.su", port = 6543,
                   user = "postgres", password = pw)
  # con <- dbConnect(drv, dbname = "qa_db",
  #                  host = "localhost", port = 5432,
  #                  user = "postgres", password = pw)
  return(con)
}


okan_db_disconnect <- function(con){
  dbDisconnect(con)
}


get_dn_list <- function(con){
  x <- dbGetQuery(con,"SELECT dn_value FROM dn")
  return(x)
}


get_control_type_list <- function(con){
  x <- dbGetQuery(con,"SELECT control_type_def FROM valve_control_type")
  Encoding(x$control_type_def) <- "UTF-8"
  return(x)
}


get_control_type_info <- function(con, ct_name, type = NaN){
  if(type == "4table"){
    ct_select <- paste0("SELECT control_type_4code
                        FROM valve_control_type
                        WHERE control_type_def='", ct_name,"'")
    x <- dbGetQuery(con,ct_select)
    Encoding(x$control_type_4code) <- "UTF-8"
    ct_name_out<- x$control_type_4code[1]
    return(ct_name_out)
  }else{
    return(NaN)
  }
  
}


get_valve_list <- function(con){
  dbSendQuery(con, "SET NAMES 'UTF-8'")
  x <- dbGetQuery(con,"SELECT valve_id, valve_name FROM valve")
  Encoding(x$valve_name) <- "UTF-8"
  x <- x[order(x$valve_id),]
  return(x)
}


get_valve_input_info <- function(con,valve_name, type=NaN){
  if(type == "id"){
    valve_select <- paste0("SELECT valve.valve_id
                           FROM valve
                           WHERE valve.valve_name='",valve_name,"'")
    valve_id_frame <- dbGetQuery(con, valve_select)
    valve_id <- valve_id_frame$valve_id[1]
    return(valve_id)
  }else if(type == "type") {
    valve_select <- paste0( "SELECT valve.kind_of_valve
                            FROM valve
                            WHERE valve.valve_name='",valve_name,"'")
    valve_type_frame <- dbGetQuery(con, valve_select)
    valve_type <- valve_type_frame$kind_of_valve[1]
    return(valve_type)
  }else if(type == "type_def"){
    valve_select <- paste0( "SELECT valve.def_kind_of_valve
                            FROM valve
                            WHERE valve.valve_name='",valve_name,"'")
    valve_type_frame <- dbGetQuery(con, valve_select)
    Encoding(valve_type_frame$def_kind_of_valve) <- "UTF-8"
    valve_type <- valve_type_frame$def_kind_of_valve[1]
    return(valve_type)
  }else if(type == "control_type") {
    valve_select <- paste0( "SELECT valve.organ_type
                            FROM valve
                            WHERE valve.valve_name='",valve_name,"'")
    valve_control_type_frame <- dbGetQuery(con, valve_select)
    valve_control_type <- valve_control_type_frame$organ_type[1]
    return(valve_control_type)
  }else {
    return(NaN)
  }
}


get_qa_type_list <- function(con){
  x <- dbGetQuery(con, "SELECT valve_qa_type_id, valve_qa_type_name FROM valve_qa_type")
  Encoding(x$valve_qa_type_name) <- "UTF-8"
  return(x)
}


get_qa_input_info <- function(con, valve_qa_type_name, type = NaN) {
  if(type == "id"){
    qa_select <- paste0("SELECT valve_qa_type.valve_qa_type_id
                        FROM valve_qa_type
                        WHERE valve_qa_type.valve_qa_type_name='", valve_qa_type_name,"'")
    qa_id_frame <- dbGetQuery(con, qa_select)
    qa_type_id <- qa_id_frame$valve_qa_type_id[1]
    return(qa_type_id)
  }else if(type == "qa type name") {
    qa_select <- paste0("SELECT valve_qa_type.valve_qa_type_4code
                        FROM valve_qa_type
                        WHERE valve_qa_type.valve_qa_type_name='", valve_qa_type_name,"'")
    qa_type_name_frame <- dbGetQuery(con, qa_select)
    qa_type_name <- qa_type_name_frame$valve_qa_type_4code[1]
    return(qa_type_name)
  }else if(type == "qa welding") {
    qa_select <- paste0("SELECT valve_qa_type.valve_qa_type_4qa2
                        FROM valve_qa_type
                        WHERE valve_qa_type.valve_qa_type_name='", valve_qa_type_name,"'")
    qa_type_name_frame <- dbGetQuery(con, qa_select)
    qa_type_name <- qa_type_name_frame$valve_qa_type_4qa2[1]
    return(qa_type_name)
  }else {
    return(NaN)
  }
}


get_tempr_list <- function(con){
  x <- dbGetQuery(con, "SELECT tempr_id, tempr_value_more_than_100 FROM tempr")
  Encoding(x$tempr_value_more_than_100) <- "UTF-8"
  return(x)
}


get_tempr_input_info <- function(con, tempr, type = NaN){
  if(type == "id"){
    tempr_select <- paste0("SELECT tempr.tempr_id
                           FROM tempr
                           WHERE tempr.tempr_value_more_than_100='",tempr,"'")
    tempr_id_frame <- dbGetQuery(con, tempr_select)
    tempr_id <- tempr_id_frame$tempr_id[1]
    return(tempr_id)
  }else {
    return(NaN)
  }
}


get_tempr_oper_list <- function(con){
  x <- dbGetQuery(con, "SELECT tempr_operation_id, tempr_oper_value_more_than_20 FROM tempr_operation")
  Encoding(x$tempr_oper_value_more_than_20) <- "UTF-8"
  return(x)
}


get_tempr_oper_input_info <- function(con, tempr_oper, type = NaN){
  if(type == "id"){
    tempr_oper_select <- paste0("SELECT tempr_operation.tempr_operation_id
                                FROM tempr_operation
                                WHERE tempr_operation.tempr_oper_value_more_than_20='",tempr_oper,"'")
    tempr_oper_id_frame <- dbGetQuery(con, tempr_oper_select)
    tempr_oper_id <- tempr_oper_id_frame$tempr_operation_id[1]
    return(tempr_oper_id)
  }else {
    return(NaN)
  }
}


get_pressure_list <- function(con){
  x <- dbGetQuery(con, "SELECT pressure_id, pressure_type FROM pressure")
  Encoding(x$pressure_type) <- "UTF-8"
  return(x)
}


get_pressure_input_info <- function(con, pressure, type = NaN){
  if(type == "id"){
    pressure_select <- paste0("SELECT pressure.pressure_id
                              FROM pressure
                              WHERE pressure.pressure_type='",pressure,"'")
    pressure_id_frame <- dbGetQuery(con, pressure_select)
    pressure_id <- pressure_id_frame$pressure_id[1]
    return(pressure_id)
  }else {
    return(NaN)
  }
}


get_detial_list <- function(con,valve_name){
  valve <- which(valve_list$valve_name == valve_name, arr.ind = TRUE)
  valve_id <- valve_list$valve_id[valve]
  select_details <- paste0("SELECT detail.detail_id, detail.detail_name_rus, detail.detail_name_eng
                           FROM detail INNER JOIN (valve INNER JOIN valve_detail ON valve.valve_id = valve_detail.valve_id) ON detail.detail_id = valve_detail.detail_id
                           WHERE (((valve.valve_id)=", valve_id, "))"
  )
  x <- dbGetQuery(con, select_details)
  Encoding(x$detail_name_rus) <- "UTF-8"
  return(x)
}


get_detail_input_info <- function(con, detail, type = NaN){
  if(type == "id"){
    detail_select <- paste0("SELECT detail.detail_id
                            FROM detail
                            WHERE detail.detail_name_rus='",detail,"';
                            ")
    detail_id_frame <- dbGetQuery(con, detail_select)
    detail_id <- detail_id_frame$detail_id[1]
    return(detail_id)
  }else if(type == "drawing name"){
    detail_select <- paste0("SELECT detail.detail_figure_id
                            FROM detail
                            WHERE detail.detail_name_rus='",detail,"';
                            ")
    detail_fig_id_frame <- dbGetQuery(con, detail_select)
    detail_fig_id <- detail_fig_id_frame$detail_figure_id[1]
    return(detail_fig_id)
  }else {
    return(NaN)
  }
}


get_overlay_detail_list <- function (con, valve_name){
  select_details <- paste0("SELECT 
                        	detail_for_con.detail_4con_name,
                        	detail_for_con.detail_name_rus
                           FROM 
                           public.valve, 
                           public.detail_for_con, 
                           public.detail_4con_to_valve
                           WHERE 
                           valve.valve_id = detail_4con_to_valve.valve_id AND
                           detail_for_con.detail_4con_id = detail_4con_to_valve.detail_4con_id AND
                           detail_for_con.detail_4con_id >= '9' AND 
                           valve.valve_name = '", valve_name, "';"
  )
  x <- dbGetQuery(con, select_details)
  if(nrow(x) !=0)
    {
    Encoding(x$detail_4con_name) <- "UTF-8"
    Encoding(x$detail_name_rus) <- "UTF-8"
    return(x)
  }else{
    return(NaN)
  }
}


get_material_list <- function(con, detail, valve_name){
  detail_list <- get_detial_list(con, valve_name)
  detail_frame_index <- which(detail_list$detail_name_rus == detail, arr.ind = TRUE)
  detail_id <- detail_list$detail_id[detail_frame_index]
  selected_material <- paste0("SELECT material.material_id, material.material_name
                              FROM material INNER JOIN (detail INNER JOIN detail_material ON detail.detail_id = detail_material.detail_id) ON material.material_id = detail_material.material_id
                              WHERE (((detail.detail_id)=",detail_id,"))"
  )
  x <- dbGetQuery(con, selected_material)
  Encoding(x$material_name) <- "UTF-8"
  return(x)
}


get_material_input_info <- function(con, material, type = NaN){
  if(type == "id"){
    material_select <- paste0("SELECT material.material_id
                              FROM material
                              WHERE (((material.material_name)='",material,"'));
                              ")
    material_id_frame <- dbGetQuery(con,material_select)
    material_id <- material_id_frame$material_id[1]
    return(material_id)
  }else if(type == "body material type") {
    bmt_select <- paste0("SELECT
                         meterial_type_general.material_type_4code
                         FROM 
                         public.detail, 
                         public.meterial_type_general, 
                         public.detail_material, 
                         public.material
                         WHERE 
                         detail.detail_id = detail_material.detail_id AND
                         detail_material.material_id = material.material_id AND
                         material.material_type_general_id = meterial_type_general.material_type_general_id AND
                         detail.detail_id = 1 AND 
                         material.material_name = '", material,"';
                         ")
    bmt_frame <- dbGetQuery(con, bmt_select)
    body_material_type <- bmt_frame$material_type_4code[1]
    return(body_material_type)
  }else if(type == "material type separate") {
    select <- paste0("SELECT 
                     material_type_separate.material_type_separete_id
                     FROM 
                     public.material, 
                     public.meterial_type_general, 
                     public.material_type_separate, 
                     public.general_to_separate
                     WHERE 
                     material.material_type_general_id = meterial_type_general.material_type_general_id AND
                     meterial_type_general.material_type_general_id = general_to_separate.material_type_general_id AND
                     general_to_separate.material_type_separete_id = material_type_separate.material_type_separete_id AND
                     material.material_name = '",material,"';
                      ")
    material_id_frame <- dbGetQuery(con,select)
    material_id <- material_id_frame$material_type_separete_id[1]
    return(material_id)
  } else {
    return(NaN)
  }
}


get_overlay_list <- function(con) {
  select_overlay <- paste0("SELECT overlay_type.overlay_type_name
                           FROM overlay_type")
  x <- dbGetQuery(con, select_overlay)
  Encoding(x$overlay_type_name) <- "UTF-8"
  return(x)
}


get_distinct_names_of_qa_operations <- function(con, type = NaN){
  if(type == "QA 1"){
    x<- dbGetQuery(con, "SELECT DISTINCT operation_order , operation_name_particular, operation_code_char 
                        FROM list_of_operations
                        WHERE list_of_operations.operation_id NOT BETWEEN 46 AND 103")
    Encoding(x$operation_name_particular) <- "UTF-8"
    x <- x[order(x$operation_order),]
    return(x)
  } else if(type == "QA 2"){
    x<- dbGetQuery(con, "SELECT DISTINCT operation_order , operation_name_particular, operation_code_char  
                        FROM list_of_operations
                   WHERE list_of_operations.operation_id BETWEEN 50 AND 103")
    Encoding(x$operation_name_particular) <- "UTF-8"
    x <- x[order(x$operation_order),]
    as.integer(x$operation_order)
    return(x)
  }else{
    return(NaN)
  }
}


get_qa_operations_for_detail <-
  function(con, valve_name, qa_type_name, tempr_name, tempr_oper_name, pressure_name, detail_name, material_sep_id){
    # get valve id
    valve_id <- get_valve_input_info(con, valve_name, type = "id")
    # get qa_type id
    qa_type_id <- get_qa_input_info(con, qa_type_name, type = "id")
    # get tempr_id
    tempr_id <- get_tempr_input_info(con, tempr_name, type = "id")
    # get tempr_oper_id
    tempr_oper_id <- get_tempr_oper_input_info(con, tempr_oper_name, type = "id")
    # get pressure_id
    pressure_id <- get_pressure_input_info(con, pressure_name, type = "id")
    # get detail id
    detail_id <- get_detail_input_info(con, detail_name, type = "id")
    if(material_sep_id == 9){
      return(NaN)
    }
    # шпильки
    if(detail_id == 5 || detail_id == 31){
      material_sep_id <- 6
    }else if(detail_id == 6 || detail_id == 32){
      material_sep_id <- 7
    }else{
      material_sep_id <- material_sep_id
    }
      select_operations <- paste0("SELECT 
  list_of_operations.operation_order, 
                                  list_of_operations.operation_name_4table
                                  FROM 
                                  public.valve, 
                                  public.valve_qa_type, 
                                  public.pressure, 
                                  public.tempr_operation, 
                                  public.tempr, 
                                  public.operations_qa_dependency, 
                                  public.material_type_separate, 
                                  public.list_of_operations, 
                                  public.detail, 
                                  public.ferrite_phase, 
                                  public.hydro, 
                                  public.radiography, 
                                  public.valve_to_bellow, 
                                  public.valve_type, 
                                  public.valve_bellow_type, 
                                  public.valve_id_to_socet, 
                                  public.detail_to_hydro, 
                                  public.detail_to_radiography, 
                                  public.ferrite_phase_to_detail
                                  WHERE 
                                  valve.valve_id = valve_to_bellow.valve_id AND
                                  valve.valve_id = valve_id_to_socet.valve_id AND
                                  valve_qa_type.valve_qa_type_id = operations_qa_dependency.valve_qa_type_id AND
                                  pressure.pressure_id = operations_qa_dependency.pressure_id AND
                                  tempr_operation.tempr_operation_id = operations_qa_dependency.tempr_operation_id AND
                                  tempr.tempr_id = operations_qa_dependency.tempr_id AND
                                  material_type_separate.material_type_separete_id = operations_qa_dependency.material_type_separete_id AND
                                  list_of_operations.operation_id = operations_qa_dependency.operation_id AND
                                  detail.detail_id = ferrite_phase_to_detail.detail_id AND
                                  ferrite_phase.fp_id = operations_qa_dependency.fp_id AND
                                  hydro.hydro_id = operations_qa_dependency.hydro_id AND
                                  radiography.rad_id = operations_qa_dependency.rad_id AND
                                  valve_to_bellow.valve_bellow_id = valve_bellow_type.valve_bellow_id AND
                                  valve_type.valve_type_by_socet = operations_qa_dependency.valve_type_by_socet AND
                                  valve_bellow_type.valve_bellow_id = operations_qa_dependency.valve_bellow_id AND
                                  valve_id_to_socet.valve_type_by_socet = valve_type.valve_type_by_socet AND
                                  detail_to_hydro.hydro_id = hydro.hydro_id AND
                                  detail_to_hydro.detail_id = detail.detail_id AND
                                  detail_to_radiography.rad_id = radiography.rad_id AND
                                  detail_to_radiography.detail_id = detail.detail_id AND
                                  ferrite_phase_to_detail.fp_id = ferrite_phase.fp_id AND
                                  valve.valve_id = ",valve_id," AND 
                                  valve_qa_type.valve_qa_type_id = ",qa_type_id ,"AND 
                                  detail.detail_id = ",detail_id ,"AND 
                                  material_type_separate.material_type_separete_id =  ",material_sep_id ,"AND 
                                  tempr.tempr_id = ",tempr_id ,"AND 
                                  tempr_operation.tempr_operation_id = ",tempr_oper_id," AND 
                                  list_of_operations.operation_id NOT BETWEEN 50 AND 103 AND
                                  pressure.pressure_id =", pressure_id,";")
    x <- dbGetQuery(con, select_operations)
    Encoding(x$operation_name_4table) <- "UTF-8"
    # Encoding(x$operation_name_particular) <- "UTF-8"
    x <- x[order(x$operation_order),]
    return(x)
}


get_welding_and_overlay_detail_list <- function(con, valve_name){

  select_welding_det <- paste0("SELECT
                   detail_for_con.detail_4con_name,
                  detail_for_con.detail_4con_id,
                  detail_for_con.detail_name_rus,
                  detail_for_con.detail_name_rus_welding,
                  detail_for_con.welding_type,
                  detail_for_con.number_of_welds
                  FROM 
                   public.valve, 
                   public.detail_4con_to_valve, 
                   public.detail_for_con
                   WHERE 
                   valve.valve_id = detail_4con_to_valve.valve_id AND
                   detail_4con_to_valve.detail_4con_id = detail_for_con.detail_4con_id AND
                   valve.valve_name = '", valve_name, "';
                   ")
  x <- dbGetQuery(con, select_welding_det)
  if(nrow(x) !=0)
  {
    Encoding(x$detail_4con_name) <- "UTF-8"
    Encoding(x$detail_name_rus) <- "UTF-8"
    Encoding(x$detail_name_rus_welding) <- "UTF-8"
    return(x)
  }else{
    return(NaN)
  }
}


get_conncetion_type_info <- function(con, detail_4con_name, type = NaN){
  if(type == "id"){
    select <- paste0("SELECT 
                              connection_type.con_type_id
                              FROM 
                              public.detail_for_con, 
                              public.detail_to_con_type, 
                              public.connection_type
                              WHERE 
                              detail_for_con.detail_4con_id = detail_to_con_type.detail_4con_id AND
                              detail_to_con_type.con_type_id = connection_type.con_type_id AND
                              detail_4con_name = '", detail_4con_name,"';
                              ")
    con_id_frame <- dbGetQuery(con,select)
    con_id <- con_id_frame$con_type_id[1]
    return(con_id)
  # } else if(type == "input type"){
  #   select <- paste0("
  #                    ")
  #   con_id_frame <- dbGetQuery(con,select)
  #   con_id <- con_id_frame$con_type_id[1]
  #   return(con_id)
  }else {
    return(NaN)
  }
}


get_qa2_operations_for_detail <- function(con, qa_type_name, tempr_name, connection_type_id, material_type_separate_id, detail_4con_name_current) {
  select <- paste0("SELECT 
                   list_of_operations.operation_name_4table, 
                   list_of_operations.operation_order
                   FROM 
                   public.operations_secondqa_dependency, 
                   public.valve_qa_type, 
                   public.tempr, 
                   public.connection_type, 
                   public.list_of_operations, 
                   public.material_type_separate, 
                   public.radiographic_for_welding, 
                   public.control_with_helium, 
                   public.detail_for_con, 
                   public.detail_4con_to_helium_control, 
                   public.detail_4con_to_radiography
                   WHERE 
                   operations_secondqa_dependency.operation_id = list_of_operations.operation_id AND
                   valve_qa_type.valve_qa_type_id = operations_secondqa_dependency.valve_qa_type_id AND
                   tempr.tempr_id = operations_secondqa_dependency.tempr_id AND
                   connection_type.con_type_id = operations_secondqa_dependency.con_type_id AND
                   material_type_separate.material_type_separete_id = operations_secondqa_dependency.material_type_separete_id AND
                   radiographic_for_welding.r4w_id = operations_secondqa_dependency.r4w_id AND
                   control_with_helium.control_helium_id = operations_secondqa_dependency.control_helium_id AND
                   detail_for_con.detail_4con_id = detail_4con_to_radiography.detail_4con_id AND
                   detail_for_con.detail_4con_id = detail_4con_to_helium_control.detail_4con_id AND
                   detail_4con_to_helium_control.control_helium_id = control_with_helium.control_helium_id AND
                   detail_4con_to_radiography.r4w_id = radiographic_for_welding.r4w_id AND
                   valve_qa_type.valve_qa_type_name ='",qa_type_name ,"'AND 
                   tempr.tempr_value_more_than_100 ='", tempr_name,"'AND 
                   connection_type.con_type_id =",connection_type_id," AND 
                   material_type_separate.material_type_separete_id =", material_type_separate_id," AND 
                   detail_for_con.detail_4con_name ='",detail_4con_name_current,"';
                   ")
 
  x <- dbGetQuery(con, select)
  # Encoding(x$operation_name_general) <- "UTF-8"
  # Encoding(x$operation_name_particular) <- "UTF-8"
  Encoding(x$operation_name_4table) <- "UTF-8"
  # Encoding(x$operation_name_4table_definition) <- "UTF-8"
  return(x)
  }


con <- okan_db_connect()

valve_list <- get_valve_list(con)
qa_type_list <- get_qa_type_list(con)
tempr_list <- get_tempr_list(con)
tempr_oper_list <- get_tempr_oper_list(con)
pressure_list <- get_pressure_list(con)
dn_value_list <- get_dn_list(con)
control_type_list <- get_control_type_list(con)
# ct_name <- control_type_list$control_type_def[1]
# # # con, vavle_type,qa_type, tempr, tempr_oper, pressure, detail, material
# qa_type <- qa_type_list$valve_qa_type_name[1]
# tempr <- tempr_list$tempr_value_more_than_100[1]
# tempr_oper <- tempr_oper_list$tempr_oper_value_more_than_20[1]
# pressure <- pressure_list$pressure_type[1]
# #
# valve_name <- valve_list$valve_name[14]
# det2_list <- get_welding_and_overlay_detail_list(con, valve_name)
# detail_list <- get_detial_list(con, valve_name)
# detail = detail_list$detail_name_rus[5]
# material_list <- get_material_list(con,detail,valve_name)
# material <- material_list$material_name[1]
# # # get_qa_operations_for_detail(con, vavle_type,qa_type, tempr, tempr_oper, pressure, detail, material)
# # qa_oper_list <- get_qa_operations_for_detail(con, valve_name, qa_type, tempr, tempr_oper, pressure, detail, material)
# # 
okan_db_disconnect(con)

#_________________________________________________________________________________________________________________________________________________
### Shiny UI ####
#_________________________________________________________________________________________________________________________________________________
ui <- dashboardPage(
  
  dashboardHeader(title = "OKANVAL web-UI demo",
  # Notification menu
  dropdownMenu(type = "messages",
               notificationItem(
                 text = "mailto: romas@okan.su",
                 #    ИНФОРМАЦИЯ ПО ИСПОЛЬЗОВАНИЮ OKANVAL /n
                 #      * 1 вкладка - выбор клапана и исходных данных
                 #      * 2 вкладка - выбор материалов деталей клапана и наплавки
                 # ОБРАТИТЕ ВНИМАНиЕ, ЧТО ПОСЛЕ ВЫБОРА КЛАПАНА ОБЯЗАТЕЛЬНО НУЖНО ЗАЙТИ ВО ВТОРУЮ ВКЛАДКУ
                 #      * 3 вкладка - ТБ 1
                 #      * 4 вкладка - ТЮ 2",
                 icon("info-circle")
               ))),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(

      menuItem("Исходные данные", tabName = "init_data", icon = icon("home"),
               badgeLabel = "1", badgeColor = "light-blue"),
      menuItem("Материалы деталей", tabName = "det_n_mat", icon = icon("list"),
               badgeLabel = "2", badgeColor = "light-blue"),
      menuItem("ТБ", tabName = "test", icon = icon("table"),
               badgeLabel = "3", badgeColor = "light-blue")
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "init_data",
                fluidPage(
                  theme  = "custom.css",
                  box(width = 8, title = h3("Тип клапана"), background = "light-blue",
                           selectInput("select_valve", label = NULL, 
                                       choices = valve_list$valve_name, 
                                       selected = 1,
                                       width = "100%")
                           ),
                  box(width =4, background = "light-blue",
                         textInput("rv_drawing_number", "Введите обозначение чертежа деталей ",
                                   value = "RV-YYYYYY", width = "80%"),
                         h5("Обозначение чертежа деталей:"),
                         verbatimTextOutput("rv_draw_numb_disp")
                  ),
                  box(width = 12, background = "light-blue",
                      column(4,
                         selectInput("select_qa_type", label = h5("Класс безопасности"), 
                                     choices = qa_type_list$valve_qa_type_name, 
                                     selected = 1,
                                     width = "80%")),
                  column(4,
                         selectInput("dn_value", label = h5("Значение DN клапана"),
                                     choices = dn_value_list$dn_value,
                                     selected = 1,
                                     width = "80%")),
                  column(4,
                         selectInput("control_type", label = h5("Управляющий орган"),
                                     choices = control_type_list$control_type_def,
                                     selected = 1,
                                     width = "80%")),
                  column(4,
                    selectInput("select_tempr", label = h5("Т-ра рабочей среды выше 100?"), 
                                choices = tempr_list$tempr_value_more_than_100, 
                                selected = 1,
                                width = "80%")),
                  column(4,
                    selectInput("select_tempr_oper", label = h5("Внешняя Т-ра выше 20?"), 
                                choices = tempr_oper_list$tempr_oper_value_more_than_20, 
                                selected = 1,
                                width = "100%")),
                  column(4,
                    htmlOutput("dynamic_select_pressure"))
                  )
                )
              
      ),
      # Second tab content
      tabItem(tabName = "det_n_mat",
              box(width = 12,title = h3("Список деталей клапана"), background = "light-blue",
                  h4("Выберите материалы для каждой детали"),
                  htmlOutput("details_and_materials")
              ),
              box(width = 12, background = "light-blue",
                  h4("Выберите материал наплавки для деталей(при наличии)"),
                  htmlOutput("details_and_overlays")
              )
      ),
      # Third tab content
      tabItem(tabName = "test",
              navbarPage("Составление таблиц ТБ",theme  = "custom.css",
                tabPanel("ТБ 1",
                         box(width = 12,
                             uiOutput("qa1_header"),
                             tags$head(
                               tags$style("#qa1_header{font-size: 10px;
                                                  }"
                                                  )
                                       ),
                             htmlOutput("qa_table"),
                             htmlOutput("text"),
                             downloadButton('downloadData', 'Скачать в *.csv'),
                             downloadButton('downloadDataDocx','Скачать в *.docx')
                         )
                ),
                tabPanel("ТБ 2",
                         box(width = 12,
                             useShinyjs(),
                             div(
                               id = "main",
                               uiOutput("qa2_header"),
                               tags$head(
                                 tags$style("#qa2_header{font-size: 10px;
                                                  }"
                                )
                               ),
                               htmlOutput("qa_table2"),
                               htmlOutput("text_qa2"),
                               downloadButton('download_qa2', 'Скачать в *.csv'),
                               downloadButton('downloadDataDocx_qa2','Скачать в *.docx')
                              )

                           )
                         )
                )
              )
      )
    )
  )

#_________________________________________________________________________________________________________________________________________________
### Shiny Server ####
#_________________________________________________________________________________________________________________________________________________
server <- function(input, output, session) {
  con <- okan_db_connect()
  
  
  reactive_get_oper_table <- reactive({
    
    dataframe_to_be_retuned <- get_distinct_names_of_qa_operations(con,type = "QA 1")
    qa_type <- input$select_qa_type
    tempr <- input$select_tempr
    tempr_oper <- input$select_tempr_oper
    pressure <- input$select_pressure
    valve_name <- input$select_valve
    detail_list <- get_detial_list(con, input$select_valve)
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
    dn<- dn$dn_4_code[1]
    valve_type <- get_valve_input_info(con, input$select_valve, type = "type")
    material <- input$material_1
    material_type <- get_material_input_info(con, input$material_1, type = "body material type")
    qa_type <- get_qa_input_info(con, input$select_qa_type, type = "qa type name")
    control_type <- get_valve_input_info(con, input$select_valve, type = "control_type")
    control_valve_type <- get_control_type_info(con, input$control_type, type = "4table")
    x <- paste0("OK.", dn, ".A",  valve_type, ".", material_type, qa_type, control_type, control_valve_type)
    return(x)
  })
  
  
  reactive_get_header_of_qa_table <- reactive({
    code <- reactive_get_valve_code()
    
    x <- paste0("Таблица контроля качества основных материалов изделия ", get_valve_input_info(con, input$select_valve, type = "type_def")
                , ", номер чертежа ", code, " СБ, классификационное обозначение ", input$select_qa_type, " по НП-068-05")
    return(x)
  })
  
  
  reactive_get_header_of_qa2_table <- reactive({
    code <- reactive_get_valve_code()
    
    x <- paste0("Таблица контроля качества сварных швов изделия ", get_valve_input_info(con, input$select_valve, type = "type_def")
                , ", номер чертежа ", code, " СБ, классификационное обозначение ", input$select_qa_type, " по НП-068-05")
    return(x)
  })
  
  
  reactive_get_rv_drawing_number <- reactive({
    dynamic_part <- input$rv_drawing_number
    x <- dynamic_part
    return(x)
  })
  
  
  reactiive_get_welding_and_overaly_table <- reactive({
    qa_type_name <- input$select_qa_type
    qa_type_welding <- get_qa_input_info(con, qa_type_name, type = "qa welding")
    tempr_name <- input$select_tempr
    valve_name <- input$select_valve
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
      detail_list2 <- get_overlay_detail_list(con, input$select_valve)
      
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
  
  output$qa1_header <- renderUI({
    str <- reactive_get_header_of_qa_table()
    Encoding(str) <- "UTF-8"
      headerPanel(h4(str))
  })
  
  output$qa2_header <- renderUI({
    headerPanel(h4( reactive_get_header_of_qa2_table(), style = "font-family: 'arial',"))
  })
  

  output$dynamic_select_pressure <-
    renderUI({
      current_qa_type <- input$select_qa_type
      if(current_qa_type == "2ВIIIс" || current_qa_type == "3СIIIс"){
        selectInput("select_pressure", label = h5("Класс давления по ANSI"),
                    choices = pressure_list$pressure_type[2],
                    selected = 1,
                    width = "80%")
      }else {
        selectInput("select_pressure", label = h5("Класс давления по ANSI"),
                    choices = pressure_list$pressure_type,
                    selected = 1,
                    width = "80%")
      }
      
    })
  
  output$details_and_materials <- 
    renderUI({
      detail_list <- get_detial_list(con, input$select_valve)
      lapply(1:length(detail_list$detail_name_rus), function(i) {
        detail_current = detail_list$detail_name_rus[i]
        Encoding(detail_current) <- "UTF-8"
        material_4_detail <- get_material_list(con, detail_current, input$select_valve)
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
      detail_list2 <- get_overlay_detail_list(con, input$select_valve)
      if(is.data.frame(detail_list2)){
        lapply(1:length(detail_list2$detail_4con_name), function(i) {
          detail_current = detail_list2$detail_name_rus[i]
          Encoding(detail_current) <- "UTF-8"
          material_4_detail <- get_overlay_list(con)
          print((paste0("overlay_",i)))
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
      x<-reactive_get_oper_table()
      # renderTable(reactive_get_oper_table())
      gvisTable(reactive_get_oper_table(), options=list(frozenColumns = 2, page = 'enable'))
    })
  
  output$qa_table2 <-
    renderGvis({
      gvisTable(reactiive_get_welding_and_overaly_table(), options=list(frozenColumns = 2, allowHtml = TRUE, showRowNumber = FALSE
                                                                        # cssClassNames = "{headerRow: 'myTableHeadrow'}", alternatingRowStyle = FALSE
      ))
    })
  
  output$text <-
    renderText({
      HTML(paste0(
        "<p><b>Обозначения:</b></p>
        <p>РГК  - радиографический контроль;</p>
        <p>УЗК  - ультразвуковой контроль;</p>
        <p>МПД  - магнитопорошковый контроль;</p>
        <p>+   - контроль производится;</p>
        <p>-   - контроль не производится;</p>
        <p>+c  - результаты испытаний подтверждаются сертификатом.</p>",
        reactive_get_definition_of_designations()),
        "<p> </p>"
      )
    })
  
  output$text_qa2 <-
    renderText({
      HTML(paste0(
        "<p><b>Обозначения:</b></p>
        <p>ВК   - входной контроль;</p>
        <p>ВиК  - визуальный и измерительный контроль;</p>
        <p>РГК  - радиографический контроль;</p>
        <p>УЗК  - ультразвуковой контроль;</p>
        <p>МПД  - магнитопорошковый контроль;</p>
        <p>+   - контроль производится;</p>
        <p>-   - контроль не производится;</p>
        <p>+c  - результаты испытаний подтверждаются сертификатом;</p>",
        reactive_get_definition_of_designations_for_qa2()),
        "<p> </p>"
      )
      
    })

  output$download_qa2 <- downloadHandler(
    filename = function() {
      paste0('QA2_table_', Sys.Date(), '.csv')
    },
    content = function(file) {
      data <- reactiive_get_welding_and_overaly_table()
      header <- paste0(reactive_get_header_of_qa2_table(),"\n")
      bottom <- paste0(
        "Обозначения:
ВК   - входной контроль;
ВиК  - визуальный и измерительный контроль;
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
 +   - контроль производится;
 +c  - результаты испытаний подтверждаются сертификатом;",
        "\n",
        reactive_get_definition_of_designations_for_qa2_file())
        
      cat(header, file=file, append = TRUE, sep =";" )
      write.table(data, file=file, append=TRUE, sep=';', row.names = FALSE, quote = TRUE)
      cat(bottom, file=file, append = TRUE, sep =";" )
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('QA_table_', Sys.Date(), '.csv')
    },
    content = function(file) {
      data <- reactive_get_oper_table()
      header <- paste0(reactive_get_header_of_qa_table(),"\n")
      bottom <- paste0(
        "Обозначения:
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
 +   - контроль производится;
 -   - контроль не производится;
 +c  - результаты испытаний подтверждаются сертификатом;",
        "\n",
        reactive_get_definition_of_designations_for_file())
      cat(header, file=file, append = TRUE, sep =";" )
      write.table(data, file=file, append=TRUE, sep=';', row.names = FALSE)
      cat(bottom, file=file, append = TRUE, sep =";" )
    }
  )
  
  output$downloadDataDocx <- downloadHandler(
    filename = function() {
      paste0('QA_table_', Sys.Date(), '.docx')
    },
    content = function(file) {
      data <- reactive_get_oper_table()
      parprop <- parProperties(padding = 2)
      cellprop <- cellProperties( text.direction = "btlr" )
      data_header <- colnames(data)
      datadata <- FlexTable(data, header.columns = FALSE) %>% 
        addHeaderRow( value = c("", "","","Наименование операции"), colspan = c( 1,1,1, 24),
                      par.properties = parprop, text.properties = textNormal() ) %>% 
        addHeaderRow( value=data_header, cell.properties = cellprop, text.properties = textNormal() )
      header <- paste0(reactive_get_header_of_qa_table(),"\n")
      bottom <- paste0(
        "Обозначения:
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
+   - контроль производится;
-   - контроль не производится;
+c  - результаты испытаний подтверждаются сертификатом;",
"\n",
        reactive_get_definition_of_designations_for_file(),
"

Настоящую таблицу рассматривать совместно с ОСТ 108.004.10 и комплектом конструкторской документации."
        )
      doc <- docx(  ) %>% addParagraph(header) %>% addFlexTable( datadata ) %>% addParagraph(bottom) 
      writeDoc(doc, file=file )
    }
  )
  output$downloadDataDocx_qa2 <- downloadHandler(
    filename = function() {
      paste0('QA2_table_', Sys.Date(), '.docx')
    },
    content = function(file) {
      data <- reactiive_get_welding_and_overaly_table()
      header <- paste0(reactive_get_header_of_qa2_table(),"\n")
      bottom <- paste0(
        "Обозначения:
ВК   - входной контроль;
ВиК  - визуальный и измерительный контроль;
РГК  - радиографический контроль;
УЗК  - ультразвуковой контроль;
МПД  - магнитопорошковый контроль;
 +   - контроль производится;
 +c  - результаты испытаний подтверждаются сертификатом;",
        "\n",
        reactive_get_definition_of_designations_for_qa2_file(),
"

Настоящую таблицу рассматривать совместно с ОСТ 108.004.10 и комплектом конструкторской документации."
        )
      cellprop <- cellProperties( text.direction = "btlr" )
      parprop <- parProperties(padding = 2)
      data_header <- colnames(data)
      datadata <- FlexTable(data, header.columns = FALSE) %>% 
        addHeaderRow( value = c("", "","","","","","Наименование операции"), colspan = c( 1,1,1,1,1,1, 19),
                      par.properties = parprop, text.properties = textNormal() ) %>% 
        addHeaderRow( value=data_header, cell.properties = cellprop, text.properties = textNormal() )
      doc <- docx(  ) %>% addParagraph(header) %>% addFlexTable( datadata ) %>% addParagraph(bottom) 
      writeDoc(doc, file=file )
    }
  )
  # observe({
  #   toggleState("main", condition = input$select_valve)
  #   
  #   # hide("main")
  # })
}
#_________________________________________________________________________________________________________________________________________________
### Shiny App ####
#_________________________________________________________________________________________________________________________________________________

shinyApp(ui, server)

# options(shiny.port = 7775)
# options(shiny.host = "192.168.1.118")

# options(shiny.port = 6545)
# options(shiny.host = "192.168.1.59")

