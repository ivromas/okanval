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

#_________________________________________________________________________________________________________________________________________________
### Usful functions ####
#_________________________________________________________________________________________________________________________________________________
okan_db_connect <- function() {
  pw <- {
    "qwerty123"
  }
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "qa_db",
                   host = "okanval.okan.su", port = 6543,
                   user = "user_web_ui", password = pw)
  # con <- dbConnect(drv, dbname = "qa_db",
  #                  host = "localhost", port = 5432,
  #                  user = "postgres", password = pw)
  return(con)
}


okan_db_disconnect <- function(con){
  dbDisconnect(con)
}


get_user_info <- function(con, type = NULL, user_name = NULL, user_password = NULL) {
  if (type == "all") {
        x <- dbGetQuery(con,"SELECT user_id, user_name, user_password
                    FROM public.user_okan;")
    Encoding(x$user_name) <- "UTF-8"
    Encoding(x$user_password) <- "UTF-8"
    return(x)
  }else if (type == "check") {
    str <- paste0("SELECT user_id FROM user_okan WHERE user_name ='"
                  ,user_name,"'AND user_password ='",user_password, "';")
    x <- dbGetQuery(con,str)
    return(x)
  }

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
  if (nrow(x) != 0)
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
  if (type == "id") {
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



PASSWORD <- get_user_info(con,type="all")
Logged = FALSE;

valve_list <- get_valve_list(con)
qa_type_list <- get_qa_type_list(con)
tempr_list <- get_tempr_list(con)
tempr_oper_list <- get_tempr_oper_list(con)
pressure_list <- get_pressure_list(con)
dn_value_list <- get_dn_list(con)
control_type_list <- get_control_type_list(con)
okan_db_disconnect(con)
