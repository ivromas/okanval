#### Log in module ###
USER <- reactiveValues(Logged = Logged)

passwdInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    tags$input(id = inputId, type="password", value="")
    ,tags$script(src = "enter_button.js")
#     tags$script('
#     $(document).on("keydown", function (e) {
#                 Shiny.onInputChange("mydata", e.which);
# });
#                 ')
  )
}


output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      
      textInput("userName", "User Name:"),
      passwdInput("passwd", "Pass word:"),
      br(),
      actionButton("Login", "Log in")
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        Id.username <- which(PASSWORD$user_name == Username)
        Id.password <- which(PASSWORD$user_password == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          check <- get_user_info(con,type = "check",user_name = Username ,user_password = Password)
          
          if (is.data.frame(check) & nrow(check)>0) {
            USER$Logged <- TRUE
          } else  {
            "User name or password failed!"
            # str <- "User name or password failed!"
            # tags$div(
            #   HTML(paste0("<strong>",'<font face="Bedrock" size="4" color = "red">', str ,"</font>","</strong>"))
            # )
          } 
        } else  {
          "User name or password failed!"
          # str <- "User name or password failed!"
          # tags$div(
          #   HTML(paste0("<strong>",'<font face="Bedrock" size="4" color = "red">', str ,"</font>","</strong>"))
          # )
        }
      } 
    }
  }
})
