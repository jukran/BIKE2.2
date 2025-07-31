library(shiny)


## Switch upload data tabs:----
observeEvent(
  input$file_conct, 
  updateTabsetPanel(inputId = "uploads", selected = "file_conctTab")
)
observeEvent(
  input$file_consm, 
  updateTabsetPanel(inputId = "uploads", selected = "file_consmTab")
)
observeEvent(
  input$file_occ, 
  updateTabsetPanel(inputId = "uploads", selected = "file_occTab")
)
observeEvent(
  input$file_prev, 
  updateTabsetPanel(inputId = "uploads", selected = "file_prevTab")
)



## Food type and hazards selections (from the uploaded file with occurrence):----
observe({
  #figures:
  ##plot1 concentrations:
  updateSelectInput(session, "thefoodnames1", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())])[1])
  updateSelectInput(session, "thehazardnames1", choices = ocdata()$hazardnames, selected = ocdata()$hazardnames[1])
  ##plot2 consumptions:
  updateSelectInput(session, "thefoodnames2", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())])[1])
  ##plot6 serving:
  updateCheckboxGroupInput(session, "thefoodnames21", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())]))
  ##plot7 serving:
  updateCheckboxGroupInput(session, "thefoodnames22", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())]))
  ##plot3 exposures:
  updateSelectInput(session, "thefoodnames3", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())])[1])
  updateSelectInput(session, "thehazardnames3", choices = ocdata()$hazardnames, selected = ocdata()$hazardnames[1])
  ##plot4 quantiles:
  updateCheckboxGroupInput(session, "thefoodnames4", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())]))
  updateSelectInput(session, "thehazardnames4", choices = ocdata()$hazardnames, selected = ocdata()$hazardnames[1])
  ##plot51&52 MCMC:
  updateSelectInput(session, "thefoodnames5", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())])[1])
  updateSelectInput(session, "thefoodnames52", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())])[1])
  
  updateSelectInput(session, "thehazardnames5", choices = ocdata()$hazardnames, selected = ocdata()$hazardnames[1])
  
  #table1:
  updateCheckboxGroupInput(session, "thefoodnames_t", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())])[1])
  updateCheckboxGroupInput(session, "thehazardnames_t", choices = ocdata()$hazardnames, selected = ocdata()$hazardnames[1])
  #table2:
  updateCheckboxGroupInput(session, "thefoodnames_t2", choices = colnames(ocdata()[,4:ncol(ocdata())]), selected = colnames(ocdata()[,4:ncol(ocdata())])[1])
  updateCheckboxGroupInput(session, "thehazardnames_t2", choices = ocdata()$hazardnames, selected = ocdata()$hazardnames[1])
  
  #report download:
  updateSelectInput(session, "thehazardnames_dl", choices = ocdata()$hazardnames, selected = ocdata()$hazardnames[1])
  
})



## Adjustment factors, fill the matrix:----
observe({
  req(ocdata())
  updateMatrixInput(session, "factor", value = matrix(
    1,
    length(names(ocdata()[,4:dim(ocdata())[2]])),
    length(ocdata()$hazardnames),
    dimnames = list(substr(names(ocdata()[,4:dim(ocdata())[2]]), 1, 5), substr(ocdata()$hazardnames, 1, 5))
  ))
  updateMatrixInput(session, "pfactor", value = matrix(
    1,
    length(names(ocdata()[,4:dim(ocdata())[2]])),
    length(ocdata()$hazardnames),
    dimnames = list(substr(names(ocdata()[,4:dim(ocdata())[2]]), 1, 5), substr(ocdata()$hazardnames, 1, 5))
  ))
})



## Update ui after link/button at home-page---- 
observeEvent(input$go_model,{
  updateNavbarPage(inputId = "bike_tabs", selected = "run_model_tab")
})

observeEvent(input$prep_files,{
  updateNavbarPage(inputId = "bike_tabs", selected = "about_model_tab")
})


## Update ui after run----

output$plot1text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$plot2text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$plot3text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$plot4text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$plot4gen <- renderText({
  paste0(tags$b("Select total exposure quantile and generate plot!"))
})
output$plot5text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$plot52text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$plot6text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$plot7text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$table1text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})
output$table2text <- renderText({
  paste0(tags$b("Select model settings and run the simulation"))
})

output$table1text_generate <- renderText({
  paste0(tags$b("Generate new table!"))
})
output$table2text_generate <- renderText({
  paste0(tags$b("Generate new table!"))
})

output$reportDLtext <- renderText({
  paste(tags$h4(tags$b("Some changes within model settings have been done. Requires new simulation."), style ="color:#D0006F"))
})

# if any of the following inputs is changed, need new model simulation. Old simulation plots are hidden and text messages appear:
observe({
  input$modelchoice
  input$priorchoice
  input$Iterations
  input$modelchoice2
  input$modelchoice3
  input$modelchoice4
  input$modelchoice5
  
  input$file_occ
  input$file_conct
  input$file_consm
  input$file_prev
  
  
  show(id = "plot1text")
  show(id = "plot2text")
  show(id = "plot3text")
  show(id = "plot4text")
  show(id = "plot5text")
  show(id = "plot52text")
  show(id = "plot6text")
  show(id = "plot7text")
  show(id = "table1text")
  show(id = "table2text")
  
  show(id = "reportDLtext")
  hide(id = "report_mcmc")
  hide(id = "report_view_dl")
  hide(id = "report_hazard_dl")
  
  hide(id = "distPlot1")
  hide(id = "distPlot2")
  hide(id = "distPlot3")
  hide(id = "distPlot4")
  hide(id = "distPlot5")
  hide(id = "distPlot52")
  hide(id = "distPlot6")
  hide(id = "distPlot7")
  hide(id = "values")
  hide(id = "pvalues")
  
})

# after run button is pressed:
observe({
  input$run
  hide(id = "plot1text")
  hide(id = "plot2text")
  hide(id = "plot3text")
  hide(id = "plot4text")
  hide(id = "plot5text")
  hide(id = "plot52text")
  hide(id = "plot6text")
  hide(id = "plot7text")
  
  hide(id = "table1text")
  hide(id = "table2text")
  show(id = "values")
  show(id = "pvalues")
  
  hide(id = "reportDLtext")
  show(id = "report_mcmc")
  show(id = "report_view_dl")
  show(id = "report_hazard_dl")
  
  show(id = "distPlot1")
  show(id = "distPlot2")
  show(id = "distPlot3")
  show(id = "distPlot4")
  show(id = "distPlot5")
  show(id = "distPlot52")
  show(id = "distPlot6")
  show(id = "distPlot7")
  
})



observeEvent(input$run, {
  
  updateTabsetPanel(inputId = "selectresults", selected = "Concentrations")
  updateTabsetPanel(inputId = "consumptions", selected = "Consumptions")
  updateTabsetPanel(inputId = "exposures", selected = "Exposures")
  updateTabsetPanel(inputId = "diagchoice", selected = "Concentration parameters")
  updateSelectInput(inputId = "selectQ", selected = "None")
  
  updateTabsetPanel(inputId = "run_sim", selected = "Concentrations")
  
  
  show(id = "report_dl")
  show(id = "plot4gen")
  show(id = "table1text_generate")
  show(id = "table2text_generate")
  
  
})

observeEvent(input$generateP4, {
  hide(id = "plot4gen")
})

observeEvent(input$generateTposter, {
  hide(id = "table1text_generate")
})

observeEvent(input$generateTlimit, {
  hide(id = "table2text_generate")
})



## Notifications for missing selections (food types and hazards):----
observe({
  
  #Plot 4
  output$thefoodname_need4 <- renderText({
    if(is.null(input$thefoodnames4)){
      paste("Select at least one food type")}
  })
  
  #Plot 6
  output$thefoodname_need6 <- renderText({
    if(is.null(input$thefoodnames21)|length(input$thefoodnames21)< 2){
      paste("Select at least two food types")}
  })
  
  #Plot 7
  output$thefoodname_need7 <- renderText({
    if(is.null(input$thefoodnames22)|length(input$thefoodnames22)< 2){
      paste("Select at least two food types")}
  })
  
})



