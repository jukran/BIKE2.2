# Desktop Shiny app for BIKE foodborne exposure model
# ------------------------------------------------------------
# You can run the application by clicking
# the 'Run App' button above.
#


# packages needed:
library(shiny)
library(R2OpenBUGS)
library(mvtnorm)
library(shinyMatrix)
library(shinythemes)
library(tidyverse)
library(shinyhelper)
library(waiter)
library(shinyjs)
library(DT)
library(data.table) # -> for the function fread() to read the uploaded files
library(rmarkdown)




source("helps.R")


# UI for application that runs BUGS model and draws results----
ui <- navbarPage(id ="bike_tabs",
                 strong("BIKE foodborne exposure model"),
                 theme = shinythemes::shinytheme("lumen"),
                 
                 tabPanel("Home", icon = icon("home"), # Home----
                          fluidPage(
                            fluidRow(
                              column(2),
                              column(8,
                                     tags$img(src = "GA_BIKE.PNG", width = "100%", height = "100%"),
                                     tags$hr(),
                                     tags$br(),
                                     wellPanel(align="center", style = "background-color: #EEEEEE;",
                                               h3(tags$b("What is BIKE app?"), style ="color:#004F71"),tags$br(),
                                               tags$p(tags$b("BIKE app"), "is a graphical user interface developed for running 
                      the BIKE model (a Bayesian dietary exposure assessment model for microbiological and chemical hazards) and inspecting the results."),
                                               tags$p("It is based on connected Bayesian 
                      hierarchical models, utilizing OpenBUGS and R in tandem. Chronic and acute exposures are estimated for chemical and microbiological 
                      hazards, respectively. Uncertainty and variability in exposures are visualized, 
                      and a few optional model structures are available."), 
                                               tags$p("BIKE app is open source and latest version is available from GitHub",tags$i(tags$a("(https://github.com/jukran)",
                                                                                                                        href = "https://github.com/jukran",
                                                                                                                        target =
                                                                                                                          "_blank")),
                                                      ". Simulated synthetic data resembling real occurrence and consumption data is 
                                       provided with the code as an example."),
                                               
                                               
                                               tags$br(),
                                               tags$br(),
                                               tags$p("Find more about BIKE model from:"),
                                               tags$p(
                                                 tags$b("Ranta J, Mikkelä A, Suomi J, Tuominen P. BIKE: Dietary Exposure Model for
                                               Foodborne Microbiological and Chemical Hazards. Foods. 2021; 10(11):2520."),
                                                 tags$a(
                                                   "https://doi.org/10.3390/foods10112520",
                                                   href = "https://www.mdpi.com/2304-8158/10/11/2520",
                                                   target =
                                                     "_blank"
                                                 )
                                               ),style = "padding: 45px;"
                                     ),
                                     tags$br(),
                                     tags$br(),
                                     wellPanel(align="center",style = "background-color: #EEEEEE;",
                                               h3(tags$b("How to use the app?"), style ="color:#004F71"),tags$br(),
                                               fluidRow(align="center",
                                                        column(4,
                                                               wellPanel(
                                                                 h4(tags$b("1. Upload data files"), style ="color:#D0006F"),tags$br(),
                                                                 tags$p(
                                                                   "To run BIKE, four separate files are needed. The files have to contain data for the concentrations, 
                                                    the consumptions, the occurrence, and the prevalence, respectively."),
                                                                 tags$p("The columns in the files should have", tags$b("specific names.")),
                                                                 tags$p("The names of the", tags$b("food types"), "and the", tags$b("hazards"), "should match in all data files." ),
                                                                 
                                                                 tags$p("The data should be uploaded as ",tags$b("csv"), "files using ",tags$b("point (.)"), "for decimal and ",tags$b("comma (,)"), " for field separation."),
                                                                 
                                                                 tags$p("The data should not contain any special characters (e.g., ä, ö, å, etc.)."),
                                                                 
                                                                 tags$p("Read more on how to prepare the files ", actionLink("prep_files", tags$b("here."))),
                                                                 style = "padding: 30px;"
                                                               )
                                                        ),
                                                        column(4,
                                                               wellPanel(
                                                                 h4(tags$b("2. Specify model settings and run simulations"), style ="color:#D0006F"),tags$br(),
                                                                 tags$p("BIKE provides an option to use model settings that are most suitable for the input data."),
                                                                 tags$p("These include ",
                                                                        tags$b("Consumption model, Correlation models, Priors for variances"), "and", tags$b("Number of MCMC iterations.")
                                                                 ),
                                                                 tags$p("Note that the time for the simulations to complete depends on the number of iterations selected. 
                                                                        It is recommended to start with small numbers, e.g., the default 4000."),
                                                                 tags$p("After the four files are uploaded and the model is set up, the simulations could be run."),
                                                                 
                                                                 style = "padding: 30px;"
                                                               )
                                                        ),
                                                        column(4,
                                                               wellPanel(
                                                                 h4(tags$b("3. Inspect the results"), style ="color:#D0006F"), tags$br(),
                                                                 tags$p("The results are visualized with", tags$b("figures"), " and ", tags$b("tables"), ", and their content could be 
                                                                        changed, e.g., food type, hazard, credible interval, etc."),
                                                                 tags$p("In addition, adjustment factors for both the concentration level 
                                                                        and the prevalence for each food-hazard combination could be assigned in the exposures section."),
                                                                 tags$p("New 2D simulation for the quantiles figure is running after the button 'Generate plot' is pressed. 
                                                                        "),
                                                                 tags$p("The posterior predictive distribution summaries table and the exposure limit analyses table
                                                                        generate after the button 'Generate table' is pressed."),
                                                                 style = "padding: 30px;"
                                                                 
                                                               ))
                                               ),
                                               fluidRow("The results could be downloaded as",tags$b("rds"), "and", tags$b("html"), "files. The ", tags$b("rds"), "file contains
                                                         the MCMC samples for all parameters. The ", tags$b("html"), "files contain
                                                         the input data, the settings used for the model, and the results as figures and tables.", style = "padding: 45px;")
                                     ),
                                     tags$br(),
                                     fluidRow(align="center",
                                              actionButton("go_model", "Get started", class = "btn-success")
                                     ),
                                     tags$br(),
                                     tags$hr()
                              ),
                              column(2)
                            )
                          )
                          
                 ),
                 tabPanel("Simulations", icon = icon("chart-line"), value = "run_model_tab",
                          tags$head(
                            tags$style(
                              HTML(
                                ".shiny-output-error-validation {color: #D0006F;font-weight: bold;}"
                              )
                            ),
                            tags$style(type = 'text/css', ".myclass1 {background-color: #EEEEEE;}")
                          ),
                          
                          # for hide/show functions:
                          useShinyjs(),
                          # for busy spinner:
                          autoWaiter(html=spin_loaders(id = 2, color = "#004F71", style = NULL),color = "#EEEEEE"),
                          
                          tabsetPanel(
                            type = "pills",
                            tabPanel("1. Upload data", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$style(type = "text/css", "a{color: #004F71;}"),
                                         width = 3,
                                         wellPanel(style = "background-color: #FFFFFF;",
                                                   help_file(),
                                                   h4(strong("Upload csv data files"), style = "color:#343841"),
                                                   tags$br(),
                                                   #Upload files----
                                                   #Upload file 1:
                                                   fileInput(
                                                     "file_conct",
                                                     label = strong("Concentrations"),
                                                     multiple = FALSE,
                                                     accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")
                                                   ),
                                                   #Upload file 2:
                                                   radioButtons(  
                                                     "datachoice",
                                                     label = h5(strong("Consumption data type:"), style = "color:#CEB888"),
                                                     choiceNames = list(tags$span("Food diary", style = "color: black"),
                                                                        tags$span("FFQ", style = "color: black")),
                                                     choiceValues = list("Food diary","FFQ"),
                                                     selected = "Food diary"
                                                   ),
                                                   
                                                   fileInput(
                                                     "file_consm",
                                                     strong("Consumptions"),
                                                     multiple = FALSE,
                                                     accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")
                                                   ),
                                                   #Upload file 3:
                                                   fileInput(
                                                     "file_occ",
                                                     strong("Occurrence"),
                                                     multiple = FALSE,
                                                     accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")
                                                   ),
                                                   #Upload file 4:
                                                   fileInput(
                                                     "file_prev",
                                                     strong("Prevalence"),
                                                     multiple = FALSE,
                                                     accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")
                                                   ),
                                                   tags$hr(),
                                                   strong("Example files:"),
                                                   tags$br(),
                                                   downloadLink("downloadConcenData", tags$i("DataConcentrations.csv")),
                                                   tags$br(),
                                                   downloadLink("downloadConsumData", tags$i("DataConsumptions.csv")), 
                                                   tags$br(),
                                                   downloadLink("downloadOccData", tags$i("DataOccurrence.csv")), 
                                                   tags$br(),
                                                   downloadLink("downloadPrevData", tags$i("DataPrevalence.csv")),
                                                   tags$br(),
                                                   downloadLink("downloadConcenDatafish", tags$i("DataConcentrations_fish.csv")),
                                                   tags$br(),  
                                                   downloadLink("downloadConsumDataFFQ", tags$i("DataConsumptions_fish_FFQ.csv")),
                                                   tags$br(),
                                                   downloadLink("downloadOccDatafish", tags$i("DataOccurrence_fish.csv")),
                                                   tags$br(),
                                                   downloadLink("downloadPrevDatafish", tags$i("DataPrevalence_fish.csv"))
                                         )
                                       ),
                                       mainPanel(
                                         width = 9,
                                         ## uploaded files tables----
                                         tabsetPanel(
                                           id = "uploads",
                                           type = "pills",
                                           tabPanel(
                                             title = "Concentrations data",
                                             value = "file_conctTab",
                                             tableOutput("contents_conct")
                                           ),
                                           tabPanel(
                                             title = "Consumptions data",
                                             value = "file_consmTab",
                                             tableOutput("contents_consm")
                                           ),
                                           tabPanel(
                                             title = "Occurrence data",
                                             value = "file_occTab",
                                             tableOutput("contents_occ")
                                           ),
                                           tabPanel(
                                             title = "Prevalence data",
                                             value = "file_prevTab",
                                             tableOutput("contents_prev")
                                           )
                                         )
                                       )
                                     )),
                            tabPanel("2. Run simulation", id = "run_sim",
                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$style(type = "text/css", "a{color: #004F71;}"),
                                         width = 3,
                                         wellPanel(
                                           style = "background-color: #004F71;",
                                           
                                           help_parameters(),
                                           
                                           # Model parameters----
                                           h4(strong("Model settings"), style = "color:#CEB888"),
                                           
                                           conditionalPanel(   ###
                                             ## Selection based on either food diary data or FFQ data
                                             condition = "input.datachoice == 'FFQ'",   ###
                                             h5(strong("Correlation model"), style ="color:#CEB888"),
                                             
                                             wellPanel( 
                                               help_params_serving(),
                                               tags$br(),
                                               fluidRow(
                                                 column(5,
                                                 radioButtons(
                                                   "modelchoice4FFQ",
                                                   label = h5(("Correlated mean serving sizes"), style ="color:#343841"),
                                                   choices = c("No", "Yes"),
                                                   selected = "No"
                                                   ###choiceNames = list(tags$span("Correlation: no ", style = "color: #000000;"),
                                                  ###                    tags$span("Correlation: yes", style = "color: #000000;")),
                                                   ###choiceValues = list("No","Yes"),
                                                 )
                                                 )
                                               )
                                             ),
                                             
                                           ),  ### FFQ 
                                           
                                           
                                           conditionalPanel(   ###
                                             ## Selection based on either food diary data or FFQ data
                                             condition = "input.datachoice != 'FFQ'",   ###
                                             
                                             radioButtons(
                                               "modelchoice",
                                               label = h5(strong("Consumption model"), style = "color:#CEB888"),
                                               choiceNames = list(tags$span("Dependent days", style = "color: #EEEEEE;"),
                                                                  tags$span("Independent days", style = "color: #EEEEEE;")),
                                               choiceValues = list("Dependent days", "Independent days"),
                                               selected = "Independent days"
                                             ),
                                             conditionalPanel(
                                               # Selection that is related only to Consumption model "Independent days":
                                               condition = "input.modelchoice == 'Independent days'",
                                               wellPanel(
                                                 style = "border-color: #CEB888",
                                                 help_params_indep(),
                                                 radioButtons(
                                                   "modelchoice2",
                                                   label = "Between-user variability in consumption frequencies",
                                                   choices = c("No", "Yes")
                                                 ),
                                                 ##
                                                 conditionalPanel(  ##
                                                   condition = "input.modelchoice2 == 'Yes'",  ##
                                                   radioButtons(
                                                     "modelchoice5",
                                                     label = "Correlation model of consumption frequencies",
                                                     choices = c("No", "Yes")
                                                   )
                                                   ##
                                                 ),   ##
                                               )
                                             ),
                                             h5(strong("Correlation models"), style ="color:#CEB888"),
                                             
                                             wellPanel( 
                                               help_params_serving(),
                                               tags$br(),
                                               fluidRow(
                                                 column(
                                                   5, 
                                                   radioButtons(
                                                     "modelchoice3",
                                                     label = h5(("Correlated serving sizes"), style = "color:#343841"),
                                                     choices = c("No", "Yes")
                                                   )
                                                 ),
                                                 column(
                                                   7, radioButtons(
                                                     "modelchoice4",
                                                     label = h5(("Correlated mean serving sizes"), style ="color:#343841"),
                                                     choices = c("No", "Yes")
                                                   )
                                                 ))
                                             ),
                                             
                                           ),  ### not FFQ
                                           
                                            
                                          
                                           radioButtons(
                                             "priorchoice",
                                             label = h5(strong("Priors for variances"), style ="color:#CEB888"),
                                             choiceNames = list(tags$span("Tau Gamma", style = "color: #EEEEEE;"),
                                                                tags$span("Sigma Uniform", style = "color: #EEEEEE;")),
                                             
                                             choiceValues = list("tau_gamma", "sigma_uniform")
                                             
                                           ),
                                           
                                           #help_iterations(),
                                           numericInput(
                                             "Iterations",
                                             h5(strong("Number of MCMC iterations"), style = "color:#CEB888"),
                                             min = 4000,
                                             max = 150000,
                                             value = 4000,
                                             step = 500
                                           ),
                                           tags$br(),
                                           # Run calculations:
                                           wellPanel(actionButton(
                                             inputId = "run",
                                             label = "RUN simulation",
                                             class = "btn-primary", 
                                             style="color: #fff; background-color: #D0006F; border-color: #D0006F"
                                             
                                           ),
                                           tags$br(),
                                           uiOutput("uploadFiles"), style = "background-color: #EEEEEE;")
                                         )
                                         
                                       ),
                                       # Results----
                                       mainPanel(width = 9,
                                                 
                                                 fluidRow(
                                                   column(
                                                     10,
                                                     tabsetPanel(
                                                       id = "selectresults",
                                                       type = "pills",
                                                       ## concentrations plot----
                                                       tabPanel(
                                                         "Concentrations",
                                                         tags$br(),
                                                         
                                                         textOutput("food_hazad_comb_na_1"),
                                                         hidden(htmlOutput("plot1text")),
                                                         plotOutput("distPlot1", width = "100%", height = "650px"),
                                                         
                                                         tags$br(),
                                                         htmlOutput("plot1_cap"),
                                                         tags$hr()
                                                       ),
                                                       ## consumptions plots----
                                                       tabPanel(
                                                         "Consumptions",
                                                         ### _consumption----
                                                         tabsetPanel(
                                                           id = "consumptions",
                                                           
                                                           tabPanel(
                                                             "Consumptions",
                                                             tags$br(),
                                                             hidden(htmlOutput("plot2text")),
                                                             plotOutput("distPlot2", width = "100%", height = "650px"),
                                                             
                                                             tags$br(),
                                                             htmlOutput("plot2_cap"),
                                                             tags$hr()
                                                           ),
                                                           
                                                           ### _serving correlation----
                                                           tabPanel(
                                                             "Correlations (servings)",
                                                             value = "serveCor",
                                                             tags$br(),
                                                             textOutput("thefoodname_need6"),
                                                             hidden(htmlOutput("plot6text")),
                                                             plotOutput("distPlot6", width = "100%", height = "650px"),
                                                             
                                                             tags$br(),
                                                             htmlOutput("plot6_cap"),
                                                             tags$hr()
                                                           ),
                                                           ### _mean serving correlation----
                                                           tabPanel(
                                                             "Correlations (mean servings)",
                                                             value = "mserveCor",
                                                             tags$br(),
                                                             textOutput("thefoodname_need7"),
                                                             hidden(htmlOutput("plot7text")),
                                                             plotOutput("distPlot7", width = "100%", height = "650px"),
                                                             
                                                             tags$br(),
                                                             htmlOutput("plot7_cap"),
                                                             tags$hr()
                                                           )
                                                           
                                                         )
                                                         # )
                                                       ),
                                                       ## exposures plots----
                                                       tabPanel(
                                                         "Exposures",
                                                         value = "exposureTab",
                                                         
                                                         fluidRow(### _exposures----
                                                                  tabsetPanel(
                                                                    id = "exposures",
                                                                    
                                                                    tabPanel(
                                                                      "Exposures",
                                                                      tags$br(),
                                                                      hidden(htmlOutput("plot3text")),
                                                                      plotOutput("distPlot3", width = "100%", height = "650px"),
                                                                      
                                                                      tags$br(),
                                                                      htmlOutput("plot3_cap")
                                                                    ),
                                                                    ### _quantiles----
                                                                    tabPanel(
                                                                      "Quantiles",
                                                                      tags$br(),
                                                                      actionButton("generateP4", "Generate plot", class = "btn-primary", 
                                                                                   style="color: #fff; background-color: #D0006F; border-color: #D0006F"),
                                                                      
                                                                      textOutput("thefoodname_need4"),
                                                                      hidden(htmlOutput("plot4text")),
                                                                      hidden(htmlOutput("plot4gen")),
                                                                      tags$style("#plot4gen{color: #D0006F;}"),
                                                                      tags$br(),
                                                                      plotOutput("distPlot4", width = "100%", height = "650px"),
                                                                      
                                                                      tags$br(),
                                                                      htmlOutput("plot4_cap"),
                                                                      tags$br()
                                                                    )
                                                                  )
                                                         ),
                                                         tags$hr(),
                                                         ## adjustment factors----
                                                         conditionalPanel(condition = "input.selectresults == 'exposureTab'",
                                                                          wellPanel(
                                                                            help_factors(),
                                                                            h4(strong("Adjustment factors"), style = "color:#CEB888"),
                                                                            fluidRow(column(
                                                                              6,
                                                                              h5(strong("Concentration factors"), style = "color: #004F71;"),
                                                                              
                                                                              tags$span(
                                                                                matrixInput(
                                                                                  "factor",
                                                                                  class = "numeric",
                                                                                  value = matrix(),
                                                                                  rows = list(names = TRUE),
                                                                                  cols = list(names = TRUE)
                                                                                )
                                                                              )
                                                                            ),
                                                                            column(
                                                                              6,
                                                                              h5(strong("Prevalence factors"), style = "color: #004F71;"),
                                                                              
                                                                              tags$span(
                                                                                matrixInput(
                                                                                  "pfactor",
                                                                                  class = "numeric",
                                                                                  value = matrix(),
                                                                                  rows = list(names = TRUE),
                                                                                  cols = list(names = TRUE)
                                                                                )
                                                                              )
                                                                            ))
                                                                          ))
                                                       ),
                                                       tabPanel(
                                                         "MCMC samples",
                                                         tabsetPanel(
                                                           id = "diagchoice",
                                                           ## MCMC diagnostics plot----
                                                           tabPanel(
                                                             "Concentration parameters",
                                                             tags$br(),
                                                             hidden(htmlOutput("plot5text")),
                                                             plotOutput("distPlot5", width = "100%", height = "650px"),
                                                             
                                                             tags$br(),
                                                             htmlOutput("plot51_cap"),
                                                             tags$hr()
                                                           ),
                                                           tabPanel(
                                                             "Consumption parameters",
                                                             tags$br(),
                                                             hidden(htmlOutput("plot52text")),
                                                             plotOutput("distPlot52", width = "100%", height = "650px"),
                                                             
                                                             tags$br(),
                                                             htmlOutput("plot52_cap"),
                                                             tags$hr()
                                                           )
                                                         )
                                                       ),
                                                       ## tables----
                                                       tabPanel(
                                                         "Posterior predictive", icon = icon("table"),
                                                         value = "summaryTab",
                                                         tags$br(),
                                                         fluidRow(
                                                           h5(strong(
                                                             "POSTERIOR PREDICTIVE DISTRIBUTION SUMMARIES", 
                                                             style ="color:#004F71"
                                                           )),
                                                           
                                                           tags$br(),
                                                           actionButton("generateTposter", "Generate table", class = "btn-primary", 
                                                                        style="color: #fff; background-color: #D0006F; border-color: #D0006F"),
                                                           tags$br(),
                                                           hidden(htmlOutput("table1text")),
                                                           hidden(htmlOutput("table1text_generate")),
                                                           tags$style("#table1text_generate{color: #D0006F;}"),
                                                           textOutput("resultsview_need"),
                                                           tags$br(),
                                                           DT::DTOutput("values"),     # posterior predictive quantiles table
                                                           tags$br(),
                                                           htmlOutput("table1_cap"),
                                                           tags$hr()
                                                         )
                                                         
                                                       ),
                                                       tabPanel(
                                                         "Exposure limit", icon = icon("table"),
                                                         value = "limitsTab",
                                                         tags$br(),
                                                         fluidRow(
                                                           h5(strong(
                                                             "EXPOSURE LIMIT ANALYSIS", 
                                                             style ="color:#004F71"
                                                           )),
                                                           
                                                           tags$br(),
                                                           actionButton("generateTlimit", "Generate table", class = "btn-primary", 
                                                                        style="color: #fff; background-color: #D0006F; border-color: #D0006F"),
                                                           tags$br(),
                                                           hidden(htmlOutput("table2text")),
                                                           hidden(htmlOutput("table2text_generate")),
                                                           tags$style("#table2text_generate{color: #D0006F;}"),
                                                           tags$br(),
                                                           DT::DTOutput("pvalues"),     # exposure limit analysis table
                                                           tags$br(),
                                                           htmlOutput("table2_cap"),
                                                           tags$hr()
                                                         )
                                                       ),
                                                       
                                                       ## download report----
                                                       tabPanel(
                                                         "Download",icon = icon("file"),
                                                         
                                                         hidden(
                                                           wellPanel(
                                                             id = "report_dl",
                                                             help_report(),
                                                             htmlOutput("reportDLtext"),
                                                             
                                                             tags$br(),
                                                             tags$h4(strong("Download MCMC samples"), style = "text-align:left;color:#5C442C"),
                                                             wellPanel(id = "report_mcmc",
                                                                       help_report_mcmc(),
                                                                       # R-File That contains the simulation results from OpenBUGS:
                                                                       downloadButton("MCMCsamples", "MCMC samples .Rds", #class = "butt"),
                                                                                      class = "btn-primary", 
                                                                                      style="color: #343841; background-color: #CEB888; border-color: #CEB888"),
                                                                       helpText(strong("The file contains the MCMC samples for all parameters for all hazards and food types.")
                                                                       )
                                                             ),
                                                             tags$br(),
                                                             tags$h4(strong("Download reports"), style = "text-align:left;color:#5C442C"),
                                                             
                                                             wellPanel(id = "report_hazard_dl",
                                                                       downloadButton("report_hazard", "Report hazard.html", class = "btn-primary", 
                                                                                      style="color: #343841; background-color: #CEB888; border-color: #CEB888"),
                                                                       
                                                                       helpText(strong("The report is for the selected hazard. No figures or tables preview are needed.")),
                                                                       
                                                                       
                                                                       
                                                                       # Select hazard to be reported:
                                                                       selectInput(
                                                                         inputId = "thehazardnames_dl",
                                                                         label = h4(strong(("Select hazard"), style = "text-align:left;color:#CEB888")),
                                                                         choices = c(),
                                                                         width = '50%'
                                                                       ),
                                                                       fluidRow(
                                                                         column(4,
                                                                                h4(strong("Plot options"), style = "color:#CEB888"),
                                                                                selectInput(
                                                                                  "conf_interval_dl",
                                                                                  label = tags$span(strong("Credible interval"),
                                                                                                    style = "color:#004F71"),
                                                                                  choices = c(0.50, 0.80, 0.90, 0.95, 0.99),
                                                                                  selected = 0.95,
                                                                                  width = '200px'
                                                                                ),
                                                                                radioButtons(
                                                                                  "selectscale_dl",
                                                                                  label = h5(strong("Scale"), style = "color:#004F71"),
                                                                                  choiceNames = list(tags$span("Absolute"),
                                                                                                     tags$span("Logarithmic")),
                                                                                  choiceValues = list("Absolute", "Logarithmic")
                                                                                ),
                                                                                radioButtons(
                                                                                  "selectdist_dl",
                                                                                  label = h5(strong("Distributions"), style = "color:#004F71"),
                                                                                  choiceNames = list(tags$span("Cumulative"),
                                                                                                     tags$span("Density")),
                                                                                  choiceValues = list("Cumulative", "Density")
                                                                                )
                                                                         ), # end of plot options
                                                                         column(6,
                                                                                h4(strong("Quantiles figure (one plot with all food types)"), style = "color:#CEB888"),
                                                                                selectInput(
                                                                                  "selectQ_dl",
                                                                                  label = tags$span(strong("Total exposure quantile"), 
                                                                                                    style = "color:#004F71"),
                                                                                  choices = c(
                                                                                    "None",
                                                                                    "Q5%"="Q5% Exposure",
                                                                                    "Q10%"="Q10% Exposure",
                                                                                    "Q25%"="Q25% Exposure",
                                                                                    "Q50%"="Q50% Exposure",
                                                                                    "Q75%"="Q75% Exposure",
                                                                                    "Q90%"="Q90% Exposure",
                                                                                    "Q95%"="Q95% Exposure"
                                                                                  ),
                                                                                  width = '200px'
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.selectQ_dl != 'None'",
                                                                                  numericInput(
                                                                                    "nV_dl",
                                                                                    tags$span("Variability sample size for Q%", style = "color: #004F71;"),
                                                                                    min = 100,
                                                                                    max = 1000,
                                                                                    value = 100,
                                                                                    step = 10,
                                                                                    width = '200px'
                                                                                  ),
                                                                                  numericInput(
                                                                                    "nU_dl",
                                                                                    tags$span("Uncertainty sample size for Q%", style = "color: #004F71;"),
                                                                                    min = 100,
                                                                                    max = 2000,
                                                                                    value = 200,
                                                                                    step = 10,
                                                                                    width = '200px'
                                                                                  )
                                                                                )
                                                                         ) # end 2D-simulation selection
                                                                       )
                                                             ), # end of hazard report options,
                                                             wellPanel(id = "report_view_dl",
                                                                       tags$br(),
                                                                       # Report with figures currently visible in the app:
                                                                       downloadButton("report_view", "Report view.html", class = "btn-primary", 
                                                                                      style="color: #343841; background-color: #CEB888; border-color: #CEB888"),
                                                                       helpText(strong("The report will contain only the figures and tables currently visualized in the app.")),
                                                                       helpText(strong("BEFORE DOWNLOAD, generate in the app:
                                                                                      'Quantiles' figure, 'Posterior predictive' and 'Exposure limit' tables!"), 
                                                                                style = "text-align:left;color:#D0006F")
                                                             ), style = "background-color: #F4F3F2;"
                                                           )
                                                         )
                                                       )
                                                     )
                                                   ),
                                                   column(
                                                     2,
                                                     # Quantiles options----
                                                     conditionalPanel(
                                                       condition = "input.selectresults == 'exposureTab'&& input.exposures == 'Quantiles'",
                                                       style = "color: #F4F3F2;",
                                                       wellPanel(
                                                         help_quantiles(),
                                                         tags$br(),
                                                         selectInput(
                                                           "selectQ",
                                                           label = h4(strong("Total exposure quantile"), style = "color:#004F71"),
                                                           choices = c(
                                                             "None",
                                                             "Q5%"="Q5% Exposure",
                                                             "Q10%"="Q10% Exposure",
                                                             "Q25%"="Q25% Exposure",
                                                             "Q50%"="Q50% Exposure",
                                                             "Q75%"="Q75% Exposure",
                                                             "Q90%"="Q90% Exposure",
                                                             "Q95%"="Q95% Exposure"
                                                           )
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.selectQ != 'None'",
                                                           
                                                           numericInput(
                                                             "nV",
                                                             tags$span("Variability sample size for Q%", style = "color: #004F71;"),
                                                             min = 100,
                                                             max = 1000,
                                                             value = 100,
                                                             step = 10
                                                           ),
                                                           numericInput(
                                                             "nU",
                                                             tags$span("Uncertainty sample size for Q%", style = "color: #004F71;"),
                                                             min = 100,
                                                             max = 2000,
                                                             value = 200,
                                                             step = 10
                                                           )
                                                           
                                                         )
                                                       )
                                                     ),
                                                     
                                                     # View options----
                                                     ## Select food/hazard----
                                                     ### plots----
                                                     conditionalPanel(
                                                       condition = "input.selectresults == 'Concentrations' ||
                                                       input.selectresults == 'Consumptions'||
                                                       input.selectresults == 'exposureTab'||
                                                       input.selectresults == 'MCMC samples'",
                                                       wellPanel(
                                                         help_view1(),
                                                         h4(strong("To plot"), style = "color:#CEB888"),
                                                         
                                                         #### _concentrations----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Concentrations'",
                                                           selectInput(
                                                             inputId = "thefoodnames1",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           ),
                                                           selectInput(
                                                             inputId = "thehazardnames1",
                                                             label = strong(("Hazards"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         
                                                         #### _consumptions----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Consumptions' && input.consumptions == 'Consumptions'",
                                                           selectInput(
                                                             inputId = "thefoodnames2",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         
                                                         #### _serving correlation----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Consumptions'&& input.consumptions == 'serveCor'",
                                                           checkboxGroupInput(
                                                             inputId = "thefoodnames21",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         
                                                         #### _mean serving correlation----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Consumptions'&& input.consumptions == 'mserveCor'",
                                                           checkboxGroupInput(
                                                             inputId = "thefoodnames22",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         
                                                         #### _exposures----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'exposureTab'&& input.exposures == 'Exposures'",
                                                           selectInput(
                                                             inputId = "thefoodnames3",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           ),
                                                           selectInput(
                                                             inputId = "thehazardnames3",
                                                             label = strong(("Hazards"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         
                                                         #### _quantiles----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'exposureTab'&& input.exposures == 'Quantiles'&& input.selectQ != 'None'",
                                                           checkboxGroupInput(
                                                             inputId = "thefoodnames4",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           ),
                                                           selectInput(
                                                             inputId = "thehazardnames4",
                                                             label = strong(("Hazards"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         #### _MCMC----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'MCMC samples' && input.diagchoice == 'Concentration parameters'",
                                                           selectInput(
                                                             inputId = "thefoodnames5",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'MCMC samples' && input.diagchoice == 'Consumption parameters'",
                                                           selectInput(
                                                             inputId = "thefoodnames52",
                                                             label = strong(("Food types"), style = "color:#004F71"),
                                                             choices = c()
                                                           )
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'MCMC samples' && input.diagchoice == 'Concentration parameters'",
                                                           selectInput(
                                                             inputId = "thehazardnames5",
                                                             label = strong(("Hazards"), style = "color:#004F71"),
                                                           choices = c()
                                                          )
                                                         )
                                                       )
                                                     ),
                                                     
                                                     ### summary table----
                                                     conditionalPanel(
                                                       condition = "input.selectresults == 'summaryTab'",
                                                       wellPanel(
                                                         help_summary(),
                                                         h4(strong("Summarize"), style = "color:#CEB888"),
                                                         checkboxGroupInput(
                                                           "selectresultsT",
                                                           label = "",
                                                           choices = c("Concentrations",
                                                                       "Consumptions",
                                                                       "Exposures"),
                                                           selected =c("Concentrations",
                                                                       "Consumptions",
                                                                       "Exposures")
                                                         ),
                                                         tags$hr(),
                                                         checkboxGroupInput(
                                                           inputId = "thefoodnames_t",
                                                           label = strong(("Food types"), style = "color:#004F71"),
                                                           choices = c(),
                                                           selected = 1
                                                         ),
                                                         checkboxGroupInput(
                                                           inputId = "thehazardnames_t",
                                                           label = strong(("Hazards"), style = "color:#004F71"),
                                                           choices = c(),
                                                           selected = 1
                                                         )
                                                       )
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.selectresults == 'limitsTab'",
                                                       wellPanel(
                                                         help_limits(),
                                                         h4(strong("Analysis for"), style = "color:#CEB888"),
                                                         checkboxGroupInput(
                                                           inputId = "thefoodnames_t2",
                                                           label = strong(("Food types"), style = "color:#004F71"),
                                                           choices = c(),
                                                           selected = c()
                                                         ),
                                                         checkboxGroupInput(
                                                           inputId = "thehazardnames_t2",
                                                           label = strong(("Hazards"), style = "color:#004F71"),
                                                           choices = c(),
                                                           selected = c()
                                                         )
                                                       )
                                                     ),
                                                     
                                                     ## Plot options panel----
                                                     conditionalPanel(
                                                       condition = "input.selectresults == 'Concentrations' ||
                                                       input.selectresults == 'Consumptions' && input.consumptions == 'Consumptions'||
                                                       input.selectresults == 'exposureTab'&& input.exposures == 'Exposures'||
                                                       input.selectresults == 'exposureTab'&& input.exposures == 'Quantiles'",
                                                       wellPanel(
                                                         help_view2(),
                                                         h4(strong("Plot options"), style = "color:#CEB888"),
                                                         
                                                         ### _concentrations----
                                                         ### Credible interval----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Concentrations'",
                                                           
                                                           sliderInput(inputId="xrange",
                                                                       label=h5(strong("Range x-axis (%)"),
                                                                                style = "color:#004F71"), 
                                                                       min = 0, max = 0.99, value = 0.95, step = 0.05),
                                                           selectInput(
                                                             "conf_interval1",
                                                             h5(strong("Credible interval"),
                                                                style = "color:#004F71"),
                                                             choices = c(0.50, 0.80, 0.90, 0.95, 0.99),
                                                             selected = 0.95
                                                           )
                                                         ),
                                                         ### Scale----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Concentrations'",
                                                           radioButtons(
                                                             "selectscale1",
                                                             label = h5(strong("Scale"), style = "color:#004F71"),
                                                             choiceNames = list(tags$span("Absolute"),
                                                                                tags$span("Logarithmic")),
                                                             choiceValues = list("Absolute", "Logarithmic")
                                                           )
                                                         ),
                                                         ### Distributions----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Concentrations'",
                                                           radioButtons(
                                                             "selectdist1",
                                                             label = h5(strong("Distributions"), style = "color:#004F71"),
                                                             choiceNames = list(tags$span("Cumulative"),
                                                                                tags$span("Density")),
                                                             choiceValues = list("Cumulative", "Density")
                                                           )
                                                         ),
                                                         
                                                         ### _consumptions----
                                                         ### Credible interval----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Consumptions'",
                                                           
                                                           sliderInput(inputId="conf_lim2",
                                                                       label=h5(strong("Range x-axis (%)"),
                                                                                style = "color:#004F71"), 
                                                                       min = 0, max = 0.99, value = 0.95, step = 0.05),
                                                           selectInput(
                                                             "conf_interval2",
                                                             h5(strong("Credible interval"),
                                                                style = "color:#004F71"),
                                                             choices = c(0.50, 0.80, 0.90, 0.95, 0.99),
                                                             selected = 0.95
                                                           )
                                                         ),
                                                         ### Scale----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Consumptions'",
                                                           radioButtons(
                                                             "selectscale2",
                                                             label = h5(strong("Scale"), style = "color:#004F71"),
                                                             choiceNames = list(tags$span("Absolute"),
                                                                                tags$span("Logarithmic")),
                                                             choiceValues = list("Absolute", "Logarithmic")
                                                           )
                                                         ),
                                                         ### Distributions----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'Consumptions'",
                                                           radioButtons(
                                                             "selectdist2",
                                                             label = h5(strong("Distributions"), style = "color:#004F71"),
                                                             choiceNames = list(tags$span("Cumulative"),
                                                                                tags$span("Density")),
                                                             choiceValues = list("Cumulative", "Density")
                                                           )
                                                         ),
                                                         
                                                         ### _exposures----
                                                         ### Credible interval----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'exposureTab'&& input.exposures == 'Exposures'",
                                                           
                                                           sliderInput(inputId="conf_lim3",
                                                                       label=h5(strong("Range x-axis (%)"),
                                                                                style = "color:#004F71"), 
                                                                       min = 0, max = 0.99, value = 0.95, step = 0.05),
                                                           selectInput(
                                                             "conf_interval3",
                                                             h5(strong("Credible interval"),
                                                                style = "color:#004F71"),
                                                             choices = c(0.50, 0.80, 0.90, 0.95, 0.99),
                                                             selected = 0.95
                                                           )
                                                         ),
                                                         ### Scale----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'exposureTab'&& input.exposures == 'Exposures'",
                                                           radioButtons(
                                                             "selectscale3",
                                                             label = h5(strong("Scale"), style = "color:#004F71"),
                                                             choiceNames = list(tags$span("Absolute"),
                                                                                tags$span("Logarithmic")),
                                                             choiceValues = list("Absolute", "Logarithmic")
                                                           )
                                                         ),
                                                         ### Distributions----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'exposureTab'&& input.exposures == 'Exposures'",
                                                           radioButtons(
                                                             "selectdist3",
                                                             label = h5(strong("Distributions"), style = "color:#004F71"),
                                                             choiceNames = list(tags$span("Cumulative"),
                                                                                tags$span("Density")),
                                                             choiceValues = list("Cumulative", "Density")
                                                           )
                                                         ),
                                                         
                                                         ### _quantiles----
                                                         
                                                         ### Scale----
                                                         conditionalPanel(
                                                           condition = "input.selectresults == 'exposureTab'&& input.exposures == 'Quantiles'&& input.selectQ != 'None'",
                                                           radioButtons(
                                                             "selectscale4",
                                                             label = h5(strong("Scale"), style = "color:#004F71"),
                                                             choiceNames = list(tags$span("Absolute"),
                                                                                tags$span("Logarithmic")),
                                                             choiceValues = list("Absolute", "Logarithmic")
                                                           )
                                                         )
                                                       )
                                                     )
                                                   )
                                                 ))
                                     )
                            )
                          )
                 ),
                 # About the app----
                 tabPanel("About", value = "about_model_tab",
                          fluidPage(
                            navlistPanel(
                              widths = c(2, 9),
                              tabPanel("The model",
                                       
                                       fluidRow(
                                         column(8,
                                                wellPanel(
                                                  h4(tags$b("Requirements for the input data", value = "requirements")),
                                                  tags$br(),
                                                  tabsetPanel(type = "pills",
                                                              tabPanel(
                                                                tags$b("Concentration data"),
                                                                tags$br(),
                                                                tags$p(
                                                                  "The data file with hazard concentrations has to contain at least the columns with the following headings: ",
                                                                  tags$b("Type, Hazard, Concentration, LOQ, LOD, Unit"),
                                                                  ". Each row should contain one measurement result of one hazard from one food type."
                                                                ),
                                                                tags$ul(
                                                                  tags$li(tags$b("Type"), ": Name of the food item, e.g. Orange. These should match with the names used in
consumption data."),
                                                                  tags$li(tags$b("Hazard"), ": Name of the hazard, e.g. Cadmium."),
                                                                  tags$li(tags$b("Concentration"), ": Value of the measured concentration. If the measurement is below LOQ
the value should be marked as NA."),
                                                                  tags$li(tags$b("LOQ"), ": Limit of quantification. If the measurement is below LOD the value should be marked
as NA."),
                                                                  tags$li(tags$b("LOD"), ": Limit of detection."),
                                                                  tags$li(tags$b("Unit"), ": Measurement units, e.g. mg/kg, or cfu/g.")
                                                                ),
                                                                tags$p("Concentration data file may also contain other columns although these are
not currently used in the model."
                                                                ),
                                                                
                                                                tags$br(),
                                                                wellPanel(style = "background-color: #EEEEEE;",
                                                                          tags$p(
                                                                            "The column",
                                                                            tags$b("Type"),
                                                                            "is for the food type
in question, e.g. 'broiler'. This must be the same food type for which consumption data are given
in the other data files, i.e., the names should be spelled exactly the same way. There can be other columns
giving broader food categories, e.g. 'poultry' or 'meat foods', but these are not used in the model since the connection
between hazard data and food consumption data is done at the finest feasible level of food
type classification provided. Hence, 'Type' can denote raw ingredients or composite food
types containing many ingredients. The names of the food types could be any character strings (without spaces),
e.g. FoodEx2 codes or other naming system, but very long names should be avoided for clarity
and for more compact labels in the plotting windows. For example, 'minced meat casserole' could be
shortened to 'mmeatcas' when preparing the data files. The column ",
                                                                            tags$b("Hazard"),
                                                                            "specifies the hazard name in question, e.g., 'cadmium' or 'salmonella' for each
row, while the",
                                                                            tags$b("Concentration"),
                                                                            "column is for the numeric concentration values measured for the specific
hazard name and food type."
                                                                          ),
                                                                          tags$p(
                                                                            "Columns ",
                                                                            tags$b("LOQ"),
                                                                            "(limit of quantification) and ",
                                                                            tags$b("LOD"),
                                                                            "(limit of detection) specify
the measurement limits. The notation format is the same for chemical hazards and microbiological
hazards. The possibilities for each measurement are: reported numerical value (> LOQ), a value
between LOQ and LOD, or a value below LOD. The limits can also be different for each measurement.
If a concentration value is reported in column 'Concentration', it is interpreted as an exact measurement.
If the value was between the two limits, then both LOQ and LOD need to be given as numerical
values, while concentration is marked NA. If the value was below LOD, then LOD needs to be given
as numerical value, and both concentration and LOQ are marked NA. In this way, BIKE will know
which of the three situations is in question for each hazard concentration measurement per row."
                                                                          ),
                                                                          
                                                                          tags$p(
                                                                            "The column ",
                                                                            tags$b("Unit"),
                                                                            "is for specifying the measurement units, e.g. mg/kg, or cfu/g.",
                                                                            tags$u(
                                                                              
                                                                              " These are not
automatically converted to be compatible in the calculations."
                                                                            ),
                                                                            "Therefore, when preparing the data files compatible
measurements must be used. If the concentration values are per gram, so
must be the food consumption amounts as grams per day. A suitable measurement unit is such that it
does not lead to extremely small or large numerical values since this could affect also the numerical computations.
                                            Therefore, sensible measurement units have to be selected."
                                                                          ))
                                                              ),
                                                              tabPanel(
                                                                tags$b("Consumption data"),
                                                                tags$br(),
                                                                tags$p("The data file with food consumption can take two formats: either food diary or food frequency questionnaire (FFQ). 
                                                                A food diary data has to contain at least the columns with the following names: ", tags$b("IDnum, Weight, foodA1, foodA2, foodB1, foodB2, etc.")),
                                                                tags$ul(
                                                                  tags$li(tags$b("IDnum"), ": The respondent's number."),
                                                                  tags$li(tags$b("Weight"), ": Body weight of each respondent. Must be given for all individuals. Missing values
not allowed."),
                                                                  tags$li(tags$b("Food type with reporting day"), ": Name of the studied food items catenated with the index
number of the reporting day. E.g Orange1 for the consumption of orange on day 1, and
Orange2 for day 2, etc. There should be at least two reporting days for all respondents.
These columns should contain consumptions of the food items for each respondent, e.g.
grams per day. With FFQ data the columns are given only for long term mean daily consumptions of each food type (possibly zero). 
Note that the chosen weight units should match the units used for hazard
concentrations. If food consumption is given as grams per day, the hazard concentrations
in the food should be given as something per grams. BIKE does not convert measurement
units.")),
                                                                tags$p("The number of reporting days should ideally be the same for all persons. 
                                                                However, when it differs, the individual with largest number of days determines 
                                                                the number of columns in the table. Individuals with less days are then filled in as NA 
                                                                for 'missing days'. Consumption data file may also contain other columns although 
                                                                these are not currently used in the model. For example, age of respondents."
                                                                ),
                                                                
                                                                tags$br(),
                                                                wellPanel(style = "background-color: #EEEEEE;",
                                                                          tags$p(
                                                                            "Food consumption data can correspond to", tags$b("food diary data format"), 
                                                                            "where daily food consumption amounts
per each individual are tabulated per food item, row-by-row.
The column named ",
                                                                            tags$b("IDnum"),
                                                                            "is for the respondent's number, ",
                                                                            tags$b("Weight"),
                                                                            "is for the bodyweight, and rest are
the columns with names for the detailed food types consumed on a specific day. For example, ",
                                                                            tags$b("broiler1"),
                                                                            "for
consumption amounts of broiler on the first day. The next columns could be likewise ",
                                                                            tags$b("fish1, apple1,"),
                                                                            "
etc. These columns would be followed by the same list of food types for the second day, e.g. ",
                                                                            tags$b("broiler2,
fish2, apple2,"),
                                                                            "etc. There need to be at least two days recorded for each consumer but not necessarily the same
number of days for all (missing days marked 'NA'). Each row represents the reported consumptions of one consumer. The food
types can represent composite foods or raw ingredients as needed, but the names of the food types
(apart from the day number as the last character) have to be the same as those used in the hazard
concentration data. Each row gives either the consumed food amounts, or zeros, for the reported
days. The measurement units also need to be compatible with those in the concentration data, e.g.
consumptions in grams if concentrations are given per grams. Consumption data may originally come
in a hierarchical form that has several levels of food types with increasing details, e.g. seafood, fish,
smoked fish, smoked salmon. However, only one of those labels (character string
without spaces) has to be selected and used throughout in consumption data as well as in concentration data. This
labeling of food items can only be as detailed as both data sets permit. Food consumption data may also correspond to", tags$b("food frequency questionnaire (FFQ) data format"), 
"where each individual gives his/her long term average daily consumption of each food type. This can also be zero, 
if the individual is truly non-consumer of the food. The columns are then simply e.g.", tags$b("IDnum, Weight, broiler, fish."),
                                                                          )
                                                                )
                                                              ),
                                                              tabPanel(
                                                                tags$b("Occurrence data"),
                                                                tags$br(),
                                                                tags$p("The file with occurrence data
needs to contain the columns with the following headings:",
                                                                       tags$b("hazardnames, hazardtypes, limitexpo, foodA, foodB, etc.")),
                                                                
                                                                tags$ul(
                                                                  tags$li("1", tags$sup("st"), "column.", tags$b('hazardnames'),": List of the hazard names. E.g. Cadmium, Campylobacter."),
                                                                  tags$li("2", tags$sup("nd"), " column.", tags$b('hazardtypes'), ": Type of the given hazard, chemical", tags$b('K'), " or microbiological", tags$b('M'),"."),
                                                                  tags$li("3", tags$sup("rd"), "column.", tags$b('limitexpo'),": Exposure limit for every hazard. It can be a health based guidance value, for example tolerable daily intake TDI. 
                                                         Note that the exposure calculated by BIKE is in units per day, and thus for a health based guidance value given in units per a longer time period you should adjust it to units per day."),
                                                                  tags$li(tags$b("remaining columns."), "Names of the food types for the food items represented in data. These
columns specify how the concentration of the hazard (row) in this food type (column)
should be interpreted in the case where concentrations are below LOD. Either they represent
strictly", tags$b('positives'), " or ", tags$b('all'), "values which could include true zeros. BIKE will choose a model
according to this selection. If concentration data about some food-hazard combination is missing,
this should be marked as", tags$b('NA'), ".")),
                                                                tags$p("Note that 'all' for occurrence information implies that the concentration distribution will be
estimated jointly with prevalence parameter using a zero-inflated model where the
measurements below LOD are interpreted allowing the possibility of true zeros. If there were no measurements below LOD, 
the zero-inflated model is still applied for estimating prevalence from the sample of concentrations (which just happens to contain only >LOD). Then, prevalence
data file should mark the corresponding hazard sample data as 'NA' to signify the hazard
prevalence is not estimated from separate sample information, but only from the zero-inflated model."),
                                                                
                                                                tags$br(),
                                                                wellPanel(style = "background-color: #EEEEEE;",
                                                                          tags$p(
                                                                            "The file should contain a table with rows for each hazard
specifying the name of the hazard (e.g. 'cadmium'), the type (",
                                                                            tags$b("'K'"),
                                                                            "for chemical, ",
                                                                            tags$b("'M'"),
                                                                            "for
microbiological), and the relevant exposure limit of interest. The remaining columns have headers according to the food types, and the row entry
will specify whether the concentration data for that food-hazard pair should be interpreted to represent
only truly positive concentrations (even when below LOD), or as any measurements which might
contain also truly zeros when the measurement fell below LOD. In the former case, the correct entry
is", tags$b('positives'), ", and in the latter case the correct entry is", tags$b('all'), ".  If data for some food-hazard pair is missing, the correct entry is", tags$b('NA'),". Note that this
interpretation applies to full set of concentration data for the particular food-hazard pair."
                                                                          )
                                                                )
                                                              ),
                                                              tabPanel(
                                                                tags$b("Prevalence data"),
                                                                tags$br(),
                                                                tags$p("The file with prevalence data need to contain a table with columns with the following headings: ",
                                                                       tags$b("hazardnames, hazardtypes, infoods, npositive,
                                            nsample.")),
                                                                tags$ul(
                                                                  tags$li("1", tags$sup("st"), "column.", tags$b('hazardnames'), ": List of the hazard names."),
                                                                  tags$li("2", tags$sup("nd"), " column.", tags$b( 'hazardtypes'), ": Type of the given hazard, chemical", tags$b('K'), " or microbiological", tags$b('M'),"."),
                                                                  tags$li("3", tags$sup("rd"), " column.", tags$b( 'infoods'), ": Name of the food types in which the hazard occurs."),
                                                                  tags$li("4", tags$sup("th"), " column.", tags$b( 'npositive'), ": Number of detected positive samples. If sample information is not
available, this should be marked NA."),
                                                                  tags$li("5", tags$sup("th"), "column.", tags$b( 'nsample'), ": Number of samples in total. If sample information is not available,
this should be marked NA.")
                                                                ),
                                                                
                                                                tags$p("Note that 'NA' for sample information implies that the prevalence will be estimated jointly from
the concentration data using a zero-inflated model where the fraction of measurements below
LOD are interpreted allowing the possibility of true zeros. Then, occurrence data file should
mark the corresponding hazard concentrations as 'all' to signify they may contain both true
zeros and small positive values when below LOD. (But all measurements could also be >LOD when they represent a sample from 'all' foods) "),
                                                                tags$br(),
                                                                wellPanel(style = "background-color: #EEEEEE;",
                                                                          tags$p(
                                                                            "The file with prevalence data need to contain a table with column names",
                                                                            tags$b("hazardnames, hazardtypes, infoods, npositive,
                                            nsample"),
                                                                            ", in the exact came order (!), and the row entries for the last two columns will define the number of true
positivesand the sample size to be used if the concentration data for that food-hazard pair should
represent only positive concentrations. Otherwise, the number of true positives and the sample size
are marked 'NA'. If the sample information is marked 'NA', then the corresponding entry in occurrence
table should be 'all' to allow estimation of prevalence jointly with concentration distribution
using zero-inflated modeling. Hence, the 'positives' in occurrence table should go together with
specific values for prevalence sample data, and 'all' should go together with 'NA' in prevalence table."
                                                                          )
                                                                )
                                                              )
                                                  )
                                                )),
                                         column(4,
                                                tags$b("Possible reasons for error messages after RUN"),
                                                tags$hr(),
                                                tags$ul(
                                                  tags$li("There is a food-hazard combination for which concentration 
                                                          data is available, but NA is written in the occurence data table."),
                                                  tags$li("The section Exposures is opened and new model settings are selected."),
                                                  tags$li("Tabs were switched when computating was in progress.")
                                                ),
                                                tags$hr()
                                         )
                                       )
                                       
                              ),
                              tabPanel("Glossary",
                                       wellPanel(
                                         tags$p(h4("Incomplete list...")),       
                                         tags$ul(
                                           tags$br(),
                                           tags$li(tags$b("Concentration+"),"positive concentration (zeros excluded)."),
                                           tags$br(),
                                           tags$li(tags$b(" C.consumption/bw+"),"
positive chronic (i.e. mean) consumption per bodyweight per consumption day (zeros excluded)."),
                                           tags$br(),
                                           tags$li(tags$b(" A.consumption+"),"
positive acute consumption per consumption day (zeros excluded)."),
                                           tags$br(),
                                           tags$li(tags$b(" C.exposure/bw+"),"
positive chronic (i.e. mean) exposure per bodyweight per exposure day (zeros excluded)."),
                                           tags$br(),
                                           tags$li(tags$b(" A.exposure+"),"
positive acute exposure per exposure day (zeros excluded)."),
                                           tags$br(),
                                           tags$li(tags$b(" MCMC"),"
Markov chain Monte Carlo sampling method."),
                                           tags$br(),
                                           tags$li(tags$b(" uncertainty"),"
uncertainty of parameter values (represented by posterior distribution, realized as an MCMC
sample)."),
                                           tags$br(),
                                           tags$li(tags$b(" variability"),"
variability of quantities in a population, modelled as a distribution that depends on its parameters."),
                                           tags$br(),
                                           tags$li(tags$b(" quantile"),"
quantile point of a variability distribution (i.e. unknown, hence uncertain quantity), or quantile point of an uncertainty distribution."),
                                           tags$br(),
                                           tags$li(tags$b(" empirical distribution"),"
a distribution of data values as such."),
                                           tags$br(),
                                           tags$li(tags$b(" pseudo empirical distribution (of exposure)"),"
a distribution of exposure produced by sampling concentrations and consumptions directly from
the separate data sets for each. Usually requires either LB or UB substitutions for values below
LOQ or LOD, leading to lower or upper estimate of pseudo empirical exposure distribution."),
                                           tags$br(),
                                           tags$li(tags$b(" bootstrap"),"
resampling of data (with replacement) to create artificial random replicate of data, with
original sample size."),
                                           tags$br(),
                                           tags$li(tags$b(" 2D simulation"),"
simulation of parameter values from uncertainty distribution (here by MCMC) and simulation
of variable quantities from variability distributions (which are defined by those parameters)."),
                                           tags$br(),
                                           tags$li(tags$b(" consumption frequency"),"
proportion of actual consumption days in the long run."),
                                           tags$br(),
                                           tags$li(tags$b(" proportion of consumers"),"
proportion of true consumers of a food type in population."),
                                           tags$br(),
                                           tags$li(tags$b(" prevalence"),"
proportion of contaminated food items."
                                           ),
                                           tags$br(),
                                           tags$li(tags$b("LB substitution"), "replacement of the <LOD or <LOQ measurement by zero."),
                                           tags$br(),
                                           tags$li(tags$b("UB substitution"), "replacement of the <LOD or <LOQ measurement by the limit"),
                                           tags$br(),
                                           tags$li(tags$b("Posterior distribution"), "uncertainty distribution of all unknown parameters, conditionally on the observed data.")
                                         )
                                       )
                              ),
                              tabPanel("Source code", 
                                       wellPanel(
                                         
                                         tags$p(
                                           "The source code will be available at",
                                           tags$a("GitHub", href =
                                                    "https://github.com/", target = "_blank"), 
                                           " / ","The source code will be published at",
                                           tags$a("Zenodo?", href =
                                                    "https://zenodo.org/", target = "_blank")
                                         )
                                       )),
                              tabPanel("How to cite?", 
                                       wellPanel(
                                         tags$p(h4("Provisional title")),
                                         tags$p("Ranta J, Marinova-Todorova M, Mikkelä A, Suomi J, Tuominen P 2023. 
                                 BIKE foodborne exposure model - A graphical user interface for 
                                 the Bayesian dietary exposure assessment model for microbiological and chemical hazards (BIKE). 
                                 Finnish Food Authority, Helsinki, Finland. Available at",
                                                tags$a("https://bike-expo-shiny.rahtiapp.fi/", href =
                                                         "https://bike-expo-shiny.rahtiapp.fi/", target = "_blank"))
                                       ))
                              
                            ))
                 )
)


######################################################################################################
# Server logic required to run BUGS model and draw results----  
server <- function(input, output, session) {
  
  
  observe_helpers()
  
  # 1. Source files----
  
  # file with the functions used for generating the plots:
  source("plotsfunctions.R",local=TRUE)     # when food diary data
  source("plotsfunctionsFFQ.R",local=TRUE)  # when FFQ data
  
  # file with the functions used for generating the tables:
  source("tablefunctions.R", local = TRUE)    # when food diary data
  source("tablefunctionsFFQ.R", local = TRUE) # when FFQ data
  
  # observers
  source("observers.R", local = TRUE)
  
  
  # 2. Upload data files and print them:----
  ## Check if all files are uploaded:----
  output$uploadFiles <- renderUI({
    if (is.null(input$file_conct) | is.null(input$file_consm) |
        is.null(input$file_occ) |
        is.null(input$file_prev) == TRUE) {
      validate("Input data is missing. Upload all the required files."
      )
    }
  })
  
  
  ## Concentration----
  concen <- reactive({
    req(input$file_conct)
    A <- fread(input$file_conct$datapath,
               header = TRUE,
               sep = ",",
               quote = "")
    A
  })
  
  
  output$contents_conct <- renderTable({
    req(input$file_conct)
    
    inFile <- input$file_conct
    
    if (is.null(inFile))
      return(NULL)
    
    file_contents <- read.csv(inFile$datapath, sep = ",", dec = ".")
    
    required_columns <- c('Type', 'Hazard', 'Concentration', 'LOQ', 'LOD', 'Unit')
    column_names <- colnames(file_contents)
    
    validate(
      need(all(required_columns %in% column_names), 
           "This file does not meet the requirements. \nCheck the file format (the decimal and field separator!) and the column names.")
    )
    
    file_contents
  })
  
  
  ## Consumption----
  consum <- reactive({
    req(input$file_consm)
    A <- fread(input$file_consm$datapath,
               header = TRUE,
               sep = ",",
               quote = "")
    A
  })
  
  
  output$contents_consm <- renderTable({
    req(input$file_consm)
    
    inFile <- input$file_consm
    
    if (is.null(inFile))
      return(NULL)
    
    file_contents <- read.csv(inFile$datapath, sep = ",", dec = ".")
    
    required_columns <- c('IDnum', 'Weight')
    column_names <- colnames(file_contents)
    
    validate(
      need(all(required_columns %in% column_names), 
           "This file does not meet the requirements. \nCheck the file format (the decimal and field separator!) and the column names.")
    )
    
    file_contents
  })
  
  
  ## Occurrence----
  
  ocdata <- reactive({
    req(input$file_occ)
    A <- fread(input$file_occ$datapath,
               header = TRUE,
               sep = ",",
               quote = "")
    A
    
  })
  
  output$contents_occ <- renderTable({
    
    req(input$file_occ)
    inFile <- input$file_occ
    
    if (is.null(inFile))
      return(NULL)
    
    file_contents <- read.csv(inFile$datapath, sep = ",", dec = ".")
    
    required_columns <- c('hazardnames', 'hazardtypes', 'limitexpo')
    column_names <- colnames(file_contents)
    
    validate(
      need(all(required_columns %in% column_names), 
           "This file does not meet the requirements. \nCheck the file format (the decimal and field separator!) and the column names.")
    )
    
    file_contents
  })
  
  
  
  ## Prevalence----
  prevdata <- reactive({
    req(input$file_prev)
    
    A <- fread(input$file_prev$datapath,
               header = TRUE,
               sep = ",",
               quote = "")
    A
    
  })
  
  
  output$contents_prev <- renderTable({
    req(input$file_prev)
    
    inFile <- input$file_prev
    
    if (is.null(inFile))
      return(NULL)
    
    file_contents <- read.csv(inFile$datapath, sep = ",", dec = ".")
    
    required_columns <- c('hazardnames', 'hazardtypes', 'infoods', 'npositive', 'nsample')
    column_names <- colnames(file_contents)
    
    validate(
      need(all(required_columns %in% column_names), 
           "This file does not meet the requirements. \nCheck the file format (the decimal and field separator!) and the column names.")
    )
    
    file_contents
  })
  
  
  ## Formatted input data----
  ### extract units for the figures:
  units_hazard <- reactive({
    req(concen())
    unit_concen <- concen() %>% pull(Unit)
    hazard_concen <- concen() %>% pull(Hazard)
    
    df <- tibble(hazard_concen, unit_concen)
    
    df %>%                                         # Specify data frame
      group_by(hazard_concen, unit_concen) %>%       # Specify group indicator(s)
      summarise(.groups = 'drop')                  # Tibble with list of hazards and the units used for each
    
  })
  
  units_food <- reactive({
    req(concen())
    unit_consum <- concen() %>% pull(Unit)
    unit_consum <- sub(".*p.", "", unit_consum) # Extract characters after pattern
    food_consum <- concen() %>% pull(Type)
    
    
    df <- tibble(food_consum, unit_consum)
    
    df %>%                                         # Specify data frame
      group_by(food_consum, unit_consum) %>%       # Specify group indicator(s)
      summarise(.groups = 'drop')                  # Tibble with list of hazards and the units used for each
    
  })
  
  # saving the uploaded formatted data in data1() so it can be used further in the code
  data1 <- reactive({
    req(ocdata(), consum(), concen())
    ocdata <- as.data.frame(ocdata())
    consum <- as.data.frame(consum())
    concen <- as.data.frame(concen())
    source("format_consum_concen.R", local = TRUE)
    mget(ls())
  })
  
  
  # 3. Computations----
  
  
  ## Reactive values for BUGS results:---- 
  # This is modified attach.bugs function from R package R2OpenBUGS
  observe({
    r2 <- reactiveValues()
    reactiveValuesToList(r2)
  })
  
  attachbugs <- function (x){
    if (class(x) != "bugs") 
      stop("attachall() requires a bugs object.")
    x$sims.list$n.sims <- x$n.sims
    r2 <- x$sims.list
    invisible(r2)
  }
  
  
  ## Compute results from the full model once, then just use as 'current' results for all plots----
  # when only post-processing of the MCMC output is needed.
  
  currentresults  <- eventReactive(input$run, {
    req(ocdata(), consum(), concen(), prevdata())
    # the input data as data table:
    ocdata <- as.data.frame(ocdata())
    consum <- as.data.frame(consum())
    concen <- as.data.frame(concen())
    prevdata <- as.data.frame(prevdata())
    
    
    source("format_ocdata_prevdata.R", local = TRUE)
    source("format_consum_concen.R", local = TRUE)
    
    iter_n <- input$Iterations # number of the iterations selected by the user
    burnin <- 1000  # number of burnin iterations for MCMC runs
    
    if (input$datachoice!="FFQ"){
    ### model assumes independent daily consumptions----
    if (input$modelchoice == "Independent days") {
      
      withProgress(message = 'Computing in progress...',
                   value = 0.1, {
                     
                     # model without user variability in consumption frequency
                     if (input$modelchoice2 == "No") {
                       between.user.pvar <- 0
                     }
                     # model with user variability in consumption frequency
                     if (input$modelchoice2 == "Yes") {
                       between.user.pvar <- 1
                     }
                     
                     
                     source("makebugscodeA.R", local = TRUE)     # write code for OpenBUGS
                     source("RunBUGSfunctionA.R", local = TRUE)  # run BUGS
                     
                     
                     if((nhK > 0)&
                        (nhM > 0)) {
                       
                       logcK 
                       logLODK
                       logLODLimK
                       logLOQK
                       logLOQLimK
                       nbelowLODK
                       nbelowLOQK
                       nexactK
                       nhK
                       sdpriorlimK
                       
                       logcM 
                       logLODM
                       logLODLimM
                       logLOQM
                       logLOQLimM
                       nbelowLODM
                       nbelowLOQM
                       nexactM
                       nhM
                       sdpriorlimM
                     } else
                       if ((nhK > 0) & (nhM == 0)) {
                         
                         logcK 
                         logLODK
                         logLODLimK
                         logLOQK
                         logLOQLimK
                         nbelowLODK
                         nbelowLOQK
                         nexactK
                         nhK
                         sdpriorlimK
                       } else
                         if ((nhK == 0) & (nhM > 0)) {
                           
                           logcM 
                           logLODM
                           logLODLimM
                           logLOQM
                           logLOQLimM
                           nbelowLODM
                           nbelowLOQM
                           nexactM
                           nhM
                           sdpriorlimM
                         }
                     
                     bugs(data,inits,model.file="bikemodel.txt",debug=FALSE,parameters,n.chains=1,n.burnin=burnin,n.iter=iter_n,DIC=FALSE,codaPkg=FALSE)%>%
                       attachbugs()
                     
                     
                   })
      
    } else         # if independent days 
      
      ## model assumes that consuming next day depends on consuming previous day----
    if (input$modelchoice == "Dependent days") {
      
      withProgress(message = 'Computing in progress',
                   value = 0.1, {
                     
                     source("makebugscodeB.R", local = TRUE)     # write code for OpenBUGS
                     source("RunBUGSfunctionB.R", local = TRUE)  # run BUGS
                     
                     
                     if((nhK > 0)&
                        (nhM > 0)) {
                       
                       logcK 
                       logLODK
                       logLODLimK
                       logLOQK
                       logLOQLimK
                       nbelowLODK
                       nbelowLOQK
                       nexactK
                       nhK
                       sdpriorlimK
                       
                       logcM 
                       logLODM
                       logLODLimM
                       logLOQM
                       logLOQLimM
                       nbelowLODM
                       nbelowLOQM
                       nexactM
                       nhM
                       sdpriorlimM
                     } else
                       if ((nhK > 0) & (nhM == 0)) {
                         
                         logcK 
                         logLODK
                         logLODLimK
                         logLOQK
                         logLOQLimK
                         nbelowLODK
                         nbelowLOQK
                         nexactK
                         nhK
                         sdpriorlimK
                       } else
                         if ((nhK == 0) & (nhM > 0)) {
                           
                           logcM 
                           logLODM
                           logLODLimM
                           logLOQM
                           logLOQLimM
                           nbelowLODM
                           nbelowLOQM
                           nexactM
                           nhM
                           sdpriorlimM
                         }
                     
                     bugs(data,inits,model.file="bikemodel.txt",debug=FALSE,parameters,n.chains=1,n.burnin=burnin,n.iter=iter_n,DIC=FALSE,codaPkg=FALSE)%>%
                       attachbugs()
                     
                   })
      
    } # if dependent days
  } else # if not FFQ data
    
  if( input$datachoice=="FFQ"){
    withProgress(message = 'Computing in progress...',
                 value = 0.1, {
                   source("makebugscodeC.R", local = TRUE)     # write code for OpenBUGS
                   source("RunBUGSfunctionC.R", local = TRUE)  # run BUGS
                   
                   
                   if((nhK > 0)&
                      (nhM > 0)) {
                     
                     logcK 
                     logLODK
                     logLODLimK
                     logLOQK
                     logLOQLimK
                     nbelowLODK
                     nbelowLOQK
                     nexactK
                     nhK
                     sdpriorlimK
                     
                     logcM 
                     logLODM
                     logLODLimM
                     logLOQM
                     logLOQLimM
                     nbelowLODM
                     nbelowLOQM
                     nexactM
                     nhM
                     sdpriorlimM
                   } else
                     if ((nhK > 0) & (nhM == 0)) {
                       
                       logcK 
                       logLODK
                       logLODLimK
                       logLOQK
                       logLOQLimK
                       nbelowLODK
                       nbelowLOQK
                       nexactK
                       nhK
                       sdpriorlimK
                     } else
                       if ((nhK == 0) & (nhM > 0)) {
                         
                         logcM 
                         logLODM
                         logLODLimM
                         logLOQM
                         logLOQLimM
                         nbelowLODM
                         nbelowLOQM
                         nexactM
                         nhM
                         sdpriorlimM
                       }
                   bugs(data,inits,model.file="bikemodel.txt",debug=FALSE,parameters,n.chains=1,n.burnin=burnin,n.iter=iter_n,DIC=FALSE,codaPkg=FALSE)%>%
                     attachbugs()
                   
                   
                 })
    
  }  # if FFQ data 
    
  })
  
  
  
  ## Format some of the bugs results for dimension correction---- 
  solvedBugs <- eventReactive(input$run, {
    req(currentresults())
    results <- currentresults()
    
    # extract the required data from the input files:
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    nf <- length(foodnames)   
    hazardtypes <- ocdata$hazardtypes
    nhK <- sum(hazardtypes=="K") # number of chemical hazards
    nhM <- sum(hazardtypes=="M") # number of microbiological hazards
    
    input_datachoice <- input$datachoice 
    input_modelchoice <- input$modelchoice
    input_modelchoice2 <- input$modelchoice2
    input_modelchoice3 <- input$modelchoice3
    if(input_datachoice != "FFQ"){
    input_modelchoice4 <- input$modelchoice4
    }
    if(input_datachoice == "FFQ"){
    input_modelchoice4 <- input$modelchoice4FFQ  
    }  
    input_modelchoice5 <- input$modelchoice5
    
    n_sim <- results$n.sims
    mucK <- results$mucK
    mucM <- results$mucM
    pK <- results$pK
    pM <- results$pM
    sigcK <- results$sigcK
    sigcM <- results$sigcM
    mus0 <- results$mus0
    Ts0 <- results$Ts0
    logitp0 <- results$logitp0
    
    if(input_datachoice!="FFQ"){
    Ts <- results$Ts
    if(input_modelchoice=="Independent days"){
      if(input_modelchoice2=="Yes"){
        Tp <- results$Tp
      }
    }
    }
    
    # redefine dimensions if BUGS returns a lower dimensional object than intended, 
    # because it does not return values which were constant over MCMC 
    # (e.g. off-diagonals in correlation matrix with no correlations), nor keep redundant dimensions.
    # Therefore a vector of length 1 comes back simply as a scalar, etc. 
    # But these still need to be indexed in their original dimensions which need to be re-created.
    
    # for chemical, if there is only one hazard, one food:
    if((nhK==1)&(nf==1)){
      mucK_s <- array(mucK,dim=c(n_sim,1,1))
      sigcK_s <- array(sigcK,dim=c(n_sim,1,1))
      pK_s <- array(pK,dim=c(n_sim,1,1))
    }    else 
      if((nhK>1)&(nf==1)) {    # if more hazards, one food:
        mucK_s = array(mucK,dim=c(n_sim,nhK,1))
        sigcK_s = array(sigcK,dim=c(n_sim,nhK,1))
        pK_s = array(pK,dim=c(n_sim,nhK,1))
      } else
        if( (nhK==1)&(nf>1) ){  # if one hazard, more foods:
          mucK_s = array(mucK,dim=c(n_sim,1,nf))
          sigcK_s = array(sigcK,dim=c(n_sim,1,nf))
          pK_s = array(pK,dim=c(n_sim,1,nf))
        } else
        if(nhK==0){  # if no chemical hazards:
          mucK_s = NULL
          sigcK_s = NULL
          pK_s = NULL 
        }
    
    # for microbiological, if there is only one hazard, one food:
    if((nhM==1)&(nf==1)) {
      mucM_s <- array(mucM,dim=c(n_sim,1,1))
      sigcM_s <- array(sigcM,dim=c(n_sim,1,1))
      pM_s <- array(pM,dim=c(n_sim,1,1))
    } else
      if((nhM>1)&(nf==1)) {  # if more hazards, one food:
        mucM_s = array(mucM,dim=c(n_sim,nhM,1))
        sigcM_s = array(sigcM,dim=c(n_sim,nhM,1))
        pM_s = array(pM,dim=c(n_sim,nhM,1))
      } else
        if((nhM==1)&(nf>1)){  # if one hazard, more foods:
          mucM_s = array(mucM,dim=c(n_sim,1,nf))
          sigcM_s = array(sigcM,dim=c(n_sim,1,nf))
          pM_s = array(pM,dim=c(n_sim,1,nf))
        } else
        if(nhM==0){ # if no microbiol hazards
          mucM_s = NULL
          sigcM_s = NULL
          pM_s = NULL
        }
      
    
    if (nf==1) {
      mus0_s <- matrix(mus0,n_sim,1)
      logitp0_s <- matrix(logitp0,n_sim,1)
      
      if(input_datachoice!="FFQ"){
      Ts_s <- array(Ts,dim=c(n_sim,1,1))
      Ss_s <- array(0,dim=c(n_sim,nf,nf))
      for(u in 1:n_sim){  
        Ss_s[u,1:nf,1:nf] <- solve(Ts_s[u,1:nf,1:nf])  # Ss (solved Ts)
      }
      }
      
      Ts0_s <- array(Ts0,dim=c(n_sim,1,1))
      Ss0_s <- array(0,dim=c(n_sim,nf,nf))
      for(u in 1:n_sim){  
        Ss0_s[u,1:nf,1:nf] <- solve(Ts0_s[u,1:nf,1:nf])  # Ss0 (solved Ts0)
      }
      
      if(input_datachoice!="FFQ"){
      if(input_modelchoice=="Independent days"){
        
        if(input_modelchoice2=="Yes"){
          Tp_s <- array(Tp,dim=c(n_sim,1,1))
          Sp_s <- array(0,dim=c(n_sim,nf,nf))
          for(u in 1:n_sim){  
            Sp_s[u,1:nf,1:nf] <- solve(Tp_s[u,1:nf,1:nf])  # Sp (solved Tp) for Independent days and between user variability
          }
        } else {
          Tp_s = NULL
          Sp_s = NULL
        }
      } else {   
        # logitp0_s = NULL # NULL-definition not needed because logitp0 well defined in all options
        Tp_s = NULL
        Sp_s = NULL
      }
      }
      
      # formatted results when one food one hazard in the input data
      if(input_datachoice!="FFQ"){
      fresults <- list(mucK_s=mucK_s,sigcK_s=sigcK_s,pK_s=pK_s,
                        mucM_s=mucM_s,sigcM_s=sigcM_s,pM_s=pM_s,
                        mus0_s=mus0_s,Ts_s=Ts_s,Ts0_s=Ts0_s,Tp_s=Tp_s,
                        logitp0_s=logitp0_s,Ss_s=Ss_s,Ss0_s=Ss0_s,Sp_s=Sp_s
      )
      }
      if(input_datachoice=="FFQ"){
      fresults <- list(mucK_s=mucK_s,sigcK_s=sigcK_s,pK_s=pK_s,
                        mucM_s=mucM_s,sigcM_s=sigcM_s,pM_s=pM_s,
                        mus0_s=mus0_s,Ts0_s=Ts0_s,
                        logitp0_s=logitp0_s,Ss0_s=Ss0_s
      )}
    }
    
    
    # redefine dimensions if diagonal matrix with wrong off-diagonals, not zero off-diagonals, was returned from BUGS:
    # if there are more than one food in the input data  
    if(nf>1){
      
      
      if(input_datachoice!="FFQ"){
      if(input_modelchoice3=="No"){  # no correlations for log-amounts, reconstruct bugs matrix: 
        Ts_m <- array(0,dim=c(n_sim,nf,nf))
        for(j in 1:n_sim){
          diag(Ts_m[j,1:nf,1:nf]) <- diag(Ts[j,1:nf,1:nf]) 
        }
      } else {
        Ts_m = Ts
      }
      Ss_m <- array(0,dim=c(n_sim,nf,nf))
      for(u in 1:n_sim){  
        Ss_m[u,1:nf,1:nf] <- solve(Ts_m[u,1:nf,1:nf])  
      }
      }
      
      if(input_modelchoice4=="No"){  # no correlations for mean log-amounts, reconstruct bugs matrix: 
        Ts0_m <- array(0,dim=c(n_sim,nf,nf))
        for(j in 1:(n_sim)){
          diag(Ts0_m[j,1:nf,1:nf]) <- diag(Ts0[j,1:nf,1:nf])   
        }
      } else {
        Ts0_m = Ts0
      }
      Ss0_m <- array(0,dim=c(n_sim,nf,nf))
      for(u in 1:n_sim){  
        Ss0_m[u,1:nf,1:nf] <- solve(Ts0_m[u,1:nf,1:nf])  
      }
      
      if(input_datachoice!="FFQ"){
      if(input_modelchoice=="Independent days"){
        if(input_modelchoice2=="Yes"){ # variability of individual consumption frequencies (between user variability)
          if(input_modelchoice5=="No"){  
            # no correlations for logit-consumption frequencies, reconstruct bugs matrix: 
            Tp_m <- array(0,dim=c(n_sim,nf,nf))
            for(j in 1:(n_sim)){
              diag(Tp_m[j,1:nf,1:nf]) <- diag(Tp[j,1:nf,1:nf])   
            }
            Sp_m <- array(0,dim=c(n_sim,nf,nf))
            for(u in 1:n_sim){  
              Sp_m[u,1:nf,1:nf] <- solve(Tp_m[u,1:nf,1:nf])  
            }
          } 
          
          if(input_modelchoice5=="Yes"){  # if correlation "Yes"
            Tp_m <- Tp # full matrix directly from BUGS
            Sp_m <- array(0,dim=c(n_sim,nf,nf))
            for(u in 1:n_sim){  
              Sp_m[u,1:nf,1:nf] <- solve(Tp_m[u,1:nf,1:nf])  
            }
          }
          
        } else {
          Tp_m = NULL
          Sp_m = NULL
          
        } 
      } else {
        Tp_m = NULL
        Sp_m = NULL
        
      } 
      }
      
      if(input_datachoice!="FFQ"){
        fresults <- list(Ts_m=Ts_m,Ts0_m=Ts0_m,Tp_m=Tp_m,
                        Ss_m=Ss_m,Ss0_m=Ss0_m,Sp_m=Sp_m)
      }
      if(input_datachoice=="FFQ"){
        fresults <- list(Ts0_m=Ts0_m,
                          Ss0_m=Ss0_m)
      }
      
    }
    
    fresults
    
  })
  
  
  # 4. Figures----
  
  
  ## Plot 1, concentrations----
  
  distPlot1_1_1 <- reactive({
    
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    # units for the plot
    units_hazard <- units_hazard()
    unit_concen <- units_hazard$unit_concen
    hazard_concen <- units_hazard$hazard_concen
    
    # range x-axis
    input_lim <- as.double(input$xrange)
    
    # transform the credible interval selected by the user to upper and lower limits:
    input_conf_interval <- as.double(input$conf_interval1)
    input_upper <- 1-(1-input_conf_interval)/2
    input_lower <- (1-input_conf_interval)/2
    
    input_selectdist <- input$selectdist1 # cumulative or density
    input_selectscale <- input$selectscale1  # absolute or logarithmic  
    foodnamesused <- input$thefoodnames1 # selected foods
    hazardnamesused <- input$thehazardnames1 # selected hazards
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    foodindex <- match(foodnamesused,foodnames) # indexing of selected foods in all foods
    nfused <- length(foodnamesused)     # number of selected foods
    nhused <- length(hazardnamesused) # number of selected hazards
    hazardtypes <- ocdata$hazardtypes
    hazardnames <- ocdata$hazardnames
    hazardtypesused <- hazardtypes[is.element(hazardnames,hazardnamesused)] # types of selected hazards (chemical/microbiological)
    nf <- length(foodnames)   # Calculate the number of foods
    nh <- length(hazardnames) # Calculate the number of hazards
    nhK <- sum(hazardtypes=="K") # number of chemical hazards
    nhM <- sum(hazardtypes=="M") # number of microbiological hazards
    hazardnamesK <- hazardnames[hazardtypes=="K"] # chemical hazard names
    hazardnamesM <- hazardnames[hazardtypes=="M"] # microbiological hazard names
    hazardnamesusedK <- hazardnamesused[hazardtypesused=="K"] # selected che hazard names
    hazardnamesusedM <- hazardnamesused[hazardtypesused=="M"] # selected mic hazard names
    nhusedK <- length(hazardnamesusedK) # number of che hazards selected
    nhusedM <- length(hazardnamesusedM) # number of mic hazards selected
    hazardindex <- match(hazardnamesused,hazardnames) # 
    hazardindexK <- match(hazardnamesusedK,hazardnamesK) # indexing of selected hazards in all che hazards
    hazardindexM <- match(hazardnamesusedM,hazardnamesM) # indexing of selected hazards in all mic hazards
    
    logcK <- data1$logcK 
    logLOQK <- data1$logLOQK 
    logLODK <- data1$logLODK 
    logLOQLimK <- data1$logLOQLimK
    logLODLimK <- data1$logLODLimK
    logcM <- data1$logcM 
    logLOQM <- data1$logLOQM 
    logLODM <- data1$logLODM 
    logLOQLimM <- data1$logLOQLimM
    logLODLimM <- data1$logLODLimM
    nexactK <- data1$nexactK
    nexactM <- data1$nexactM
    
    
    n_sim <- results$n.sims
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1){
      mucK <- solvedBugs$mucK_s
      sigcK <- solvedBugs$sigcK_s
      pK <- solvedBugs$pK_s
      mucM <- solvedBugs$mucM_s
      sigcM <- solvedBugs$sigcM_s
      pM <- solvedBugs$pM_s
    }
    if(nf>1){
      mucK <- results$mucK
      sigcK <- results$sigcK
      pK <- results$pK
      mucM <- results$mucM
      sigcM <- results$sigcM
      pM <- results$pM
    }
    
    # call plot function:
    distPlot1_1(input_lim, unit_concen, hazard_concen, input_upper, input_lower, n_sim, input_selectdist, input_selectscale, foodnamesused,
                nfused, foodindex, hazardnamesused, hazardtypesused, nhused, 
                hazardnamesK, hazardnamesM, hazardnamesusedK, hazardnamesusedM,
                nhusedK, nhusedM, hazardindex, hazardindexK, hazardindexM,
                nexactK,nexactM,
                nhK, nf, mucK, sigcK, pK, nhM, mucM, sigcM, pM,
                logcK, logLOQK, logLODK, logLOQLimK, logLODLimK, logcM, logLOQM, logLODM, logLOQLimM, logLODLimM
    )
    
    recordPlot()
    
  })
  
  output$distPlot1 <- renderPlot({
    distPlot1_1_1()
    
  })
  

  
  output$plot1_cap <- renderText({
    paste(tags$b("Figure 1."), "Variability distribution for positive hazard concentrations in food. 
          The uncertainty of the true variability distribution is expressed by plotting a range 
          (e.g. pointwise 95%CI) of probable variability distributions (straw color). The uncertainty 
          distribution for mean concentration (yellow color) and median concentration (black color) are 
          plotted in bold lines. In log-scale, the mean and median are equal. For comparison with data, 
          the raw data are represented as cumulative empirical distribution. For data containing censored 
          concentration values, two empirical distributions are plotted, one with lower bound substitution 
          (blueberry color) and one with upper bound substitution method (raspberry color). These represent 
          the best case and worst case interpretations for censored values. Also, data points are plotted 
          as tick marks on the x-axis, showing exact measurements in raspberry color, LOQ-values in green 
          color, and LOD-values in blueberry color. Note that the distributions in the figures represent 
          truly positive concentrations, excluding zeros.")
  })
  
  
  ## Plot 2, consumptions----
  
  distPlot2_1_1 <- reactive({
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    units_food <- units_food()
    unit_consum <- units_food$unit_consum
    food_consum <- units_food$food_consum
    
    # range x-axis
    input_lim <- as.double(input$conf_lim2)
    
    # transform the credible interval selected by the user to upper and lower limits:
    input_conf_interval <- as.double(input$conf_interval2)
    input_upper <- 1-(1-input_conf_interval)/2
    input_lower <- (1-input_conf_interval)/2
    
    input_selectdist <-input$selectdist2 # cumulative or density
    input_selectscale <- input$selectscale2  # absolute or logarithmic
    foodnamesused <- input$thefoodnames2 # selected foods
    
    input_datachoice <- input$datachoice #####  Food diary or FFQ 
    input_modelchoice <- input$modelchoice
    input_modelchoice2 <- input$modelchoice2
    input_modelchoice3 <- input$modelchoice3
    if(input_datachoice != "FFQ"){
    input_modelchoice4 <- input$modelchoice4
    }
    if(input_datachoice == "FFQ"){
      input_modelchoice4 <- input$modelchoice4FFQ
    }
    input_modelchoice5 <- input$modelchoice5
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    nfused <- length(foodnamesused)
    foodindex <- match(foodnamesused, foodnames)
    nf <- length(foodnames)   # Calculate the number of foods
    
    IDnum <- data1$IDnum
    
    if(input_datachoice != "FFQ"){
    nd <- data1$nd     # number of days reported
    }
    nr <- data1$nr  	 # number of respondents
    logs <- data1$logs
    logsw <- data1$logsw
    
    n_sim <- results$n.sims
    muw <- results$muw
    sigw <- results$sigw
    
    
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1){
      mus0<- solvedBugs$mus0_s
      logitp0 <- solvedBugs$logitp0_s 
      Ss0 <- solvedBugs$Ss0_s
      if(input_datachoice != "FFQ"){
      Ss<- solvedBugs$Ss_s
      }
      
    }
    if(nf>1){ 
      mus0 <- results$mus0
      logitp0 <- results$logitp0
      Ss0 <- solvedBugs$Ss0_m
      if(input_datachoice != "FFQ"){
      Ss <- solvedBugs$Ss_m
      }
    }  
    
    # redundant inputs: input_modelchoice,input_modelchoice2,input_modelchoice3,input_modelchoice4,input_modelchoice5,
    # call plot function:
    if(input_datachoice!="FFQ"){
    distPlot2_1(input_lim, food_consum, unit_consum, input_upper, input_lower, n_sim, input_selectdist, input_selectscale, foodnamesused, nfused, foodindex,
                nf, nr, nd,logs, logsw,
                mus0,muw,logitp0,sigw,Ss,Ss0
    )
      output$plot2_cap <- renderText({
        paste(tags$b("Figure 2a."),"Variability distributions for both mean consumptions per bodyweight 
          ('chronic') and single consumptions ('acute'). The uncertainty of the true variability 
          distribution of positive consumptions is expressed by plotting a range (e.g. pointwise 95%CI) 
          of probable variability distributions (straw color). The uncertainty distributions for mean 
          (yellow color) and median (black color) are plotted in bold lines. Observed data for positive 
          consumptions are shown as raspberry color ticks and the empirical cumulative distribution in 
          raspberry color line. Note that the distributions in the figures represent truly positive 
          consumptions, excluding zeros.")
      })  
    }
      #### redundant inputs: input_modelchoice,input_modelchoice2,input_modelchoice3,input_modelchoice4,input_modelchoice5
    if(input_datachoice=="FFQ"){
      distPlot2_1FFQ(input_lim, food_consum, unit_consum, input_upper, input_lower, n_sim, input_selectdist, input_selectscale, foodnamesused, nfused, foodindex,
                  nf, nr,logs, logsw,
                  mus0,muw,logitp0,sigw,Ss0
      )
      output$plot2_cap <- renderText({
        paste(tags$b("Figure 2a."),"Variability distributions for both mean ('chronic') consumptions per bodyweight 
          and as absolute. The uncertainty of the true variability 
          distribution of positive mean consumptions is expressed by plotting a range (e.g. pointwise 95%CI) 
          of probable variability distributions (straw color). The uncertainty distributions for mean 
          (yellow color) and median (black color) are plotted in bold lines. Observed data for positive 
          mean consumptions are shown as raspberry color ticks and the empirical cumulative distribution in 
          raspberry color line. Note that the distributions in the figures represent truly positive 
          mean consumptions, excluding zero mean consumptions (based on FFQ data).")
      })
    }
    
    
    recordPlot()
    
  })
  
  output$distPlot2 <- renderPlot({
    distPlot2_1_1()
  })
  
  
  ## Plot 3, exposures----
  
  distPlot3_1_1 <- reactive({
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    units_hazard <- units_hazard()
    unit_concen <- units_hazard$unit_concen
    hazard_concen <- units_hazard$hazard_concen
    
    # range x-axis
    input_lim <- as.double(input$conf_lim3)
    
    # transform the credible interval selected by the user to upper and lower limits:
    input_conf_interval <- as.double(input$conf_interval3)
    input_upper <- 1-(1-input_conf_interval)/2
    input_lower <- (1-input_conf_interval)/2
    
    input_datachoice <- input$datachoice # FFQ data or food diary data 
    input_selectdist <-input$selectdist3 # cumulative or density
    input_selectscale <- input$selectscale3 # absolute or logarithmic
    input_modelchoice <- input$modelchoice
    input_modelchoice2 <- input$modelchoice2
    input_modelchoice3 <- input$modelchoice3
    if(input_datachoice != "FFQ"){
    input_modelchoice4 <- input$modelchoice4
    }
    if(input_datachoice == "FFQ"){
    input_modelchoice4 <- input$modelchoice4FFQ
    }  
    input_modelchoice5 <- input$modelchoice5
    
    foodnamesused <- input$thefoodnames3 # selected foods
    hazardnamesused <- input$thehazardnames3 # selected hazard
    Rall <- input$factor  # adjustment factor for concentrations
    Pall <- input$pfactor # adjustment factor for prevalences
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    hazardtypes <- ocdata$hazardtypes
    hazardnames <- ocdata$hazardnames
    nfused <- length(foodnamesused)
    foodindex <- match(foodnamesused, foodnames)
    hazardtypesused <- hazardtypes[is.element(hazardnames, hazardnamesused)]
    nhused <- length(hazardnamesused)
    hazardnamesK <- hazardnames[hazardtypes == "K"]
    hazardnamesM <- hazardnames[hazardtypes == "M"]
    hazardnamesusedK <- hazardnamesused[hazardtypesused == "K"]
    hazardnamesusedM <- hazardnamesused[hazardtypesused == "M"]
    nhusedK <- length(hazardnamesusedK)
    nhusedM <- length(hazardnamesusedM)
    hazardindex <- match(hazardnamesused, hazardnames)
    hazardindexK <- match(hazardnamesusedK, hazardnamesK)
    hazardindexM <- match(hazardnamesusedM, hazardnamesM)
    nf <- length(foodnames)   # Calculate the number of foods
    nh <- length(hazardnames) # Calculate the number of hazards
    nhK <- sum(hazardtypes=="K") # number of chemical hazards
    nhM <- sum(hazardtypes=="M") # number of microbiological hazards
    
    if(input_datachoice != "FFQ"){
    nd <- data1$nd # number of days reported
    }
    nr <- data1$nr  	   # number of respondents
    logs <- data1$logs
    logsw <- data1$logsw
    logcK <- data1$logcK
    logLOQK <- data1$logLOQK
    logLODK <- data1$logLODK
    logLOQLimK <- data1$logLOQLimK
    logLODLimK <- data1$logLODLimK
    logcM <- data1$logcM
    logLOQM <- data1$logLOQM
    logLODM <- data1$logLODM
    logLOQLimM <- data1$logLOQLimM
    logLODLimM <- data1$logLODLimM
    nexactK <- data1$nexactK
    nexactM <- data1$nexactM
    
    
    n_sim <- results$n.sims
    mus0 <- results$mus0
    muw <- results$muw
    sigw <- results$sigw
    
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1){
      mucK <- solvedBugs$mucK_s
      sigcK <- solvedBugs$sigcK_s
      pK <- solvedBugs$pK_s
      mucM <- solvedBugs$mucM_s
      sigcM <- solvedBugs$sigcM_s
      pM <- solvedBugs$pM_s
      mus0<- solvedBugs$mus0_s
      logitp0 <- solvedBugs$logitp0_s
      Ss0 <- solvedBugs$Ss0_s
      
      if(input_datachoice != "FFQ"){
      Ss <- solvedBugs$Ss_s 
      if(input_modelchoice=="Independent days"){
        if(input_modelchoice2=="Yes"){
          Sp <- solvedBugs$Sp_s
        }
      }
      }  
      
    }
    
    # redefine dimensions if diagonal matrix with wrong off-diagonals, not zero off-diagonals, was returned from BUGS:
    if(nf>1){
      mucK <- results$mucK
      sigcK <- results$sigcK
      pK <- results$pK
      mucM <- results$mucM
      sigcM <- results$sigcM
      pM <- results$pM
      mus0 <- results$mus0
      logitp0 <- results$logitp0
      Ss0 <- solvedBugs$Ss0_m
      
      if(input_datachoice != "FFQ"){
      Ss <- solvedBugs$Ss_m
      if(input_modelchoice=="Independent days"){
        if(input_modelchoice2=="Yes"){
          Sp <- solvedBugs$Sp_m
        }
      }
      }
      
    }
    
    # redundant inputs: input_modelchoice3, input_modelchoice4,
    # call plot function: 
    if(input_datachoice!="FFQ"){
    distPlot3_1(input_lim, unit_concen, hazard_concen, input_upper, input_lower, n_sim, 
                input_selectdist, input_selectscale, input_modelchoice, input_modelchoice2, 
                foodnamesused, nfused, foodindex, hazardnames,
                nhused,  hazardnamesusedK, hazardnamesusedM,
                nhusedK, nhusedM, hazardindexK, hazardindexM, Rall, Pall,nhK,nhM,nf,nr,nd,
                nexactK, nexactM, 
                logs,logsw,logcK,logLOQK,logLODK,logLOQLimK,logLODLimK, logcM,logLOQM,logLODM,logLOQLimM,logLODLimM,
                logitp0,mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                Ss,Ss0,Sp
    )
      output$plot3_cap <- renderText({
        paste(tags$b("Figure 3."), "Variability distribution for mean positive exposures per bodyweight (chemical) 
          or acute positive exposures (microbial). The uncertainty of the true variability distribution of 
          positive exposures is expressed by plotting a range (e.g. pointwise 95%CI) of probable variability 
          distributions (straw color). The uncertainty distributions for mean (yellow color) and median 
          (black color) are plotted in bold lines. The (pseudo)empirical cumulative distribution simulated 
          from bootstrapped data are shown using LB substitution (blueberry color) and UB substitution 
          (raspberry color) method. Note that the distributions in the figures represent truly positive 
          exposures, excluding zeros.")
      })  
    }
    # redundant inputs: input_modelchoice, input_modelchoice2, input_modelchoice3,input_modelchoice4,
    if(input_datachoice=="FFQ"){
      distPlot3_1FFQ(input_lim, unit_concen, hazard_concen, input_upper, input_lower, n_sim, input_selectdist, input_selectscale,
                  foodnamesused, nfused, foodindex, hazardnames,
                  nhused,  hazardnamesusedK, hazardnamesusedM,
                  nhusedK, nhusedM, hazardindexK, hazardindexM, Rall, Pall,nhK,nhM,nf,nr,
                  nexactK, nexactM, 
                  logs,logsw,logcK,logLOQK,logLODK,logLOQLimK,logLODLimK, logcM,logLOQM,logLODM,logLOQLimM,logLODLimM,
                  logitp0,mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                  Ss0
      )
      output$plot3_cap <- renderText({
        paste(tags$b("Figure 3."), "Variability distribution for mean exposures per bodyweight (chemical) 
          or mean absolute exposures (microbial). The uncertainty of the true variability distribution of 
          mean exposures is expressed by plotting a range (e.g. pointwise 95%CI) of probable variability 
          distributions (straw color). The uncertainty distributions for mean (yellow color) and median 
          (black color) are plotted in bold lines. The (pseudo)empirical cumulative distribution simulated 
          from bootstrapped data are shown using LB substitution (blueberry color) and UB substitution 
          (raspberry color) method. Note that the distributions in the figures represent true consumers, 
          excluding zero mean consumptions (based on FFQ data).")
      })
    }
    
    recordPlot()
    
    
  })
  
  output$distPlot3 <- renderPlot({
    distPlot3_1_1()
  }) 
  

  ## Plot 4, quantiles----
  
  distPlot4_1_1 <- eventReactive(input$generateP4, { 
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    units_hazard <- units_hazard()
    unit_concen <- units_hazard$unit_concen
    hazard_concen <- units_hazard$hazard_concen
    
    input_selectscale = input$selectscale4  # absolute or logarithmic
    input_selectQ <- input$selectQ
    nV = input$nV  # variability iterations for simulations
    nU = input$nU  # uncertainty iterations for simulations
    Rall = input$factor  # processing factors
    Pall = input$pfactor  # prevalence factors
    input_datachoice <- input$datachoice  # Food diary or FFQ data
    input_modelchoice <- input$modelchoice
    input_modelchoice2 <- input$modelchoice2
    input_modelchoice3 <- input$modelchoice3
    if(input_datachoice != "FFQ"){
    input_modelchoice4 <- input$modelchoice4
    }
    if(input_datachoice == "FFQ"){
    input_modelchoice4 <- input$modelchoice4FFQ
    }
    input_modelchoice5 <- input$modelchoice5
    foodnamesused = input$thefoodnames4 # selected foods
    hazardnamesused = input$thehazardnames4 # selected hazard
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    foodindex = match(foodnamesused,foodnames) # indexing of selected foods in all foods
    nfused = length(foodnamesused)     # number of selected foods
    nhused = length(hazardnamesused) # number of selected hazards
    hazardtypes <- ocdata$hazardtypes
    hazardnames <- ocdata$hazardnames
    hazardtypesused = hazardtypes[is.element(hazardnames,hazardnamesused)] # types of selected hazards (chemical/microbiological)
    nf <- length(foodnames)   # Calculate the number of foods
    nh <- length(hazardnames) # Calculate the number of hazards
    nhK <- sum(hazardtypes=="K") # number of chemical hazards
    nhM <- sum(hazardtypes=="M") # number of microbiological hazards
    hazardnamesK = hazardnames[hazardtypes=="K"] # chemical hazard names
    hazardnamesM = hazardnames[hazardtypes=="M"] # microbiological hazard names
    hazardnamesusedK = hazardnamesused[hazardtypesused=="K"] # selected che hazard names
    hazardnamesusedM = hazardnamesused[hazardtypesused=="M"] # selected mic hazard names
    nhusedK = length(hazardnamesusedK) # number of che hazards selected
    nhusedM = length(hazardnamesusedM) # number of mic hazards selected
    hazardindex = match(hazardnamesused,hazardnames) # 
    hazardindexK = match(hazardnamesusedK,hazardnamesK) # indexing of selected hazards in all che hazards
    hazardindexM = match(hazardnamesusedM,hazardnamesM) # indexing of selected hazards in all mic hazards
    
    nexactK <- data1$nexactK
    nexactM <- data1$nexactM
    
    
    n_sim <- results$n.sims
    muw <- results$muw
    sigw <- results$sigw
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1){
      mucK <- solvedBugs$mucK_s
      sigcK <- solvedBugs$sigcK_s
      pK <- solvedBugs$pK_s
      mucM <- solvedBugs$mucM_s
      sigcM <- solvedBugs$sigcM_s
      pM <- solvedBugs$pM_s
      mus0<- solvedBugs$mus0_s
      logitp0 <- solvedBugs$logitp0_s
      Ss0 <- solvedBugs$Ss0_s
      
      if(input_datachoice != "FFQ"){
        Ss<- solvedBugs$Ss_s  
      if(input_modelchoice=="Independent days"){
        if(input_modelchoice2=="Yes"){
          Sp<- solvedBugs$Sp_s
        }
      }
      }  
      
    }
    
    # redefine dimensions if diagonal matrix with wrong off-diagonals, not zero off-diagonals, was returned from BUGS:
    if(nf>1){ 
      mucK <- results$mucK
      sigcK <- results$sigcK
      pK <- results$pK
      mucM <- results$mucM
      sigcM <- results$sigcM
      pM <- results$pM
      mus0 <- results$mus0
      logitp0 <- results$logitp0
      Ss0 <- solvedBugs$Ss0_m
      
      if(input_datachoice!="FFQ"){
      Ss <- solvedBugs$Ss_m
      if(input_modelchoice=="Independent days"){
        if(input_modelchoice2=="Yes"){
          Sp <- solvedBugs$Sp_m
        }
      }
      }
      
    }
    
    # redundant inputs: input_modelchoice5, input_modelchoice4, input_modelchoice3,
    # call plot function:
    if(input_datachoice!="FFQ"){
    distPlot4_1(unit_concen, hazard_concen, n_sim, input_selectscale, input_selectQ, nV,
                nU, Rall, Pall, input_modelchoice, input_modelchoice2,
                nfused, foodindex, 
                nexactK, nexactM, 
                nhused, hazardnames, hazardnamesusedK, hazardnamesusedM,
                nhusedK, nhusedM, hazardindexK, hazardindexM, nhK,nhM,nf,
                mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                Ss,Ss0,Sp,
                logitp0
    )
      output$plot4_cap <- renderText({
        paste(tags$b("Figure 4."), "Cumulative distributions for separation of uncertainty and variability for 
          mean exposures per bodyweight (chemical) or acute exposures (microbial). The uncertainty of 
          true variability distribution of positive exposures summed from all selected foods is expressed 
          by plotting a sample of probable variability distributions (raspberry color). The uncertainty 
          distribution for the selected variability quantile is shown between vertical bars (and is subject
          to Monte Carlo error of 2D simulations).")
      })  
    }
    # redundant inputs: input_modelchoice5, input_modelchoice4, input_modelchoice3,input_modelchoice2, input_modelchoice,
    if(input_datachoice=="FFQ"){
      distPlot4_1FFQ(unit_concen, hazard_concen, n_sim, input_selectscale, input_selectQ, nV,
                  nU, Rall, Pall, 
                  nfused, foodindex, 
                  nexactK, nexactM, 
                  nhused, hazardnames, hazardnamesusedK, hazardnamesusedM,
                  nhusedK, nhusedM, hazardindexK, hazardindexM, nhK,nhM,nf,
                  mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                  Ss0,
                  logitp0
      )
      output$plot4_cap <- renderText({
        paste(tags$b("Figure 4."), "Cumulative distributions for separation of uncertainty and variability for 
          mean exposures per bodyweight (chemical) or mean absolute exposures (microbial). The uncertainty of 
          true variability distribution of mean exposures summed from all selected foods is expressed 
          by plotting a sample of probable variability distributions (raspberry color). The uncertainty 
          distribution for the selected variability quantile is shown between vertical bars (and is subject
          to Monte Carlo error of 2D simulations).")
      })
    }
    
    
    recordPlot()
    
    
  })
  
  
  
  output$distPlot4 <- renderPlot({
    withProgress(message = '2D simulation in progress...',
                 value = 0.1, {
                   distPlot4_1_1()
                 })
  })
  
  
  ## Plot 5.1, MCMC concentration----
  
  distPlot5_1_1 <- reactive({
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
  
    foodnamesused <- input$thefoodnames5 # selected foods
    hazardnamesused <- input$thehazardnames5 # selected hazard
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    foodindex <- match(foodnamesused, foodnames)
    nfused <- length(foodnamesused)     # number of selected foods
    hazardtypes <- ocdata$hazardtypes
    hazardnames <- ocdata$hazardnames
    hazardtypesused <- hazardtypes[is.element(hazardnames, hazardnamesused)]
    hazardnamesK <- hazardnames[hazardtypes == "K"]
    hazardnamesM <- hazardnames[hazardtypes == "M"]
    hazardnamesusedK <- hazardnamesused[hazardtypesused == "K"]
    hazardnamesusedM <- hazardnamesused[hazardtypesused == "M"]
    nhusedK <- length(hazardnamesusedK)
    nhusedM <- length(hazardnamesusedM)
    hazardindex <- match(hazardnamesused, hazardnames)
    hazardindexK <- match(hazardnamesusedK, hazardnamesK)
    hazardindexM <- match(hazardnamesusedM, hazardnamesM)
    nf <- length(foodnames)   # Calculate the number of foods
    nhK <- sum(hazardtypes=="K") # number of chemical hazards
    nhM <- sum(hazardtypes=="M") # number of microbiological hazards
    
    nexactK <- data1$nexactK
    nexactM <- data1$nexactM
    
    
    n_sim <- results$n.sims
    
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1) {
      mucK <- solvedBugs$mucK_s
      sigcK <- solvedBugs$sigcK_s
      pK <- solvedBugs$pK_s
      mucM <- solvedBugs$mucM_s
      sigcM <- solvedBugs$sigcM_s
      pM <- solvedBugs$pM_s
    }
    if(nf>1) {
      mucK <- results$mucK
      sigcK <- results$sigcK
      pK <- results$pK
      mucM <- results$mucM
      sigcM <- results$sigcM
      pM <- results$pM
      
    }
    
    # call plot function:
    distPlot5_1(n_sim, foodnamesused, nfused, foodindex, 
                hazardnamesusedK, hazardnamesusedM, nhusedK, nhusedM,
                hazardindexK, hazardindexM,nf,nhK,nhM,
                nexactK, nexactM,
                mucK,mucM,pK,pM,sigcK,sigcM
    )
    
    recordPlot()
    
  })
  
  output$distPlot5 <- renderPlot({
    distPlot5_1_1()
  })
  
  
  output$plot51_cap <- renderText({
    paste(tags$b("Figure 5a."), "Permuted MCMC samples of model parameters mu and sigma of the log-normal 
          (", tags$i('mu, sigma'),")-distribution, and prevalence", tags$b(tags$i('q')), "of the", tags$b('hazard'),". 
          For each parameter, also the approximated marginal probability density is shown.")
  })
  
  
  ## Plot 5.2, MCMC consumptions----
  
  distPlot5_2_1 <- reactive({
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    input_datachoice <- input$datachoice  # Food diary or FFQ data  
    foodnamesused <- input$thefoodnames52 # selected foods
    input_modelchoice3 <- input$modelchoice3 # serving correlations
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    nfused <- length(foodnamesused)     # number of selected foods
    
    foodindex <- match(foodnamesused, foodnames)
    nf <- length(foodnames)   # Calculate the number of foods
    
    n_sim <- results$n.sims
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1){
      mus0<- solvedBugs$mus0_s
      logitp0 <- solvedBugs$logitp0_s
      if(input_datachoice!="FFQ"){
      Ss <- solvedBugs$Ss_s
      }
      if(input_datachoice=="FFQ"){
        Ss0 <- solvedBugs$Ss0_s
      }
    }
    if(nf>1){ 
      mus0 <- results$mus0
      logitp0 <- results$logitp0
      if(input_datachoice!="FFQ"){
      Ss <- solvedBugs$Ss_m
      }
      if(input_datachoice=="FFQ"){
      Ss0 <- solvedBugs$Ss0_m  
      }
      
    }
    
    # call plot function:
    if(input_datachoice!="FFQ"){
    distPlot5_2(n_sim,foodnamesused, nfused, foodindex,
                nf,
                mus0,logitp0,
                Ss
    )
      output$plot52_cap <- renderText({
        paste(tags$b("Figure 5b."), "Permuted MCMC samples of model parameters mu and sigma of the log-normal 
          (", tags$i('mu, sigma'),")-distribution for single servings, and consumption frequency", tags$b(tags$i('p')), "of the", tags$b('food'),". 
          For each parameter, also the approximated marginal probability density is shown.")
      })  
    } else
    if(input_datachoice=="FFQ"){
      distPlot5_2FFQ(n_sim,foodnamesused, nfused, foodindex,
                  nf,
                  mus0,logitp0,
                  Ss0
      )
      output$plot52_cap <- renderText({
        paste(tags$b("Figure 5b."), "Permuted MCMC samples of model parameters mu and sigma of the log-normal 
          (", tags$i('mu, sigma'),")-distribution for mean servings (FFQ data), and population proportion of consumers", tags$b(tags$i('p')), "of the", tags$b('food'),". 
          For each parameter, also the approximated marginal probability density is shown.")
      })
    }
    
    recordPlot()
    
  })
  
  output$distPlot52 <- renderPlot({
    distPlot5_2_1()
  })
  
  
  
  ## Plot 6, serving correlations----
  
  distPlot6_1_1 <- reactive({
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    # units for the plot
    units_food <- units_food()
    unit_consum <- units_food$unit_consum
    food_consum <- units_food$food_consum
    
    foodnamesused <- input$thefoodnames21 # selected foods
    
    input_datachoice <- input$datachoice
    if(input_datachoice != "FFQ"){
    input_modelchoice4 <- input$modelchoice4
    input_modelchoice3 <- input$modelchoice3 
    }

    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    nfused <- length(foodnamesused)
    foodindex <- match(foodnamesused, foodnames)
    nf <- length(foodnames)   # Calculate the number of foods
    
    if(input_datachoice != "FFQ"){
    nd <- data1$nd # number of days reported
    }
    nr <- data1$nr  	   # number of respondents
    logsw <- data1$logsw
    
    n_sim <- results$n.sims
    mus0 <- results$mus0
    
    # redefine dimensions if diagonal matrix with wrong off-diagonals, not zero off-diagonals, was returned from BUGS:
    if(input_datachoice != "FFQ"){
    Ss <- solvedBugs$Ss_m
    }
    Ss0 <- solvedBugs$Ss0_m
    
    
    # redundant inputs: input_modelchoice4, input_modelchoice3
    # call plot function:
    if(input_datachoice != "FFQ"){  # this is only for Food diary data
    distPlot6_1(food_consum, unit_consum, n_sim, foodnamesused,
                nfused, foodindex,nr,nd,nf,logsw,
                mus0,
                Ss,Ss0
    )
      output$plot6_cap <- renderText({
        paste(tags$b("Figure 2b."), "Pairwise scatter plots of logarithms of actual positive consumptions per 
         bodyweights. Data points (blueberry color) and model based simulations (raspberry color).")
      })    
    }
    if(input_datachoice == "FFQ"){  # this could be done only with Food diary data
      output$plot6_cap <- renderText({
        paste(tags$b("Figure 2b."), "FFQ data has no observed individual (acute) consumptions. Plot not possible.")
      })    
    }
    
    
    recordPlot()
    
  })
  
  output$distPlot6 <- renderPlot({
    distPlot6_1_1()
  })
  
  
  ## Plot 7, mean serving correlations ----
  
  distPlot7_1_1 <- reactive({
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    # units for the plot
    units_food <- units_food()
    unit_consum <- units_food$unit_consum
    food_consum <- units_food$food_consum
    
    foodnamesused <- input$thefoodnames22 # selected foods
    
    input_datachoice <- input$datachoice
    input_modelchoice3 <- input$modelchoice3
    if(input_datachoice != "FFQ"){
    input_modelchoice4 <- input$modelchoice4  
    }
    if(input_datachoice == "FFQ"){
    input_modelchoice4 <- input$modelchoice4FFQ  
    }
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    nfused <- length(foodnamesused)
    foodindex <- match(foodnamesused, foodnames)
    nf <- length(foodnames)   # Calculate the number of foods
    
    if(input_datachoice != "FFQ"){
    nd <- data1$nd # number of days reported
    }
    nr <- data1$nr  	   # number of respondents
    logsw <- data1$logsw
    
    
    n_sim <- results$n.sims
    mus0 <- results$mus0
    
    # redefine dimensions if diagonal matrix with wrong off-diagonals, not zero off-diagonals, was returned from BUGS:
    if(input_datachoice !="FFQ"){
    Ss <- solvedBugs$Ss_m  
    }
    Ss0 <- solvedBugs$Ss0_m
    
    # call plot function:
    # redundant inputs: input_modelchoice4
    if(input_datachoice != "FFQ"){
    distPlot7_1(food_consum, unit_consum, n_sim, foodnamesused, nfused, foodindex,
                nf,nr,nd,logsw,
                mus0,
                Ss,Ss0
    )
    }
    if(input_datachoice == "FFQ"){
      distPlot7_1FFQ(food_consum, unit_consum, n_sim, foodnamesused, nfused, foodindex,
                  nf,nr,logsw,
                  mus0,
                  Ss0
      )
    }  
    
    recordPlot()
    
  })
  
  output$distPlot7 <- renderPlot({
    distPlot7_1_1()
  })
  
  output$plot7_cap <- renderText({
    paste(tags$b("Figure 2c."), "Pairwise scatter plots of logarithms of mean positive consumptions per 
          bodyweights. Data points (blueberry color) and model based simulations (raspberry color). ")
  })
  
  
  
  # 5. Tables----
  
  ## Table 1, posterior predictive distribution summary----
  
  resultValues <- eventReactive(input$generateTposter, {
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    theresults <- input$selectresultsT
    input_datachoice <- input$datachoice
    input_modelchoice <- input$modelchoice
    input_modelchoice2 <- input$modelchoice2
    input_modelchoice3 <- input$modelchoice3
    if(input_datachoice != "FFQ"){
    input_modelchoice4 <- input$modelchoice4
    }
    if(input_datachoice == "FFQ"){
    input_modelchoice4 <- input$modelchoice4FFQ
    }
    input_modelchoice5 <- input$modelchoice5
    foodnamesused <- input$thefoodnames_t # selected foods
    hazardnamesused <- input$thehazardnames_t
    Rall <- input$factor # optional processing factors
    Pall <- input$pfactor  # optional prevalence factors
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    foodindex <- match(foodnamesused,foodnames) # indexing of selected foods in all foods
    nfused <- length(foodnamesused)     # number of selected foods
    nhused <- length(hazardnamesused) # number of selected hazards
    hazardtypes <- ocdata$hazardtypes
    hazardnames <- ocdata$hazardnames
    hazardtypesused <- hazardtypes[is.element(hazardnames,hazardnamesused)] # types of selected hazards (chemical/microbiological)
    nf <- length(foodnames)   # Calculate the number of foods
    nhK <- sum(hazardtypes=="K") # number of chemical hazards
    nhM <- sum(hazardtypes=="M") # number of microbiological hazards
    hazardnamesK <- hazardnames[hazardtypes=="K"] # chemical hazard names
    hazardnamesM <- hazardnames[hazardtypes=="M"] # microbiological hazard names
    hazardnamesusedK <- hazardnamesused[hazardtypesused=="K"] # selected che hazard names
    hazardnamesusedM <- hazardnamesused[hazardtypesused=="M"] # selected mic hazard names
    nhusedK <- length(hazardnamesusedK) # number of che hazards selected
    nhusedM <- length(hazardnamesusedM) # number of mic hazards selected
    hazardindex <- match(hazardnamesused,hazardnames) # 
    hazardindexK <- match(hazardnamesusedK,hazardnamesK) # indexing of selected hazards in all che hazards
    hazardindexM <- match(hazardnamesusedM,hazardnamesM) # indexing of selected hazards in all mic hazards
    
    nexactK <- data1$nexactK
    nexactM <- data1$nexactM
    
    
    n_sim <- results$n.sims
    muw <- results$muw
    sigw <- results$sigw
    
    
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1){
      mucK <- solvedBugs$mucK_s
      sigcK <- solvedBugs$sigcK_s
      pK <- solvedBugs$pK_s
      mucM <- solvedBugs$mucM_s
      sigcM <- solvedBugs$sigcM_s
      pM <- solvedBugs$pM_s
      mus0<- solvedBugs$mus0_s
      if(input_datachoice!="FFQ"){
      Ss<- solvedBugs$Ss_s
      }
      Ss0 <- solvedBugs$Ss0_s
      logitp0 <- solvedBugs$logitp0_s  
      
      if(input_datachoice != "FFQ"){ 
      if(input_modelchoice=="Independent days"){
        if(input_modelchoice2=="Yes"){
          Sp <- solvedBugs$Sp_s
        }
      }
      }  
      
    }
    if(nf>1){
      mucK <- results$mucK
      sigcK <- results$sigcK
      pK <- results$pK
      mucM <- results$mucM
      sigcM <- results$sigcM
      pM <- results$pM
      mus0 <- results$mus0
      logitp0 <- results$logitp0
      
      Ss0 <- solvedBugs$Ss0_m
      if(input_datachoice != "FFQ"){
      Ss <- solvedBugs$Ss_m
      Sp <- solvedBugs$Sp_m
      }
      
    }
    
    
    # call table function: 
    if(input_datachoice != "FFQ"){
    table1(n_sim, input_modelchoice,input_modelchoice2,input_modelchoice3,input_modelchoice4,input_modelchoice5,
           theresults, foodnamesused, nfused, foodindex, hazardnames, 
           hazardnamesusedK,hazardnamesusedM, nhusedK, nhusedM, hazardindexK, hazardindexM,
           Rall, Pall,nhK,nhM,nf,nexactK,nexactM,
           mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
           logitp0,
           Ss,Ss0,Sp
    )
    } else
    if(input_datachoice == "FFQ"){
      table1FFQ(n_sim, input_modelchoice,input_modelchoice2,input_modelchoice3,input_modelchoice4,input_modelchoice5,
             theresults, foodnamesused, nfused, foodindex, hazardnames, 
             hazardnamesusedK,hazardnamesusedM, nhusedK, nhusedM, hazardindexK, hazardindexM,
             Rall, Pall,nhK,nhM,nf,nexactK,nexactM,
             mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
             logitp0,
             Ss0
      )
    }
    
  })
  
  
  output$values <- DT::renderDT({
    withProgress(message = 'Generating posterior predictive summaries...',
                 value = 0.1, {
                   
                   
                   resultValues()%>%
                     DT::datatable(rownames = FALSE,
                                   class = 'row-border stripe',
                                   extensions = c("Buttons", "RowGroup"),
                                   options = list(
                                     rowGroup = list(dataSrc = 1), # row grouping feature -> grouped by second column value 
                                     
                                     dom = "Bfrtip",
                                     buttons = list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span> Excel *', title = NULL, 
                                                         exportOptions = list(columns = ":visible"),
                                                         filename =  paste("Posterior_predictive_distribution_summaries")), 
                                                    list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV *', title = NULL, 
                                                         exportOptions = list(columns = ":visible"),
                                                         filename =  paste("Posterior_predictive_distribution_summaries"))
                                     ),
                                     
                                     paging=FALSE, scrollY = "480px"
                                   )
                     )
                 })
  })
  
  output$table1_cap <- renderText({
    paste(tags$b("Table 1."), "Posterior predictive distributions present predictions where all uncertainties and 
          variabilities are integrated into one single probability distribution. This can be a useful summary for 
          assessing what is now probable, given all the data with all its variability and parameter uncertainties. 
          The distribution is obtained by averaging (weighing) the possible variability distributions over the uncertainty 
          distribution of their parameters.")
  })
  
  ## Table 2, exposure limit ----
  
  resultProbs <- eventReactive(input$generateTlimit, { 
    req(currentresults())
    ocdata <- as.data.frame(ocdata())
    data1 <- data1()
    results <- currentresults()
    solvedBugs <- solvedBugs()
    
    input_datachoice <- input$datachoice # FFQ or Food diary data
    input_modelchoice <- input$modelchoice
    input_modelchoice2 <- input$modelchoice2
    
    foodnamesused <- input$thefoodnames_t2 # selected foods
    hazardnamesused <- input$thehazardnames_t2
    Rall <- input$factor # optional processing factors
    Pall <- input$pfactor  # optional prevalence factors
    
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    foodindex <- match(foodnamesused,foodnames) # indexing of selected foods in all foods
    nfused <- length(foodnamesused)     # number of selected foods
    hazardtypes <- ocdata$hazardtypes
    hazardnames <- ocdata$hazardnames
    hazardtypesused <- hazardtypes[is.element(hazardnames,hazardnamesused)] # types of selected hazards (chemical/microbiological)
    nf <- length(foodnames)   # Calculate the number of foods
    nhK <- sum(hazardtypes=="K") # number of chemical hazards
    nhM <- sum(hazardtypes=="M") # number of microbiological hazards
    hazardnamesK <- hazardnames[hazardtypes=="K"] # chemical hazard names
    hazardnamesM <- hazardnames[hazardtypes=="M"] # microbiological hazard names
    hazardnamesusedK <- hazardnamesused[hazardtypesused=="K"] # selected che hazard names
    hazardnamesusedM <- hazardnamesused[hazardtypesused=="M"] # selected mic hazard names
    nhusedK <- length(hazardnamesusedK) # number of che hazards selected
    nhusedM <- length(hazardnamesusedM) # number of mic hazards selected
    hazardindexK <- match(hazardnamesusedK,hazardnamesK) # indexing of selected hazards in all che hazards
    hazardindexM <- match(hazardnamesusedM,hazardnamesM) # indexing of selected hazards in all mic hazards
    
    nexactK <- data1$nexactK
    nexactM <- data1$nexactM
    
    limitexpo <- ocdata$limitexpo
    limitexpoK <- numeric()
    limitexpoM <- numeric()
    limitexpoK <- as.numeric(limitexpo[hazardtypes=="K"])
    limitexpoM <- as.numeric(limitexpo[hazardtypes=="M"])
    
    
    
    n_sim <- results$n.sims
    muw <- results$muw
    sigw <- results$sigw
    
    
    # redefine dimensions if scalars were returned from BUGS:
    if(nf==1){
      mucK <- solvedBugs$mucK_s
      sigcK <- solvedBugs$sigcK_s
      pK <- solvedBugs$pK_s
      mucM <- solvedBugs$mucM_s
      sigcM <- solvedBugs$sigcM_s
      pM <- solvedBugs$pM_s
      mus0<- solvedBugs$mus0_s
      logitp0 <- solvedBugs$logitp0_s
      Ss0 <- solvedBugs$Ss0_s
      
      if(input_datachoice != "FFQ"){
        Ss<- solvedBugs$Ss_s  
      if(input_modelchoice=="Independent days"){
        if(input_modelchoice2=="Yes"){
          Sp <- solvedBugs$Sp_s
        }
      }
      }  
      
    }
    if(nf>1){
      mucK <- results$mucK
      sigcK <- results$sigcK
      pK <- results$pK
      mucM <- results$mucM
      sigcM <- results$sigcM
      pM <- results$pM
      mus0 <- results$mus0
      logitp0 <- results$logitp0
      Ss0 <- solvedBugs$Ss0_m
      
      if(input_datachoice != "FFQ"){
        Ss<- solvedBugs$Ss_m  
        if(input_modelchoice=="Independent days"){
          if(input_modelchoice2=="Yes"){
            Sp <- solvedBugs$Sp_m
          }
        }
      } 
      
    }
    
    # call table function: 
    if(input_datachoice != "FFQ"){
    table2(n_sim, input_modelchoice,input_modelchoice2,foodnamesused,nfused,foodindex,hazardnames,
           hazardnamesused,hazardtypesused,hazardnamesK,hazardnamesM,
           hazardnamesusedK,hazardnamesusedM,nhusedK,nhusedM,hazardindexK,hazardindexM,
           Rall,Pall,nhK,nhM,nf,nexactK,nexactM,limitexpoK,limitexpoM,
           mus0,mucK,mucM,sigcK,sigcM,pK,pM,logitp0,muw,sigw,
           Ss,Ss0,Sp 
    )} else
    if(input_datachoice == "FFQ"){
      table2FFQ(n_sim, input_modelchoice,input_modelchoice2,foodnamesused,nfused,foodindex,hazardnames,
             hazardnamesused,hazardtypesused,hazardnamesK,hazardnamesM,
             hazardnamesusedK,hazardnamesusedM,nhusedK,nhusedM,hazardindexK,hazardindexM,
             Rall,Pall,nhK,nhM,nf,nexactK,nexactM,limitexpoK,limitexpoM,
             mus0,mucK,mucM,sigcK,sigcM,pK,pM,logitp0,muw,sigw,
             Ss0 
      )}
    
    
  })
  
  
  output$pvalues <- DT::renderDT({
    withProgress(message = 'Generating exposure limit analysis table...',
                 value = 0.1, {
                   
                   
                   resultProbs()%>%
                     DT::datatable(rownames = FALSE,
                                   class = 'row-border stripe',
                                   extensions = c("Buttons", "RowGroup"),
                                   options = list(
                                     rowGroup = list(dataSrc = 0), # row grouping feature -> grouped by second column value 
                                     
                                     dom = "Bfrtip",
                                     buttons = list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span> Excel *', title = NULL, 
                                                         exportOptions = list(columns = ":visible"),
                                                         filename =  paste("Exposure_limit_analysis")), 
                                                    list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV *', title = NULL, 
                                                         exportOptions = list(columns = ":visible"),
                                                         filename =  paste("Exposure_limit_analysis"))
                                     ),
                                     
                                     paging=FALSE, scrollY = "480px"
                                   )
                     )
                 })
  }) 
  
  output$table2_cap <- renderText({
    #req(resultProbs())
    paste(tags$b("Table 2."), "Exposure limit analysis presents the part of the population (1 = 100%) with exposure below the 
          limit given in the Occurrence dataset. The estimate is given for all days and for days with only positive consumption 
          of contaminated food. The columns", tags$b("Q05, Q50, Q95"), "refer to uncertainty quantiles for the 'Quantity' in first column.
          The 'Quantity'", tags$b(" Q95(exposure)"), "refer to the unknown quantiles of variability distribution.  
          This becomes estimated with the indicated uncertainty quantiles Q05, Q50, Q95.")
  })
  
  
  
  # 6. Report & downloads-------
  
  ## bugs-results for RMarkdown----
  bugsresults <- reactive({
    results <- currentresults()
    results
  })
  
  bugsresultsls <- reactive({
    results <- currentresults()
    results%>%
      ls()
  })
  
  
  # set parameters for food types and hazards which are used in the html file:
  nf_dl <- reactive({
    ocdata <- as.data.frame(ocdata())
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    nf <- length(foodnames)   # Calculate the number of foods
    
    nf
  })
  
  food_dl <- reactive({
    ocdata <- as.data.frame(ocdata())
    foodnames<-names(ocdata[4:dim(ocdata)[2]])
    foodnames
  })
  
  food_used_dl <- reactive({
    ocdata <- as.data.frame(ocdata())
    foodnamesused <- colnames(ocdata()[,4:ncol(ocdata())])
    foodnamesused
    
  })
  
  hazard_dl <- reactive({
    ocdata <- as.data.frame(ocdata())
    hazardnames <- ocdata$hazardnames
    hazardnames
  })
  
  
  ## MCMC-samples rds file download----
  output$MCMCsamples <- downloadHandler(
    filename = paste0("BIKE_MCMC-samples", format(Sys.time(), '%d.%m.%Y'), ".rds"),
    content = function(file) {
      results <- currentresults()
      saveRDS(results, file = file)
    }
  )  
  
  ## R-markdown----
  ### report_view html file----
  output$report_view <- downloadHandler(
    filename = paste0("BIKEreport_app-view", format(Sys.time(), '%d.%m.%Y'), ".html"), #"report1.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      if(input$datachoice!="FFQ"){
      tempReport <- file.path(tempdir(), "report.Rmd") # for desktop app
      reportfile <- "report_view_food_diary.Rmd"
      file.copy(reportfile, tempReport, overwrite = TRUE)
      } else
      if(input$datachoice=="FFQ"){  
      tempReport <- file.path(tempdir(), "report.Rmd") # for desktop app
      reportfile <- "report_view_FFQ.Rmd"
      file.copy(reportfile, tempReport, overwrite = TRUE)    
      }    
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      withProgress(message = 'Preparing the report in progress...',
                   value = 0.1, {
                     
                     bugsresults = bugsresults()
                     bugsresultsls = bugsresultsls()
                     plot1 = distPlot1_1_1()
                     plot2 = distPlot2_1_1()
                     plot3 = distPlot3_1_1()
                     plot4 = distPlot4_1_1()
                     plot51 = distPlot5_1_1()
                     plot52 = distPlot5_2_1()
                     plot6 = distPlot6_1_1()
                     plot7 = distPlot7_1_1()
                     resultProbs = resultProbs()
                     resultValues = resultValues()
                     
                     
                     params <- list(
                       data1 = data1(),
                       bugsresults = bugsresults,
                       bugsresultsls = bugsresultsls,
                       plot1 = plot1, 
                       plot2 = plot2,
                       plot3 = plot3,
                       plot4 = plot4,
                       plot51 = plot51,
                       plot52 = plot52,
                       plot6 = plot6,
                       plot7 = plot7,
                       resultValues = resultValues,
                       resultProbs = resultProbs,
                       modelchoice = model_parameters(),
                       factor = input$factor,
                       pfactor = input$pfactor,
                       thefoodnames1 = input$thefoodnames1,
                       thehazardnames1 = input$thehazardnames1,
                       thefoodnames2 = input$thefoodnames2,
                       thefoodnames21=input$thefoodnames21,
                       thefoodnames22=input$thefoodnames22,
                       thefoodnames3 = input$thefoodnames3,
                       thehazardnames3 = input$thehazardnames3,
                       thefoodnames4 = input$thefoodnames4,
                       thehazardnames4 = input$thehazardnames4,
                       thefoodnames5 = input$thefoodnames5,
                       thefoodnames52 = input$thefoodnames52,
                       thehazardnames5 = input$thehazardnames5,
                       conf_interval1 = input$conf_interval1,
                       conf_interval2 = input$conf_interval2,
                       conf_interval3 = input$conf_interval3,
                       selectscale1 = input$selectscale1,
                       selectscale2 = input$selectscale2,
                       selectscale3 = input$selectscale3,
                       selectscale4 = input$selectscale4,
                       selectdist1 = input$selectdist1,
                       selectdist2 = input$selectdist2,
                       selectdist3 = input$selectdist3,
                       concen = concen(),
                       consum = consum(),
                       ocdata = ocdata(),
                       prevdata = prevdata(), 
                       nf = nf_dl(),
                       food = food_dl(),
                       hazard = hazard_dl(),
                       nU = input$nU,
                       nV = input$nV,
                       input_selectQ = input$selectQ
                     )
                     
                     
                     # Set up parameters to pass to Rmd document
                     
                     rmarkdown::render(tempReport, output_file = file,
                                       params = params,
                                       envir = new.env(parent = globalenv())
                     )
                   })
      
      
    }
  )
  
  ### report_hazard html file----
  output$report_hazard <- downloadHandler(
    filename = function() {
      paste0("BIKEreport_", input$thehazardnames_dl, format(Sys.time(), '%d.%m.%Y'), ".html")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
    
      
      if(input$datachoice!="FFQ"){
      tempReport <- file.path(tempdir(), "report_food_diary.Rmd")
      reportfile <- "report_food_diary.Rmd"
      file.copy(reportfile, tempReport, overwrite = TRUE)  
      tempR <- file.path(tempdir(), "plotsfunctions.R")
      rfile <- "plotsfunctions.R"
      tempRtable <- file.path(tempdir(), "tablefunctions.R")
      rfiletable <- "tablefunctions.R"
      } else
      if(input$datachoice=="FFQ"){
      tempReport <- file.path(tempdir(), "report_FFQ.Rmd")
      reportfile <- "report_FFQ.Rmd"
      file.copy(reportfile, tempReport, overwrite = TRUE)   
      tempR <- file.path(tempdir(), "plotsfunctionsFFQ.R")
      rfile <- "plotsfunctionsFFQ.R"
      tempRtable <- file.path(tempdir(), "tablefunctionsFFQ.R")
      rfiletable <- "tablefunctionsFFQ.R"
      }
      file.copy(rfile, tempR, overwrite = TRUE)
      file.copy(rfiletable, tempRtable, overwrite = TRUE)
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      withProgress(message = 'Preparing the hazard report in progress...',
                   value = 0.1,
                   {
                     if(input$datachoice != "FFQ"){
                     params <- list(solvedBugs = solvedBugs(),
                                    data1 = data1(),
                                    bugsresults = bugsresults(),
                                    modelchoice = model_parameters(),
                                    factor = input$factor,
                                    pfactor = input$pfactor,
                                    conf_interval1 = input$conf_interval_dl,
                                    selectscale1 = input$selectscale_dl,
                                    selectdist1 = input$selectdist_dl,
                                    concen = concen(),
                                    consum = consum(),
                                    ocdata = ocdata(),
                                    prevdata = prevdata(),
                                    nf = nf_dl(),
                                    food = food_dl(),
                                    hazard = hazard_dl(),
                                    food_used = food_used_dl(),
                                    input_modelchoice = input$modelchoice,
                                    input_modelchoice2 = input$modelchoice2,
                                    input_modelchoice3 = input$modelchoice3,
                                    units_hazard = units_hazard(),
                                    units_food = units_food(),
                                    nU = input$nU_dl,
                                    nV = input$nV_dl,
                                    selectQ_dl = input$selectQ_dl,
                                    thehazardnames_dl = input$thehazardnames_dl
                     )
                     } else
                     if(input$datachoice == "FFQ"){
                       params <- list(solvedBugs = solvedBugs(),
                                      data1 = data1(),
                                      bugsresults = bugsresults(),
                                      modelchoice = model_parameters(),
                                      factor = input$factor,
                                      pfactor = input$pfactor,
                                      conf_interval1 = input$conf_interval_dl,
                                      selectscale1 = input$selectscale_dl,
                                      selectdist1 = input$selectdist_dl,
                                      concen = concen(),
                                      consum = consum(),
                                      ocdata = ocdata(),
                                      prevdata = prevdata(),
                                      nf = nf_dl(),
                                      food = food_dl(),
                                      hazard = hazard_dl(),
                                      food_used = food_used_dl(),
                                      input_modelchoice = input$modelchoice,
                                      input_modelchoice2 = input$modelchoice2,
                                      input_modelchoice3 = input$modelchoice3,
                                      units_hazard = units_hazard(),
                                      units_food = units_food(),
                                      nU = input$nU_dl,
                                      nV = input$nV_dl,
                                      selectQ_dl = input$selectQ_dl,
                                      thehazardnames_dl = input$thehazardnames_dl
                                      
                       )
                     }
                     rmarkdown::render(tempReport, output_file = file,
                                       params = params,
                                       envir = new.env(parent = globalenv())
                     )
                   })
    }
  )
  
  
  ## Model settings for download---- 
  model_parameters <- reactive({
    
    param_table <- matrix(data=NA,nrow = 7, ncol = 2)
    if(input$datachoice!="FFQ"){
    param_table[1,1] = paste("Consumption model")
    param_table[1,2] = input$modelchoice
    param_table[2,1] = paste("Between-user variability in consumption frequencies")
    param_table[2,2] = input$modelchoice2
    param_table[3,1] = paste("Correlation model of consumption frequencies")
    param_table[3,2] = input$modelchoice5
    param_table[4,1] = paste("Correlation model of serving sizes")
    param_table[4,2] = input$modelchoice3
    param_table[5,1] = paste("Correlation model of mean serving sizes")
    param_table[5,2] = input$modelchoice4
    param_table[6,1] = paste("Priors for variances")
    param_table[6,2] = input$priorchoice
    param_table[7,1] = paste("Number of MCMC iterations")
    param_table[7,2] = input$Iterations
    } else
    if(input$datachoice=="FFQ"){
      param_table[1,1] = paste("Consumption model")
      param_table[1,2] = "FFQ model"
      param_table[2,1] = paste("Between-user variability in consumption frequencies")
      param_table[2,2] = "Does not apply to FFQ data"
      param_table[3,1] = paste("Correlation model of consumption frequencies")
      param_table[3,2] = "Does not apply to FFQ data"
      param_table[4,1] = paste("Correlation model of serving sizes")
      param_table[4,2] = "Does not apply to FFQ data"
      param_table[5,1] = paste("Correlation model of mean serving sizes")
      param_table[5,2] = input$modelchoice4FFQ
      param_table[6,1] = paste("Priors for variances")
      param_table[6,2] = input$priorchoice
      param_table[7,1] = paste("Number of MCMC iterations")
      param_table[7,2] = input$Iterations
    }
    
    param_table
    
    
  })
  
  
  
  ## Example files for download----
  output$downloadConcenData <- downloadHandler(
    filename <- function() {
      paste("DataConcentrations", ".csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("files/DataConcentrations.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadConcenDatafish <- downloadHandler(
    filename <- function() {
      paste("DataConcentrations_fish", ".csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("files/DataConcentrations_fish.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadConsumData <- downloadHandler(
    filename <- function() {
      paste("DataConsumptions", ".csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("files/DataConsumptions.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadConsumDataFFQ <- downloadHandler(
    filename <- function() {
      paste("DataConsumptions_fish_FFQ", ".csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("files/DataConsumptions_fish_FFQ.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadOccData <- downloadHandler(
    filename <- function() {
      paste("DataOccurrence", ".csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("files/DataOccurrence.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadOccDatafish <- downloadHandler(
    filename <- function() {
      paste("DataOccurrence_fish", ".csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("files/DataOccurrence_fish.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$downloadPrevData <- downloadHandler(
    
    filename = function() {
      paste("DataPrevalence", ".csv", sep=".")
    },
    content = function(file) {
      file.copy("files/DataPrevalence.csv", file)
    },
    contentType = "application/zip"
  )
  
  output$downloadPrevDatafish <- downloadHandler(
    
    filename = function() {
      paste("DataPrevalence_fish", ".csv", sep=".")
    },
    content = function(file) {
      file.copy("files/DataPrevalence_fish.csv", file)
    },
    contentType = "application/zip"
  )
  
} 

# Run the application
shinyApp(ui = ui, server = server)
