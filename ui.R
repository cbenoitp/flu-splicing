library(shiny)

##### Define UI #####
tagList(
    navbarPage("",
        # Welcome page
        tabPanel("FluHit",
                   fluidRow(
                       column(width=8, offset = 2,
                              div(h1("Welcome to the FluHit data exploration Shiny web interface"), br(),
                                  div(style= "text-align:justify", p("ABSTRACT"),
                                      p("This Shiny Interface allow to explore the results presented in XXX (2020) paper. Complete results of the different transcriptomic analysis are available: mutltivariate analysis (PCA), alternative splicing analysis with KisSplice, differential expression with DESeq2 and readthrough analysis.")),
                                  br(), hr(), br(),
                                  align = "center"
                              ),
                              h5(strong("Description of the data")),
                              p("The dataset is comprised of 6 conditions (2 infection conditions x 3 transfection conditions) with 4 biological replicates each (total of 24 samples)."),
                              p("FIGURE"),
                              br(), hr(), br(),
                              h5(strong("Author")),
                              p("Clara Benoit-Pilven - clara.benoit-pilven@helsinki.fi - Institute for Molecular Medicine – Helsinki - Finland"),
                              br(),
                              h5(strong("Citation")),
                              p("Manuscript in preparation"),
                              br(), hr(), br()
                       ),
                       column(width=12,
                              div(img(src = "LBBE.png", height = 75, hspace = 5), 
                                  img(src = "Lyon1.jpg", height = 70, hspace = 5), 
                                  img(src = "INRIA.jpg", height = 65, hspace = 5), 
                                  img(src = "CNRS.jpg", height = 65, hspace = 5), 
                                  img(src = "Pasteur.png", height = 70, hspace = 5),
                                  align = "center"
                              )
                       )
                   )
        ),
        
        # 
        tabPanel("Multivariate analysis",
                 fluidRow(
                     column(width=12,
                            tags$div(h3("Principal component analysis (PCA)"),
                                     "Plots presented here are results from PCA analysis done with ",
                                     a("ade4", href="http://pbil.univ-lyon1.fr/ade4/", target="_blank"),
                                     "R package."
                            ),
                            
                            tags$hr(),
                            
                            helpText("The first step is to choose a dataset."),
                            
                            # Input: Select a dataset
                            column(width=3,
                                   wellPanel(radioButtons("DataTypePCA", "Choose the type of data to use for the PCA:",
                                                          choices = c("expression", "splicing", "readthrough"),
                                                          selected = "expression",
                                                          inline = FALSE
                                   ),
                                   # this panel only appear if the PCA have to be done on splicing events
                                   conditionalPanel("input.DataTypePCA == 'splicing'",
                                                    radioButtons("ASEtypePCA", "Choose the type of splicing event to use for the PCA:",
                                                                 choices = c("All", "ES", "ES_MULTI", "IR", "altA", "altD"),
                                                                 selected = "All",
                                                                 inline = TRUE
                                                    )
                                   )
                                   )
                            ),
                            column(width=3,
                                   wellPanel(selectInput("ComparisonPCA", "Choose a comparison:",
                                                         choices = c(Choose = '', listPossibleComparisons("input.DataTypePCA"))
                                   )
                                   )
                            ),
                            column(width=3,
                                   h4("Description of the chosen comparison:"),
                                   textOutput("descriptionPCA")
                            ),
                            column(width=2,
                                   actionButton("updateDataPCA", "Update dataset",
                                                icon("refresh"))
                            )
                     )
                 ),
                 fluidRow(
                     column(width=12,
                            uiOutput("uiPCAdata")
                     )
                 )
        ),
        
        # Splicing analysis
        tabPanel("Splicing analysis",
                 fluidRow(
                     column(width=12,
                            tags$div(h3("Alternative splicing analysis with KisSplice"),
                                     "Table and plots presented here are results from ",
                                     a("KisSplice", href="http://kissplice.prabi.fr/", target="_blank"),
                                     "analysis."
                            ),
                            
                            tags$hr(),
                            
                            helpText("The first step is to choose a dataset.")
                     ),
                     
                     # Input: Select a dataset
                     column(width=3,
                            wellPanel(selectInput("dataset", "Choose a dataset:",
                                                  choices = c(Choose = '',
                                                              "U virus VS mock", "C virus VS mock", "R virus VS mock",
                                                              "UC virus VS mock", "UCR virus VS mock", "mock R VS C",
                                                              "virus R VS C")
                                    ),
                                    
                                    radioButtons("nbrData", "Number of dataset to analyze:",
                                                 choices = c("1 dataset", "2 datasets"),
                                                 selected = "1 dataset",
                                                 inline = TRUE)
                            )
                     ),
                     
                     column(width=5,
                            h4("Description of the chosen dataset :"),
                            htmlOutput("description")
                     ),
                     
                     column(width=3, offset=1,
                            actionButton("updateData", "Update dataset",
                                         icon("refresh"))
                     )
                 ),
                 
                 fluidRow(
                     conditionalPanel("input.nbrData == '2 datasets'",
                         # Input: Select a second dataset
                         column(width=3,
                                wellPanel(helpText("By choosing a second dataset, you can compare the results of 2 analysis."),
                                        
                                        selectInput("dataset2", "Choose a second dataset:",
                                                    choices = c(Choose = '', vectorDataset2("input.dataset"))
                                        )
                                )
                         ),
                         
                         column(width=5,
                                h4("Description of the second dataset :"),
                                htmlOutput("description2")
                         )
                     )
                 ),
                 
                 fluidRow(
                     column(width=12,
                            uiOutput("uiKSData")
                     )
                 )
        ),
        
        # Expression analysis
        tabPanel("Expression analysis",
                 fluidRow(
                     column(width=12,
                            tags$div(h3("Expression analysis"),
                                     "Table and plots presented here are results from ",
                                     a("DESeq2", href="https://bioconductor.org/packages/release/bioc/html/DESeq2.html", target="_blank"),
                                     "analysis."
                            ),
                            
                            tags$hr(),
                            
                            helpText("The first step is to choose a dataset.")
                     ),
                     
                     # Input: Select a dataset
                     column(width=3,
                            wellPanel(selectInput("datasetExpr", "Choose a dataset:",
                                                  choices = c(Choose = '', 
                                                              "U virus VS mock", "C virus VS mock", "R virus VS mock",
                                                              "UC virus VS mock", "UCR virus VS mock", "mock R VS C",
                                                              "virus R VS C")
                                    )
                            )
                     ),
                     
                     column(width=3,
                            h4("Description of the chosen dataset :"),
                            htmlOutput("descriptionExpr")
                     ),
                     column(width=4,
                            verbatimTextOutput("summaryDESeq", placeholder = FALSE)
                     ),
                     
                     column(width=2,
                            actionButton("updateDataExpr", "Update dataset",
                                         icon("refresh"))
                     )
                 ),
                 
                 fluidRow(
                     column(width=12,
                            uiOutput("uiExprData")
                     )
                 )
        ),
        
        # Readthrough analysis
        tabPanel("Readthrough analysis",
                 fluidRow(
                     column(width=12,
                            tags$div(h3("Readthrough analysis"),
                                     "Table and plots presented here are results from the readthrough (RT) analysis.",
                                     "Only the comparison U virus VS mock was analysed for the readthrough."
                            ),
                            
                            tags$hr()
                            
                     ),
                     
                     # Show data
                     column(width=2,
                            actionButton("updateDataRT", "Show data",
                                         icon("refresh"))
                     )
                 ),
                 
                 fluidRow(
                     column(width=12,
                            uiOutput("uiRTData")
                     )
                 )
        )
    )
)