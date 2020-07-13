# Module UI function
plotPanelUI <- function(id, xchoices, xselected, ychoices, yselected, 
                        colorChoices, colorSelectedVar, colorSelectedCond, colorSelectedVal) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    br(),
    plotOutput(ns('plot'),
                 click = ns("plot_click"),
                 brush = brushOpts(
                           id = ns("plot_brush")
                        )
    ),
    column(width = 12,
           br(),
           DT::dataTableOutput(ns('selectedPoints'))
    ),
    column(width = 12,
           br(),
           column(width = 6,
                  wellPanel(
                      h4("Choose the variables to plot."),
                      fluidRow(
                          column(width = 6,
                                 selectInput(ns("xplot"), "variable x :",
                                             choices = xchoices,
                                             selected = xselected)
                          ),
                          column(width = 6,
                                 selectInput(ns("yplot"), "variable y :",
                                             choices = ychoices,
                                             selected = yselected)
                          )
                      ),
                      h4("Choose scales:"),
                      uiOutput(ns("uixAxisLim")),
                      uiOutput(ns("uiyAxisLim")),
                      checkboxInput(ns("xscale"), "x axis scale in log10", value = FALSE),
                      checkboxInput(ns("yscale"), "y axis scale in log10", value = FALSE),
                      h4("Formating parameters:"),
                      textInput(ns("plottitle"), "Plot title:", value = ""),
                      textInput(ns("xtitle"), "Label for x axis:", value = ""),
                      textInput(ns("ytitle"), "Label for y axis:", value = ""),
                      sliderInput(ns("sizelabel"), 
                                  "Choose text size on the plot:", 
                                  min = 4,
                                  max = 30,
                                  value = 10),
                      checkboxInput(ns("coloring"), "Select to allow coloring of points following a condition", value = FALSE),
                      conditionalPanel("input.coloring", ns = ns,
                           fluidRow(
                               column(width = 6,
                                      colourInput(ns("selectedColor"), "Select color",
                                                  value = "red", 
                                                  showColour = "background",
                                                  palette = "limited"),
                                      radioButtons(ns("selectedCond"), "Select condition", 
                                                   choices = c("<", ">", "<=", ">=", "=", "!="), 
                                                   selected = colorSelectedCond, inline = TRUE)
                               ),
                               column(width = 6,
                                      selectInput(ns("selectedVar"), "Select variable", 
                                                  choices = colorChoices, 
                                                  selected = colorSelectedVar),
                                      numericInput(ns("selectedVal"), "Select value",
                                                  value = colorSelectedVal)
                               )
                           )
                      )
                  )
           ),
           column(width = 6,
                  wellPanel(
                      h4("Download the plot:"),
                      # Choose the size of the plot and the format
                      sliderInput(ns("plotWidth"),
                                  label = "Width of the plot:",
                                  min = 7,
                                  max = 20,
                                  value = 10),
                      sliderInput(ns("plotHeight"),
                                  label = "Height of the plot:",
                                  min = 7,
                                  max = 20,
                                  value = 10),
                      selectInput(ns("plotFormat"), "Choose image format :",
                                  choices = c("png", "pdf", "jpeg"),
                                  selected = "png"),
                      downloadButton(ns('downloadPlot'), 'Download Plot')
                  )
           )
    )
  )
}

# Module server function
plotPanel<- function(input, output, session, nameData, dataInput, selectedCol) {
    
    # ui part to choose the axis limit of the plot
    output$uixAxisLim <- renderUI({
        ns <- session$ns
        x <- completeData()[, input$xplot]
        if (!is.factor(x)){
            fluidRow(
                column(width = 6,
                       numericInput(ns("xmin"), label = "Minimum value on x axis:",
                                    value = isolate(floor(min(x))))
                ),
                column(width = 6,
                       numericInput(ns("xmax"), label = "Maximum value on x axis:",
                                    value = isolate(ceiling(max(x))))
                )
            )
        }
    })
    output$uiyAxisLim <- renderUI({
        ns <- session$ns
        y <- completeData()[, input$yplot]
        if (!is.factor(y)){
            fluidRow(
                column(width = 6,
                       numericInput(ns("ymin"), label = "Minimum value on y axis:",
                                    value = isolate(floor(min(y))))
                ),
                column(width = 6,
                       numericInput(ns("ymax"), label = "Maximum value on y axis:",
                                    value = isolate(ceiling(max(y))))
                )
            )
        }
    })
    
    # for the plot, keep only the line that are not equal to NA for x and y
    completeData <- reactive({
        # if (input$coloring){
        #     dataInput()[(!is.na(dataInput()[, input$xplot]) & 
        #                      !is.na(dataInput()[, input$yplot]) & 
        #                      dataInput()[, input$colorSelectedVar]), ] # TODO
        # } else{
            dataInput()[(!is.na(dataInput()[, input$xplot]) & !is.na(dataInput()[, input$yplot])), ]
        # }
    })
    
    # select only the line to color
    dataToColor <- reactive({
        if (input$coloring){
            selectedData <- switch(input$selectedCond,
                ">" = completeData()[which(completeData()[, input$selectedVar] > input$selectedVal), ],
                "<" = completeData()[which(completeData()[, input$selectedVar] < input$selectedVal), ],
                ">=" = completeData()[which(completeData()[, input$selectedVar] >= input$selectedVal), ],
                "<=" = completeData()[which(completeData()[, input$selectedVar] <= input$selectedVal), ],
                "=" = completeData()[which(completeData()[, input$selectedVar] == input$selectedVal), ],
                "!=" = completeData()[which(completeData()[, input$selectedVar] != input$selectedVal), ])
        } else{
            cat(file=stderr(), "no selected data", "\n")
            selectedData <- NULL
        }
        return(selectedData)
    })
    
    # render plot
    plotGraph <- reactive({
        x <- completeData()[, input$xplot]
        y <- completeData()[, input$yplot]
        p <- ggplot(completeData(), aes(x=x, y=y))
        if (is.factor(x)){
            p <- p + geom_boxplot(aes(fill=x))
            p <- p + labs(fill=input$xplot)
            p <- p + coord_cartesian(ylim = c(input$ymin, input$ymax))
        } else if (is.factor(y)){
            p <- p + geom_point()
            p <- p + coord_cartesian(xlim = c(input$xmin, input$xmax))
        } else{
            p <- p + geom_point()
            p <- p + coord_cartesian(xlim = c(input$xmin, input$xmax),
                                     ylim = c(input$ymin, input$ymax))
        }
        if (input$xscale){
            p <- p + scale_x_log10()
        }
        if (input$yscale){
            p <- p + scale_y_log10()
        }
        if (input$xtitle != ""){
            p <- p + xlab(input$xtitle)
        } else{
            p <- p + xlab(input$xplot)
        }
        if (input$ytitle != ""){
            p <- p + ylab(input$ytitle)
        } else{
            p <- p + ylab(input$yplot)
        }
        if (input$plottitle != ""){
            p <- p + ggtitle(input$plottitle)
        }
        if (input$coloring){
            xdataToColor <- dataToColor()[, input$xplot]
            ydataToColor <- dataToColor()[, input$yplot]
            p <- p + geom_point(data = dataToColor(), aes(x=xdataToColor, y=ydataToColor), color = input$selectedColor)
        }
        p <- p + theme(axis.text = element_text(size=input$sizelabel - 4),
                       axis.title = element_text(size=input$sizelabel),
                       plot.title = element_text(hjust=0.5, size=input$sizelabel + 4, face="bold"))
        p
    })
    
    output$plot <- renderPlot({
        plotGraph()
    })
    
    output$selectedPoints <- DT::renderDataTable({
        dataToUse <- completeData()[, selectedCol]
        colNames <- colnames(dataToUse)
        dataToShow <- data.frame(matrix(ncol = length(colNames), nrow = 0))
        colnames(dataToShow) <- colNames
        if (!is.null(input$plot_brush)){
            dataToShowBrush <- brushedPoints(dataToUse, input$plot_brush, input$xplot, input$yplot)
            dataToShow <- dataToShowBrush
        }
        if (!is.null(input$plot_click)){
            dataToShowClick <- nearPoints(dataToUse, input$plot_click, input$xplot, input$yplot)
            dataToShow <- rbind(dataToShow, dataToShowClick)
        }
        DT::datatable(dataToShow, 
                      rownames = FALSE,
                      extensions = 'Buttons',
                      options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'print'),
                          pageLength = 15,
                          scrollX = TRUE
                      )
        )
    })
    
    # download plot
    output$downloadPlot <- downloadHandler(
        filename = function() {
            paste(paste(paste(nameData(), input$yplot, sep="_"), input$xplot, sep="-"), input$plotFormat, sep=".")
        },
        content = function(file) {
            ggsave(file, plot = plotGraph(), width = input$plotWidth, height = input$plotHeight, device = input$plotFormat)
        }
    )
}
