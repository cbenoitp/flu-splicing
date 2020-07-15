# Module UI function
pcaPlotPanelUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
      br(),
      fluidRow(
          column(width = 7,
                 withSpinner(plotOutput(ns('PCA'),
                                        click = ns("plotClickPCA"),
                                        brush = brushOpts(
                                            id = ns("plotBrushPCA")
                                        )
                            )
                 )
          ),
          column(width = 5,
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
          ),
          style = "height:600px;"
      ),
      fluidRow(
          column(width = 12,
                 br(),
                 DT::dataTableOutput(ns('selectedPointsPCA'))
          )
      )
  )
}


# Module server function
pcaPlotPanel<- function(input, output, session, typeData, dataInput){
    # plot PCA
    output$PCA <- renderPlot({
        # prepare data to plot
        dataPCA <- dataInput()
        dataPCAToPlot <- dataPCA$dataPCAplot
        # prepare names of the axis
        pcaVar <- dataPCA$pcaVar
        # prepare the color vector
        colorsLab <- c("Untreated_mock" = "#70a9c8", "Untreated_virus" = "#1171a4", "siControl_mock" = "#82c082", 
                       "siControl_virus" = "#78b4a8",  "siRED_mock" = "#2f972f", "siRED_virus" = "#1f826f")
        colorsLab <- colorsLab[which(names(colorsLab) %in% dataPCAToPlot$condition)]
        # prepare the shape vector
        pchsLab <- c("Untreated_mock" = 2, "Untreated_virus" = 1, "siControl_mock" = 5, 
                     "siControl_virus" = 8,  "siRED_mock" = 0, "siRED_virus" = 4)
        pchsLab <- pchsLab[which(names(pchsLab) %in% dataPCAToPlot$condition)]
        # do the plot
        p <- ggplot(data = dataPCAToPlot, 
                    aes(x = Axis1, y = Axis2, color = condition, shape = condition))
        p <- p + geom_point(size = 5, stroke = 2)
        p <- p + theme_bw(base_size = 20)
        p <- p + labs(x = paste0("PCA 1 = ", round(pcaVar[1]*100, 0), "%", sep=""), 
                      y = paste0("PCA 2 = ", round(pcaVar[2]*100, 0), "%", sep=""))
        p <- p + scale_color_manual(values = colorsLab, name = "Condition")
        p <- p + scale_shape_manual(values = pchsLab, name = "Condition")
        #p <- p + geom_hline(yintercept = 0)
        #p <- p + geom_vline(xintercept = 0)
        return(p)
    },
    width = 800,
    height = 600)
    
    # display the table containing the data of the PCA
    output$selectedPointsPCA <- DT::renderDataTable({
      # prepare data to plot
      dataPCA <- dataInput()
      dataToUse <- dataPCA$dataPCAplot
      colNames <- colnames(dataToUse)
      dataToShow <- data.frame(matrix(ncol = length(colNames), nrow = 0))
      names(dataToShow) <- colNames
      if (!is.null(input$plotBrushPCA)){
        dataToShowBrush <- brushedPoints(dataToUse, input$plotBrushPCA)
        dataToShow <- dataToShowBrush[-1]
      }
      if (!is.null(input$plotClickPCA)){
        dataToShowClick <- nearPoints(dataToUse, input$plotClickPCA)
        dataToShow <- rbind(dataToShow, dataToShowClick[-1])
      }
      DT::datatable(dataToShow, rownames = FALSE,
                    options = list(scrollX = TRUE),
                    caption = 'Select points on the plot to show them in this table.')
    })
}
