# Module UI function
tablePanelUI <- function(id, choices, selected) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
      br(),
      DT::dataTableOutput(ns('data')),
      column(width = 12,
             br(),
             column(width = 6,
                    wellPanel(
                        checkboxGroupInput(ns("showCols"),
                                           label = "Columns to show in table:",
                                           choices = choices(),
                                           selected = selected,
                                           inline = TRUE)
                    )
             ),
             column(width = 6,
                    wellPanel(
                        h4("Download the csv file containing only the filtered events:"),
                        checkboxInput(ns("header"), "Header", TRUE),
                        downloadButton(ns("downloadData"), "Download Data")
                    )
             )
        )
    )
}

# Module server function
tablePanel <- function(input, output, session, nameData, dataInput) {
    
    # Keep only the selected columns
    dataToShow <- reactive({
        dataInput()[, input$showCols]
    })
    
    # display the table containing the data
    output$data <- DT::renderDataTable(
        DT::datatable(dataToShow(),
                      rownames = FALSE, 
                      filter = "top",
                      extensions = 'Buttons', 
                      options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'print'),
                          pageLength = 15,
                          scrollX = TRUE
                      )
        )
    )
    
    # download csv of the filtered dataset
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(nameData(), "_filtered.csv", sep = "")
        },
        content = function(file) {
            write.table(dataToShow(), file, sep = ",",
                        col.names= input$header, row.names = FALSE)
        }
    )
}