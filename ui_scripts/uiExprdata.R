# if no dataset is chosen, show nothing
if (isDatasetExprDefined()){
    return()
} else{
    return(
        # Sidebar with the different filters
        sidebarLayout(position = "right",
                      sidebarPanel(h2("Filters"),
                                   width = 3,
                                   helpText("Choose the threshold for each filter."),
                                   
                                   sliderInput("baseMeanExpr",
                                               label = "Threshold minimum baseMean value:",
                                               min = 0,
                                               max = 500,
                                               value = 10),
                                   
                                   sliderInput("C1fpkmExpr",
                                               label = "Threshold minimum FPKM value of first condition (C1):",
                                               min = 0,
                                               max = 100,
                                               value = 1),
                                   sliderInput("C2fpkmExpr",
                                               label = "Threshold minimum FPKM value of second condition (C2):",
                                               min = 0,
                                               max = 100,
                                               value = 1),
                                   
                                   sliderInput("l2fcExpr",
                                               label = "Threshold absolute log2 fold change value:",
                                               min = 0,
                                               max = ceiling(max(abs(datasetExprInput()[,'log2FoldChange']), na.rm=TRUE)),
                                               value = 1,
                                               step = 0.1),
                                   
                                   numericInput("padjExpr",
                                                label = "Threshold adjusted p-value:",
                                                value = 0.05,
                                                step = 0.01),
                                   helpText("Adjusted p-value should be a numeric between 0 and 1."),
                                   
                                   br(),
                                   
                                   h4("Other filters:"),
                                   
                                   checkboxInput("splicingFactorExpr", "Select only the genes from the splicing factor list.", FALSE),
                                   
                                   checkboxInput("filterORfpkmExpr", "Apply/Remove filter on FPKM of first (C1) or second (C2) condition", FALSE),
                                   conditionalPanel("input.filterORfpkmExpr",
                                                    sliderInput("ORfpkmExpr",
                                                                label = "Threshold minimum FPKM value of first (C1) or second (C2) condition:",
                                                                min = 0,
                                                                max = 100,
                                                                value = 1),
                                                    helpText("To apply this filter correctly, the filters on the FPKM of each condition separatly should be set to 0.")
                                   ),
                                   
                                   tags$b("Filters on VirHostNet data"),
                                   
                                   checkboxInput("filterVDegreeExpr", "Apply/Remove filter on number of viral partner", FALSE),
                                   conditionalPanel("input.filterVDegreeExpr",
                                                    sliderInput("vDegreeExpr",
                                                                label = "Minimum number of viral partner:",
                                                                min = 0,
                                                                max = 70,
                                                                value = 1,
                                                                step = 1)
                                   ),
                                   checkboxInput("filterHDegreeExpr", "Apply/Remove filter on number of human partner", FALSE),
                                   conditionalPanel("input.filterHDegreeExpr",
                                                    sliderInput("hDegreeExpr",
                                                                label = "Minimum number of human partner:",
                                                                min = 0,
                                                                max = 100,
                                                                value = 1,
                                                                step = 1)
                                   )
                      ),
                      mainPanel(width = 9,
                                tabsetPanel(id = "tabExpr",
                                            tabPanel('Dataset',
                                                     withSpinner(
                                                     tablePanelUI('showDatasetExpr',
                                                                  choices = reactive({names(datasetExprInput())}),
                                                                  selected = c("Gene_Name", "C1_fpkm", "C2_fpkm",
                                                                               "baseMean", "log2FoldChange", "padj")
                                                     ))
                                            ),
                                            
                                            tabPanel('Plot',
                                                     withSpinner(
                                                     plotPanelUI('plotDatasetExpr',
                                                                 xchoices = c("baseMean", "log2FoldChange", "padj",
                                                                              "C1_fpkm", "C2_fpkm"),
                                                                 xselected = "padj",
                                                                 ychoices = c("baseMean", "log2FoldChange", "padj",
                                                                              "C1_fpkm", "C2_fpkm"),
                                                                 yselected = "log2FoldChange",
                                                                 colorChoices = c("baseMean", "log2FoldChange", "padj",
                                                                                  "C1_fpkm", "C2_fpkm"),
                                                                 colorSelectedVar = "baseMean",
                                                                 colorSelectedCond = "=",
                                                                 colorSelectedVal = 0
                                                     ))
                                            )
                                )
                      )
        )
    )
}
        