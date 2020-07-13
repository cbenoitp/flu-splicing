# Sidebar with the different filters
sidebarLayout(position = "right",
              sidebarPanel(h2("Filters"),
                           width = 3,
                           helpText("Choose the threshold for each filter."),
                           
                           br(),
                           
                           numericInput("dRatioRT",
                                        label = "Threshold absolute value of delta ratio:",
                                        value = 0.025,
                                        step = 0.005),
                           helpText("Delta ratio should be a numeric between 0 and 1."),
                           
                           numericInput("padjRT",
                                        label = "Threshold adjusted p-value:",
                                        value = 0.05,
                                        step = 0.01),
                           helpText("Adjusted p-value should be a numeric between 0 and 1."),
                           
                           h4("Other filters:"),
                           
                           checkboxInput("splicingFactorRT", "Select only the genes from the splicing factor list.", FALSE),
                           
                           checkboxInput("filterl2fc_RT", "Apply/Remove filter on log2 fold change for the readthrough", FALSE),
                           conditionalPanel("input.filterl2fc_RT",
                                            sliderInput("l2fc_RT",
                                                        label = "Threshold minimum log2 fold change value for the readthrough:",
                                                        min = 0,
                                                        max = 15,
                                                        value = 1)
                           ),
                           
                           tags$b("Filters on expression data"),
                           
                           checkboxInput("filterbaseMean_RT", "Apply/Remove filter on baseMean value", FALSE),
                           conditionalPanel("input.filterbaseMean_RT",
                                            sliderInput("baseMean_RT",
                                                        label = "Threshold minimum baseMean value:",
                                                        min = 0,
                                                        max = 500,
                                                        value = 10)
                           ),
                           checkboxInput("filterORfpkm_RT", "Apply/Remove filter on FPKM of mock or virus condition", FALSE),
                           conditionalPanel("input.filterORfpkm_RT",
                                            sliderInput("ORfpkm_RT",
                                                        label = "Threshold minimum FPKM value of mock or virus condition:",
                                                        min = 0,
                                                        max = 100,
                                                        value = 1)
                           ),
                           checkboxInput("filterl2fcDESeq_RT", "Apply/Remove filter on log2 fold change value", FALSE),
                           conditionalPanel("input.filterl2fcDESeq_RT",
                                            sliderInput("l2fcDESeq_RT",
                                                        label = "Range absolute log2 fold change value:",
                                                        min = 0,
                                                        max = ceiling(max(abs(datasetRTInput()[,'log2FoldChange']), na.rm=TRUE)),
                                                        value = c(1,3),
                                                        step = 0.1)
                           ),
                           checkboxInput("filterpadjDESeq_RT", "Apply/Remove filter on DESeq2 adjusted p-value", FALSE),
                           conditionalPanel("input.filterpadjDESeq_RT",
                                            numericInput("padjDESeq_RT",
                                                         label = "Threshold minimum DESeq2 adjusted p-value:",
                                                         value = 0.05,
                                                         step = 0.01),
                                            helpText("Adjusted p-value should be a numeric between 0 and 1.")
                           ),
                           
                           tags$b("Filters on VirHostNet data"),
                           
                           checkboxInput("filterVDegree_RT", "Apply/Remove filter on number of viral partner", FALSE),
                           conditionalPanel("input.filterVDegree_RT",
                                            sliderInput("vDegree_RT",
                                                        label = "Minimum number of viral partner:",
                                                        min = 0,
                                                        max = 70,
                                                        value = 1,
                                                        step = 1)
                           ),
                           checkboxInput("filterHDegree_RT", "Apply/Remove filter on number of human partner", FALSE),
                           conditionalPanel("input.filterHDegree_RT",
                                            sliderInput("hDegree_RT",
                                                        label = "Minimum number of human partner:",
                                                        min = 0,
                                                        max = 100,
                                                        value = 1,
                                                        step = 1)
                           )
              ),
              
              mainPanel(width = 9,
                        tabsetPanel(id = "tabRT",
                                    tabPanel('Dataset',
                                             withSpinner(
                                                 tablePanelUI('showDatasetRT',
                                                              choices = reactive({names(datasetRTInput())}),
                                                              selected = c("Gene_Name", "RT_log2FoldChange", "RT_padj", "delta_ratio")
                                                 ))
                                    ),
                                    
                                    tabPanel('Plot',
                                             withSpinner(
                                                 plotPanelUI('plotDatasetRT',
                                                             xchoices = c("log2FoldChange", "padj", "U_mock_fpkm", "U_virus_fpkm",
                                                                          "RT_log2FoldChange", "RT_padj", "U_mock_RT_fpkm", "U_virus_RT_fpkm",
                                                                          "U_mock_ratio", "U_virus_ratio", "delta_ratio"),
                                                             xselected = "RT_padj",
                                                             ychoices = c("log2FoldChange", "padj", "U_mock_fpkm", "U_virus_fpkm",
                                                                          "RT_log2FoldChange", "RT_padj", "U_mock_RT_fpkm", "U_virus_RT_fpkm",
                                                                          "U_mock_ratio", "U_virus_ratio", "delta_ratio"),
                                                             yselected = "delta_ratio",
                                                             colorChoices = c("log2FoldChange", "padj", "U_mock_fpkm", "U_virus_fpkm",
                                                                              "RT_log2FoldChange", "RT_padj", "U_mock_RT_fpkm", "U_virus_RT_fpkm",
                                                                              "U_mock_ratio", "U_virus_ratio", "delta_ratio"),
                                                             colorSelectedVar = "RT_log2FoldChange",
                                                             colorSelectedCond = "=",
                                                             colorSelectedVal = 0
                                                 ))
                                    )
                        )
              )
)