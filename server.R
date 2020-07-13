function(input, output, session) {
    
    ####### UI functions #######
    
    output$uiPCAdata <- renderUI({
        source("ui_scripts/uiPCAdata.R", local=TRUE)$value
    })
    
    output$uiKSData <- renderUI({
        source("ui_scripts/uiKSdata.R", local=TRUE)$value
    })
    
    output$uiExprData <- renderUI({
        source("ui_scripts/uiExprdata.R", local=TRUE)$value
    })
    
    output$uiRTData <- renderUI({
        source("ui_scripts/uiRTdata.R", local=TRUE)$value
    })
    
    ####### observe functions #######
    
    # update the choice of the comparison in the PCA tab
    observe({
        updateSelectInput(session, "ComparisonPCA",
                          choices = c(Choose = '', listPossibleComparisons(input$DataTypePCA))
        )
    })
    
    # update the possible dataset2 that can be choosen by the user
    # (can't be the same as the first dataset)
    observe({
        updateSelectInput(session, "dataset2",
          choices = c(Choose = '', vectorDataset2(input$dataset))
        )
        if (input$nbrData == "1 dataset"){
            datasetInput()
        }
    })
    
    # select or deselect all by clicking on the selectall
    observeEvent(input$selectAllType, {
        if (input$selectAllType%%2 == 0){
            updateCheckboxGroupInput(session, "eventType", 
                                     "Choose the type of splicing events:", 
                                     choices = listTypeEvents(datasetInput()),
                                     selected = listTypeEvents(datasetInput()))
            updateActionButton(session, "selectAllType","Deselect All")
        } else{
            updateCheckboxGroupInput(session, "eventType",
                                     "Choose the type of splicing events:",
                                     choices=listTypeEvents(datasetInput()))
            updateActionButton(session, "selectAllType","Select All")
        }
    })
       
    ####### functions to check if the dataset are defined #######
    
    # check if PCA dataset is defined
    isDatasetPCADefined <- eventReactive(input$updateDataPCA, {
        input$ComparisonPCA != ''
    }, ignoreNULL=FALSE)
    
    # check if dataset is defined
    isDatasetDefined <- eventReactive(input$updateData, {
        input$dataset == ''
    }, ignoreNULL=FALSE)
    
    # check if dataset2 is defined
    isDataset2Defined <- eventReactive(input$updateData, {
        input$dataset2 == ''
    }, ignoreNULL=FALSE)
    
    # check if expression dataset is defined
    isDatasetExprDefined <- eventReactive(input$updateDataExpr, {
        input$datasetExpr == ''
    }, ignoreNULL=FALSE)
    
    ####### functions to load datasets #######
    
    # load the chosen PCA results
    datasetPCAInput <- eventReactive(input$updateDataPCA, {
        chosePCAData(input$DataTypePCA, input$ComparisonPCA, input$ASEtypePCA)
    })
    # and keep in memory the chosen values
    conditionsPCA <- reactiveValues()
    conditionsPCA <- eventReactive(input$updateDataPCA, {
        reactiveValues(dataType = input$DataTypePCA,
                       comparison = input$ComparisonPCA,
                       ASEtype = input$ASEtypePCA)
    }, ignoreNULL=FALSE)
    
    # load the chosen dataset
    datasetInput <- eventReactive(input$updateData, {
        choseASData(input$dataset)
    })
    
    # load the chosen dataset if 2 datasets are selected
    dataset2Input <- eventReactive(input$updateData, {
        if (input$dataset2 != ''){
            mergeASData(choseASData(input$dataset), choseASData(input$dataset2))
        }
    })
    
    # load the chosen expression dataset
    datasetExprInput <- eventReactive(input$updateDataExpr,{
        choseExprData(input$datasetExpr)
    })
    
    # load the RT dataset
    datasetRTInput <- eventReactive(input$updateDataRT,{
        choseRTData()
    })
    
    ####### functions to filter datasets #######
    
    # filter the data with the threshold set by the user
    dataInput <- reactive({
        # initialize the filter in function of the parameters chosen by the user
        if (!input$filterbaseMean){
            filterBaseMean = NULL
        } else{
            filterBaseMean = input$baseMean
        }
        if (!input$filterC1fpkm){
            filterC1fpkm = NULL
        } else{
            filterC1fpkm = input$C1fpkm
        }
        if (!input$filterC2fpkm){
            filterC2fpkm = NULL
        } else{
            filterC2fpkm = input$C2fpkm
        }
        if (!input$filterORfpkm){
            filterORfpkm = NULL
        } else{
            filterORfpkm = input$ORfpkm
        }
        if (!input$filterC1LocalExpr){
            filterC1LocalExpr <- NULL
        } else{
            filterC1LocalExpr <- input$C1LocalExpr
        }
        if (!input$filterC2LocalExpr){
            filterC2LocalExpr <- NULL
        } else{
            filterC2LocalExpr <- input$C2LocalExpr
        }
        if (!input$filterORLocalExpr){
            filterORLocalExpr <- NULL
        } else{
            filterORLocalExpr <- input$ORLocalExpr
        }
        if (!input$filterC1PSI){
            filterC1PSI <- NULL
        } else{
            filterC1PSI <- input$C1PSI
        }
        if (!input$filterC2PSI){
            filterC2PSI <- NULL
        } else{
            filterC2PSI <- input$C2PSI
        }
        if (!input$filterl2fc){
            filterl2fc_min = NULL
            filterl2fc_max = NULL
        } else{
            filterl2fc_min = input$l2fc[1]
            filterl2fc_max = input$l2fc[2]
        }
        if (!input$filterpadjDESeq){
            filterpadjDESeq = NULL
        } else{
            filterpadjDESeq = input$padjDESeq
        }
        if (!input$filterVDegree){
            filterVDegree = NULL
        } else{
            filterVDegree = input$vDegree
        }
        if (!input$filterHDegree){
            filterHDegree = NULL
        } else{
            filterHDegree = input$hDegree
        }
        # apply filters
        filterASData(datasetInput(),
                     input$deltaPSI, 
                     input$padj, 
                     input$eventType,
                     input$splicingFactor,
                     filterBaseMean,
                     filterC1fpkm,
                     filterC2fpkm,
                     filterORfpkm,
                     filterC1LocalExpr,
                     filterC2LocalExpr,
                     filterORLocalExpr,
                     filterC1PSI,
                     filterC2PSI,
                     filterl2fc_min,
                     filterl2fc_max,
                     filterpadjDESeq,
                     filterVDegree,
                     filterHDegree)
    })
    
    # filter the data2 with the threshold set by the user
    data2Input <- reactive({
        if (isolate({input$dataset2 != ''})){
            # initialize the filter in function of the parameters chosen by the user
            if (!input$filterDiffdeltaPSI){
                filterDiffdeltaPSI = NULL
            } else{
                filterDiffdeltaPSI = input$diffdeltaPSI
            }
            if (!input$filterbaseMean1){
                filterBaseMean1 = NULL
            } else{
                filterBaseMean1 = input$baseMean1
            }
            if (!input$filterbaseMean2){
                filterBaseMean2 = NULL
            } else{
                filterBaseMean2 = input$baseMean2
            }
            if (!input$filterC1fpkm1){
                filterC1fpkm1 = NULL
            } else{
                filterC1fpkm1 = input$C1fpkm1
            }
            if (!input$filterC2fpkm1){
                filterC2fpkm1 = NULL
            } else{
                filterC2fpkm1 = input$C2fpkm1
            }
            if (!input$filterC1fpkm2){
                filterC1fpkm2 = NULL
            } else{
                filterC1fpkm2 = input$C1fpkm2
            }
            if (!input$filterC2fpkm2){
                filterC2fpkm2 = NULL
            } else{
                filterC2fpkm2 = input$C2fpkm2
            }
            if (!input$filterORfpkm1){
                filterORfpkm1 = NULL
            } else{
                filterORfpkm1 = input$ORfpkm1
            }
            if (!input$filterORfpkm2){
                filterORfpkm2 = NULL
            } else{
                filterORfpkm2 = input$ORfpkm2
            }
            if (!input$filterC1LocalExpr1){
                filterC1LocalExpr1 <- NULL
            } else{
                filterC1LocalExpr1 <- input$C1LocalExpr1
            }
            if (!input$filterC2LocalExpr1){
                filterC2LocalExpr1 <- NULL
            } else{
                filterC2LocalExpr1 <- input$C2LocalExpr1
            }
            if (!input$filterC1LocalExpr2){
                filterC1LocalExpr2 <- NULL
            } else{
                filterC1LocalExpr2 <- input$C1LocalExpr2
            }
            if (!input$filterC2LocalExpr2){
                filterC2LocalExpr2 <- NULL
            } else{
                filterC2LocalExpr2 <- input$C2LocalExpr2
            }
            if (!input$filterORLocalExpr1){
                filterORLocalExpr1 <- NULL
            } else{
                filterORLocalExpr1 <- input$ORLocalExpr1
            }
            if (!input$filterORLocalExpr2){
                filterORLocalExpr2 <- NULL
            } else{
                filterORLocalExpr2 <- input$ORLocalExpr2
            }
            if (!input$filterC1PSI1){
                filterC1PSI1 <- NULL
            } else{
                filterC1PSI1 <- input$C1PSI1
            }
            if (!input$filterC2PSI1){
                filterC2PSI1 <- NULL
            } else{
                filterC2PSI1 <- input$C2PSI1
            }
            if (!input$filterC1PSI2){
                filterC1PSI2 <- NULL
            } else{
                filterC1PSI2 <- input$C1PSI2
            }
            if (!input$filterC2PSI2){
                filterC2PSI2 <- NULL
            } else{
                filterC2PSI2 <- input$C2PSI2
            }
            if (!input$filterl2fc1){
                filterl2fc1_min = NULL
                filterl2fc1_max = NULL
            } else{
                filterl2fc1_min = input$l2fc1[1]
                filterl2fc1_max = input$l2fc1[2]
            }
            if (!input$filterl2fc2){
                filterl2fc2_min = NULL
                filterl2fc2_max = NULL
            } else{
                filterl2fc2_min = input$l2fc2[1]
                filterl2fc2_max = input$l2fc2[2]
            }
            if (!input$filterpadjDESeq1){
                filterpadjDESeq1 = NULL
            } else{
                filterpadjDESeq1 = input$padjDESeq1
            }
            if (!input$filterpadjDESeq2){
                filterpadjDESeq2 = NULL
            } else{
                filterpadjDESeq2 = input$padjDESeq2
            }
            if (!input$filterVDegree){
                filterVDegree = NULL
            } else{
                filterVDegree = input$vDegree
            }
            if (!input$filterHDegree){
                filterHDegree = NULL
            } else{
                filterHDegree = input$hDegree
            }
            # apply filters
            filter2ASData(dataset2Input(),
                          input$deltaPSI1, 
                          input$signDeltaPSI1,
                          input$padj1,
                          input$deltaPSI2, 
                          input$signDeltaPSI2,
                          input$padj2, 
                          input$eventType,
                          input$splicingFactor,
                          filterDiffdeltaPSI, 
                          filterBaseMean1,
                          filterC1fpkm1,
                          filterC2fpkm1,
                          filterORfpkm1,
                          filterC1LocalExpr1,
                          filterC2LocalExpr1,
                          filterORLocalExpr1,
                          filterC1PSI1,
                          filterC2PSI1,
                          filterl2fc1_min,
                          filterl2fc1_max,
                          filterpadjDESeq1,
                          filterBaseMean2,
                          filterC1fpkm2,
                          filterC2fpkm2,
                          filterORfpkm2,
                          filterC1LocalExpr2,
                          filterC2LocalExpr2,
                          filterORLocalExpr2,
                          filterC1PSI2,
                          filterC2PSI2,
                          filterl2fc2_min,
                          filterl2fc2_max,
                          filterpadjDESeq2,
                          filterVDegree,
                          filterHDegree,
                          sign=input$signDeltaPSI)
        }
    })
    
    
    # filter the expression data with the threshold set by the user
    dataExprInput <- reactive({
        if (!input$filterVDegreeExpr){
            filterVDegree = NULL
        } else{
            filterVDegree = input$vDegreeExpr
        }
        if (!input$filterHDegreeExpr){
            filterHDegree = NULL
        } else{
            filterHDegree = input$hDegreeExpr
        }
        if (!input$filterORfpkmExpr){
            filterORfpkmExpr = NULL
        } else{
            filterORfpkmExpr = input$ORfpkmExpr
        }
        filterExprData(datasetExprInput(),
                       input$baseMeanExpr,
                       input$C1fpkmExpr,
                       input$C2fpkmExpr,
                       input$l2fcExpr,
                       input$padjExpr,
                       input$splicingFactorExpr,
                       filterVDegree,
                       filterHDegree,
                       filterORfpkmExpr)
    })
    
    # filter the readthrough data with the threshold set by the user
    dataRTInput <- reactive({
        # initialize the filter in function of the parameters chosen by the user
        if (!input$filterbaseMean_RT){
            filterBaseMean = NULL
        } else{
            filterBaseMean = input$baseMean_RT
        }
        if (!input$filterl2fc_RT){
            filterl2fc_RT <- NULL
        } else{
            filterl2fc_RT <- input$l2fc_RT
        }
        if (!input$filterl2fcDESeq_RT){
            filterl2fc_min = NULL
            filterl2fc_max = NULL
        } else{
            filterl2fc_min = input$l2fcDESeq_RT[1]
            filterl2fc_max = input$l2fcDESeq_RT[2]
        }
        if (!input$filterpadjDESeq_RT){
            filterpadjDESeq = NULL
        } else{
            filterpadjDESeq = input$padjDESeq_RT
        }
        if (!input$filterORfpkm_RT){
            filterORfpkm = NULL
        } else{
            filterORfpkm = input$ORfpkm_RT
        }
        if (!input$filterVDegree_RT){
            filterVDegree = NULL
        } else{
            filterVDegree = input$vDegree_RT
        }
        if (!input$filterHDegree_RT){
            filterHDegree = NULL
        } else{
            filterHDegree = input$hDegree_RT
        }
        filterRTData(datasetRTInput(),
                     input$padjRT,
                     input$dRatioRT,
                     input$splicingFactorRT,
                     filterBaseMean,
                     filterl2fc_RT,
                     filterl2fc_min,
                     filterl2fc_max,
                     filterpadjDESeq,
                     filterORfpkm,
                     filterVDegree,
                     filterHDegree
        )
    })
    
    
    ####### functions to show description of datasets #######
    
    # show description of the chosen dataset for PCA
    output$descriptionPCA <- renderText({
        if (input$DataTypePCA == "splicing"){
            descriptionPCA(input$DataTypePCA, input$ComparisonPCA, input$ASEtypePCA)
        } else{
            descriptionPCA(input$DataTypePCA, input$ComparisonPCA)
        }
    })
    
    # show description of the chosen dataset
    output$description <- renderText({
        descriptionASData(input$dataset)
    })
    
    # show description of the chosen dataset2
    output$description2 <- renderText({
        descriptionASData(input$dataset2)
    })
    
    # show description of the chosen expression dataset
    output$descriptionExpr <- renderText({
        descriptionExprData(input$datasetExpr)
    })
    
    # show summary of the DESeq analysis
    output$summaryDESeq <- renderPrint({
        if (input$datasetExpr != ''){
            summary(results(getDESeqData(input$datasetExpr)))
        }
    })
    
    ####### module calls for the table and plot panels #######
    
    # call module pcaPlotPanel for the plot of the PCA
    callModule(pcaPlotPanel, 'plotPCA',
               typeData = reactive({conditionsPCA()$dataType}),
               dataInput = reactive({datasetPCAInput()})
    )
    
    # call module tablePanel for the view of the table of the first dataset
    callModule(tablePanel, 'showDataset',
               nameData = reactive({input$dataset}), 
               dataInput = reactive({dataInput()})
    )
    
    # call module tablePanel for the view of the table of dataset2
    callModule(tablePanel, 'showDataset2',
               nameData = reactive({paste(input$dataset, input$dataset2, sep = "_")}), 
               dataInput = reactive({data2Input()})
    )
    
    # call module tablePanel for the view of the table of expression dataset
    callModule(tablePanel, 'showDatasetExpr',
               nameData = reactive({input$datasetExpr}), 
               dataInput = reactive({dataExprInput()})
    )
    
    # call module tablePanel for the view of the table of readthrough dataset
    callModule(tablePanel, 'showDatasetRT',
               nameData = reactive({"U virus VS mock"}), 
               dataInput = reactive({dataRTInput()})
    )
    
    # call module plotPanel for the plot of the first dataset
    callModule(plotPanel, 'plotDataset', 
               nameData = reactive({input$dataset}), 
               dataInput = reactive({dataInput()}),
               selectedCol = c("Gene_Name", "Chromosome_and_genomic_position", "Strand", 
                               "Event_type", "Event_name",
                               "Variable_part_length", "adjusted_pvalue", 
                               "dPSI", "baseMean", "log2FoldChange",
                               "C1_local_expression", "C2_local_expression",
                               "C1_fpkm", "C2_fpkm", "C1_PSI_mean", "C2_PSI_mean")
    )
    
    # call module plotPanel for the plot of dataset2
    callModule(plotPanel, 'plotDataset2', 
               nameData = reactive({input$dataset2}), 
               dataInput = reactive({data2Input()}),
               selectedCol = c("Gene_Name", "Chromosome_and_genomic_position", "Strand", 
                               "Event_type", "Event_name",
                               "Variable_part_length", "adjusted_pvalue.1",
                               "dPSI.1","adjusted_pvalue.2", "dPSI.2",
                               "baseMean.1", "log2FoldChange.1", 
                               "baseMean.2", "log2FoldChange.2",
                               "C1_local_expression.1", "C2_local_expression.1",
                               "C1_local_expression.2", "C2_local_expression.2",
                               "C1_fpkm.1", "C2_fpkm.1", "C1_fpkm.2", "C2_fpkm.2",
                               "C1_PSI_mean.1", "C2_PSI_mean.1", "C1_PSI_mean.2", "C2_PSI_mean.2")
    )
    
    # call module plotPanel for the plot of expression dataset
    callModule(plotPanel, 'plotDatasetExpr', 
               nameData = reactive({input$datasetExpr}), 
               dataInput = reactive({dataExprInput()}),
               selectedCol = c("Gene_Name", "baseMean", "log2FoldChange", "padj",
                               "C1_fpkm", "C2_fpkm")
    )
    
    # call module plotPanel for the plot of readthrough dataset
    callModule(plotPanel, 'plotDatasetRT', 
               nameData = reactive({"U virus VS mock"}), 
               dataInput = reactive({dataRTInput()}),
               selectedCol = c("Gene_Name", "RT_padj", "delta_ratio")
    )
    
    
    ####### others functions #######
    
    # Close the running server when the browser tab is closed (only when running the app on localhost)
    session$onSessionEnded(stopApp)
}
