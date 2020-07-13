##### helpers functions #####

# chose PCA data
chosePCAData <- function(typeData, comparison, ASEtype = ''){
    PCAresults <- switch(typeData,
                          "expression" = switch(comparison,
                                                "U virus VS mock" = PCAexpr_U_virusVSmock,
                                                "C virus VS mock" = PCAexpr_C_virusVSmock,
                                                "R virus VS mock" = PCAexpr_R_virusVSmock,
                                                "UC virus VS mock" = PCAexpr_UC_virusVSmock,
                                                "UCR virus VS mock" = PCAexpr_UCR_virusVSmock,
                                                "mock R VS C" = PCAexpr_mock_siRVSsiC,
                                                "virus R VS C" = PCAexpr_virus_siRVSsiC,
                                                "U virus VS mock + mock R VS C" = PCAexpr_4cond
                          ),
                          "splicing" = switch(ASEtype,
                                              "All" = switch(comparison,
                                                             "U virus VS mock" = PCA_ASE_U_virusVSmock$All,
                                                             "C virus VS mock" = PCA_ASE_C_virusVSmock$All,
                                                             "R virus VS mock" = PCA_ASE_R_virusVSmock$All,
                                                             "UC virus VS mock" = PCA_ASE_UC_virusVSmock$All,
                                                             "UCR virus VS mock" = PCA_ASE_UCR_virusVSmock$All,
                                                             "mock R VS C" = PCA_ASE_mock_siRVSsiC$All,
                                                             "virus R VS C" = PCA_ASE_virus_siRVSsiC$All,
                                                             "U virus VS mock + mock R VS C" = PCA_ASE_4cond$All),
                                              "ES" = switch(comparison,
                                                            "U virus VS mock" = PCA_ASE_U_virusVSmock$ES,
                                                            "C virus VS mock" = PCA_ASE_C_virusVSmock$ES,
                                                            "R virus VS mock" = PCA_ASE_R_virusVSmock$ES,
                                                            "UC virus VS mock" = PCA_ASE_UC_virusVSmock$ES,
                                                            "UCR virus VS mock" = PCA_ASE_UCR_virusVSmock$ES,
                                                            "mock R VS C" = PCA_ASE_mock_siRVSsiC$ES,
                                                            "virus R VS C" = PCA_ASE_virus_siRVSsiC$ES,
                                                            "U virus VS mock + mock R VS C" = PCA_ASE_4cond$ES),
                                              "ES_MULTI" = switch(comparison,
                                                                  "U virus VS mock" = PCA_ASE_U_virusVSmock$ES_MULTI,
                                                                  "C virus VS mock" = PCA_ASE_C_virusVSmock$ES_MULTI,
                                                                  "R virus VS mock" = PCA_ASE_R_virusVSmock$ES_MULTI,
                                                                  "UC virus VS mock" = PCA_ASE_UC_virusVSmock$ES_MULTI,
                                                                  "UCR virus VS mock" = PCA_ASE_UCR_virusVSmock$ES_MULTI,
                                                                  "mock R VS C" = PCA_ASE_mock_siRVSsiC$ES_MULTI,
                                                                  "virus R VS C" = PCA_ASE_virus_siRVSsiC$ES_MULTI,
                                                                  "U virus VS mock + mock R VS C" = PCA_ASE_4cond$ES_MULTI),
                                              "IR" = switch(comparison,
                                                            "U virus VS mock" = PCA_ASE_U_virusVSmock$IR,
                                                            "C virus VS mock" = PCA_ASE_C_virusVSmock$IR,
                                                            "R virus VS mock" = PCA_ASE_R_virusVSmock$IR,
                                                            "UC virus VS mock" = PCA_ASE_UC_virusVSmock$IR,
                                                            "UCR virus VS mock" = PCA_ASE_UCR_virusVSmock$IR,
                                                            "mock R VS C" = PCA_ASE_mock_siRVSsiC$IR,
                                                            "virus R VS C" = PCA_ASE_virus_siRVSsiC$IR,
                                                            "U virus VS mock + mock R VS C" = PCA_ASE_4cond$IR),
                                              "altA" = switch(comparison,
                                                            "U virus VS mock" = PCA_ASE_U_virusVSmock$altA,
                                                            "C virus VS mock" = PCA_ASE_C_virusVSmock$altA,
                                                            "R virus VS mock" = PCA_ASE_R_virusVSmock$altA,
                                                            "UC virus VS mock" = PCA_ASE_UC_virusVSmock$altA,
                                                            "UCR virus VS mock" = PCA_ASE_UCR_virusVSmock$altA,
                                                            "mock R VS C" = PCA_ASE_mock_siRVSsiC$altA,
                                                            "virus R VS C" = PCA_ASE_virus_siRVSsiC$altA,
                                                            "U virus VS mock + mock R VS C" = PCA_ASE_4cond$altA),
                                              "altD" = switch(comparison,
                                                            "U virus VS mock" = PCA_ASE_U_virusVSmock$altD,
                                                            "C virus VS mock" = PCA_ASE_C_virusVSmock$altD,
                                                            "R virus VS mock" = PCA_ASE_R_virusVSmock$altD,
                                                            "UC virus VS mock" = PCA_ASE_UC_virusVSmock$altD,
                                                            "UCR virus VS mock" = PCA_ASE_UCR_virusVSmock$altD,
                                                            "mock R VS C" = PCA_ASE_mock_siRVSsiC$altD,
                                                            "virus R VS C" = PCA_ASE_virus_siRVSsiC$altD,
                                                            "U virus VS mock + mock R VS C" = PCA_ASE_4cond$altD)
                          ),
                          "readthrough" = switch(comparison,
                                                "U virus VS mock" = PCA_RT_U_virusVSmock,
                                                "C virus VS mock" = PCA_RT_C_virusVSmock)
    )
    return(PCAresults)
}

# chose AS data
choseASData <- function(ASdatasetName){
    ASdataset <- switch(ASdatasetName,
           "U virus VS mock" = U_virusVSmock,
           "C virus VS mock" = C_virusVSmock,
           "R virus VS mock" = R_virusVSmock,
           "UC virus VS mock" = UC_virusVSmock,
           "UCR virus VS mock" = UCR_virusVSmock,
           "mock R VS C" = mock_siRVSsiC,
           "virus R VS C" = virus_siRVSsiC
    )
    if (!is.null(ASdataset)){
        # merge with the expression data 
        # (that already contain splicing factors info and virhostnet data)
        ASdataset <- addExprInfo(ASdatasetName, ASdataset)
    }
    return(ASdataset)
}

# chose Expr data
getDESeqData <- function(ExprdatasetName){
    Exprdataset <- switch(ExprdatasetName,
            "U virus VS mock" = dds_U_virusVSmock,
            "C virus VS mock" = dds_C_virusVSmock,
            "R virus VS mock" = dds_R_virusVSmock,
            "UC virus VS mock" = dds_UC_virusVSmock,
            "UCR virus VS mock" = dds_UCR_virusVSmock,
            "mock R VS C" = dds_mock_siRVSsiC,
            "virus R VS C" = dds_virus_siRVSsiC
    )
    return(Exprdataset)
}
choseExprData <- function(ExprdatasetName){
    Exprdataset <- switch(ExprdatasetName,
            "U virus VS mock" = dds_U_virusVSmock_res,
            "C virus VS mock" = dds_C_virusVSmock_res,
            "R virus VS mock" = dds_R_virusVSmock_res,
            "UC virus VS mock" = dds_UC_virusVSmock_res,
            "UCR virus VS mock" = dds_UCR_virusVSmock_res,
            "mock R VS C" = dds_mock_siRVSsiC_res,
            "virus R VS C" = dds_virus_siRVSsiC_res
    )
    if (!is.null(Exprdataset)){
        # merge with splicing factor list
        Exprdataset <- addSplicFInfo(Exprdataset, splicF)
        # merge with the virhostnet data
        Exprdataset <- addVirhostnetInfo(Exprdataset, virhostnet)
    }
    return(Exprdataset)
}

# chose RT data
choseRTData <- function(){
    RT_U_virusVSmock <- addExprInfoRT("U virus VS mock", RT_U_virusVSmock)
    return(RT_U_virusVSmock)
}

# add expression information to AS data
addExprInfo <- function(datasetName, dataset){
    expr <- choseExprData(datasetName)
    # merge with the expression data
    dataset <- merge(dataset, expr, by.x = c("Gene_Name", "Gene_Id"), by.y = c("Gene_Name", "EnsemblID"), all.x = TRUE)
    return(dataset)
}

# add expression information to RT data
addExprInfoRT <- function(datasetName, dataset){
    expr <- choseExprData(datasetName)
    # select only the column we want to keep for the merge
    expr <- expr[, c(1,2,5,11:14)]
    # remove the version number at the end of the ensemblID to be able to do the merge
    expr$EnsemblID <- unlist(lapply(strsplit(as.character(expr$EnsemblID), "\\."), function(x){ x[1] }))
    # merge with the expression data
    dataset <- merge(dataset, expr, by.x = c("Gene_Name", "Gene_Id"), by.y = c("Gene_Name", "EnsemblID"), all.x = TRUE)
    return(dataset)
}

# load splicing factor list
loadSplicingFactorList <- function(){
    splicF <- read.table("Data/Splicing_factors_Clara.csv", header = FALSE, sep = ",")
    names(splicF) <- c("Gene", "isSplicingFactor")
    return(splicF)
}

# annotate AS/Expression data with splicing factors information
addSplicFInfo <- function(dataset, splicF){
    mergedData <- merge(dataset, splicF, by.x = "Gene_Name", by.y = "Gene", all.x = TRUE)
    return(mergedData)
}

# get a vector containing the possible dataset2 in a vector
vectorDataset2 <- function(dataset1){
    d <- c("U virus VS mock", "C virus VS mock", "R virus VS mock", "UC virus VS mock", 
           "UCR virus VS mock", "mock R VS C", "virus R VS C")
    d2 <- d[!d %in% c(dataset1)]
    return(d2)
}

# get a vector of the column in dataset expect for the ones correponding to the expression table
getColumnName <-function(dataset, toRemove = c("EnsemblID", "lfcSE", "stat", "pvalue")){
    colNames <- names(dataset)
    colNamesCleaned <- colNames[! colNames %in% toRemove]
    return(colNamesCleaned)
}

# description of chosen dataset
descriptionPCA <- function(typeData, comparison, ASEtype = ""){
    if (comparison == ""){
        return("")
    }
    if (ASEtype != ""){
        listText <- c("Principal component analysis on ", typeData, "(", ASEtype, "events) data from comparison ", comparison, ".")
    } else{
        listText <- c("Principal component analysis on ", typeData, " data from comparison ", comparison, ".")
    }
    return(paste0(listText, collapse = ""))
}

# description of the chosen AS data
descriptionASData <- function(ASdatasetName){
    ASdataset <- choseASData(ASdatasetName)
    nbrEvents <- dim(ASdataset)[1]
    description <- switch(ASdatasetName,
        "U virus VS mock" = paste0("AS analysis with KisSplice (stranded).<br/>
                                   Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                   untreated (U).<br/>
                                   A total of ", nbrEvents, " events were analyzed by kissDE.<br/>
                                   C1 = U_virus<br/>
                                   C2 = U_mock"),
        "C virus VS mock" = paste0("AS analysis with KisSplice (stranded).<br/>
                                   Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                   treated with siRNA Control (C).<br/>
                                   A total of ", nbrEvents, " events were analyzed by kissDE.<br/>
                                   C1 = C_virus<br/>
                                   C2 = C_mock"),
        "R virus VS mock" = paste0("AS analysis with KisSplice (stranded).<br/>
                                   Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                   treated with siRNA RED-SMU1 (R).<br/>
                                   A total of ", nbrEvents, " events were analyzed by kissDE.<br/>
                                   C1 = R_virus<br/>
                                   C2 = R_mock"),
        "UC virus VS mock" = paste0("AS analysis with KisSplice (stranded).<br/>
                                   Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                   untreated (U) and treated with siRNA Control (C).<br/>
                                   A total of ", nbrEvents, " events were analyzed by kissDE.<br/>
                                   C1 = U_virus & C_virus<br/>
                                   C2 = U_mock & C_mock"),
        "UCR virus VS mock" = paste0("AS analysis with KisSplice (stranded).<br/>
                                   Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                   in all conditions (untreated, treated with siRNA Control and treated with siRNA RED-SMU1).<br/>
                                   A total of ", nbrEvents, " events were analyzed by kissDE.<br/>
                                   C1 = U_virus, C_virus & R_virus<br/>
                                   C2 = U_mock, C_mock & R_mock"),
        "mock R VS C" = paste0("AS analysis with KisSplice (stranded).<br/>
                                   Comparison of the samples treated with siRNA RED-SMU1 (R) versus the samples treated
                                   with siRNA Control (C) in the non-infected condition (mock).<br/>
                                   A total of ", nbrEvents, " events were analyzed by kissDE.<br/>
                                   C1 = R_mock<br/>
                                   C2 = C_mock"),
        "virus R VS C" = paste0("AS analysis with KisSplice (stranded).<br/>
                                   Comparison of the samples treated with siRNA RED-SMU1 (R) versus the samples treated
                                   with siRNA Control (C) in the infected condition (virus).<br/>
                                   A total of ", nbrEvents, " events were analyzed by kissDE.<br/>
                                   C1 = R_virus<br/>
                                   C2 = C_virus")
    )
    return(description)
}

# description of the chosen expression data
descriptionExprData <- function(ExprdatasetName){
    description <- switch(ExprdatasetName,
        "U virus VS mock" = paste0("Expression analysis with DESeq2.<br/>
                                    Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                    untreated (U).<br/>
                                    C1 = U_virus<br/>
                                    C2 = U_mock"),
        "C virus VS mock" = paste0("Expression analysis with DESeq2.<br/>
                                    Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                    treated with siRNA Control (C).<br/>
                                    C1 = C_virus<br/>
                                    C2 = C_mock"),
        "R virus VS mock" = paste0("Expression analysis with DESeq2.<br/>
                                    Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                    treated with siRNA RED-SMU1 (R).<br/>
                                    C1 = R_virus<br/>
                                    C2 = R_mock"),
        "UC virus VS mock" = paste0("Expression analysis with DESeq2.<br/>
                                    Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                    untreated (U) and treated with siRNA Control (C).<br/>
                                    C1 = U_virus & C_virus<br/>
                                    C2 = U_mock & C_virus"),
        "UCR virus VS mock" = paste0("Expression analysis with DESeq2.<br/>
                                     Comparison of the infected samples (virus) versus the non-infected samples (mock)
                                     in all conditions (untreated, treated with siRNA Control and treated with siRNA RED-SMU1).<br/>
                                     C1 = U_virus, C_virus & R_virus<br/>
                                     C2 = U_mock, C_mock & R_mock"),
        "mock R VS C" = paste0("Expression analysis with DESeq2.<br/>
                               Comparison of the samples treated with siRNA RED-SMU1 (R) versus the samples treated
                               with siRNA Control (C) in the non-infected condition (mock).<br/>
                               C1 = R_mock<br/>
                               C2 = C_mock"),
        "virus R VS C" = paste0("Expression analysis with DESeq2.<br/>
                               Comparison of the samples treated with siRNA RED-SMU1 (R) versus the samples treated
                               with siRNA Control (C) in the infected condition (virus).<br/>
                               C1 = R_virus<br/>
                               C2 = C_virus")
    )
    return(description)
}

# merge 2 AS datasets
mergeASData <- function(dataset1, dataset2){
    mergeData <- merge(dataset1, dataset2, by=c(1:19,35:38), suffixes = c(".1", ".2"))
    return(mergeData)
}

# filter the AS dataset with the threshold chosen by the user
filterASData <- function(ASdataset, deltaPSI, padj, eventType, splicingFactor,
                         baseMean=NULL, C1fpkm=NULL, C2fpkm=NULL, ORfpkm=NULL,
                         C1LocalExpr=NULL, C2LocalExpr=NULL, ORLocalExpr=NULL,
                         C1PSI=NULL, C2PSI=NULL,
                         l2fc_min=NULL, l2fc_max=NULL, padjExpr=NULL,
                         vDegree=NULL, hDegree=NULL){
    filtered_ASdata <- ASdataset[which(abs(ASdataset$'dPSI') >= deltaPSI/100 &
                                ASdataset$'adjusted_pvalue' <= padj &
                                ASdataset$'Event_type' %in% eventType), 
                                ]
    if (!is.null(baseMean)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'baseMean' >= baseMean),]
    }
    if (!is.null(C1fpkm)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_fpkm' >= C1fpkm),]
    }
    if (!is.null(C2fpkm)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_fpkm' >= C2fpkm),]
    }
    if (!is.null(ORfpkm)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_fpkm' >= ORfpkm | filtered_ASdata$'C1_fpkm' >= ORfpkm),]
    }
    if (!is.null(C1LocalExpr)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_local_expression' >= C1LocalExpr),]
    }
    if (!is.null(C2LocalExpr)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_local_expression' >= C2LocalExpr),]
    }
    if (!is.null(ORLocalExpr)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_local_expression' >= ORLocalExpr | 
                                                     filtered_ASdata$'C2_local_expression' >= ORLocalExpr),]
    }
    if (!is.null(C1PSI)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_PSI_mean' >= C1PSI/100),]
    }
    if (!is.null(C2PSI)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_PSI_mean' >= C2PSI/100),]
    }
    if (!is.null(l2fc_min) & !is.null(l2fc_max)){
        filtered_ASdata <- filtered_ASdata[which(abs(filtered_ASdata$'log2FoldChange') <= l2fc_max &
                                                 abs(filtered_ASdata$'log2FoldChange') >= l2fc_min),]
    }
    if (!is.null(padjExpr)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'padj' >= padjExpr),]
    }
    if (splicingFactor){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'isSplicingFactor' == 1),]
    }
    if (!is.null(vDegree)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'v_degree_max' >= vDegree),]
    }
    if (!is.null(hDegree)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'h_degree_max' >= hDegree),]
    }
    return(filtered_ASdata)
}

# filter the merge AS dataset with the threshold chosen by the user
filter2ASData <- function(ASdataset, deltaPSI1, signDeltaPSI1, padj1, deltaPSI2, signDeltaPSI2, padj2,
                          eventType, splicingFactor, diffdeltaPSI=NULL,
                          baseMean1=NULL, C1fpkm1=NULL, C2fpkm1=NULL, ORfpkm1=NULL, 
                          C1LocalExpr1=NULL, C2LocalExpr1=NULL, ORLocalExpr1=NULL,
                          C1PSI1=NULL, C2PSI1=NULL, l2fc1_min=NULL, l2fc1_max=NULL, padjExpr1=NULL, 
                          baseMean2=NULL, C1fpkm2=NULL, C2fpkm2=NULL, ORfpkm2=NULL, 
                          C1LocalExpr2=NULL, C2LocalExpr2=NULL, ORLocalExpr2=NULL,
                          C1PSI2=NULL, C2PSI2=NULL, l2fc2_min=NULL, l2fc2_max=NULL, padjExpr2=NULL,
                          vDegree=NULL, hDegree=NULL, sign=FALSE){
    if (sign){
        ligneSameSign <- (ASdataset$'dPSI.1' >= 0 & ASdataset$'dPSI.2' >= 0) | 
                         (ASdataset$'dPSI.1' < 0 & ASdataset$'dPSI.2' < 0)
    } else{
        ligneSameSign <- rep(TRUE, length(ASdataset$'dPSI.1'))
    }
    if (signDeltaPSI1 == "+"){
        ligneSignDeltaPSI1 <- ASdataset$'dPSI.1' >= 0
    } else if (signDeltaPSI1 == "-"){
        ligneSignDeltaPSI1 <- ASdataset$'dPSI.1' <= 0
    } else{
        ligneSignDeltaPSI1 <- rep(TRUE, length(ASdataset$'dPSI.1'))
    }
    if (signDeltaPSI2 == "+"){
        ligneSignDeltaPSI2 <- ASdataset$'dPSI.2' >= 0
    } else if (signDeltaPSI2 == "-"){
        ligneSignDeltaPSI2 <- ASdataset$'dPSI.2' <= 0
    } else{
        ligneSignDeltaPSI2 <- rep(TRUE, length(ASdataset$'dPSI.2'))
    }
    filtered_ASdata <- ASdataset[which(abs(ASdataset$'dPSI.1') >= deltaPSI1/100 &
                                       ligneSignDeltaPSI1 &
                                       abs(ASdataset$'dPSI.2') >= deltaPSI2/100 &
                                       ligneSignDeltaPSI2 & 
                                       ASdataset$'adjusted_pvalue.1' <= padj1 &
                                       ASdataset$'adjusted_pvalue.2' <= padj2 &
                                       ASdataset$'Event_type' %in% eventType &
                                       ligneSameSign),
                                ]
    if (!is.null(diffdeltaPSI)){
        filtered_ASdata <- filtered_ASdata[which(abs(filtered_ASdata$'dPSI.1' - filtered_ASdata$'dPSI.2') <= diffdeltaPSI/100),]
    }
    if (!is.null(baseMean1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'baseMean.1' >= baseMean1),]
    }
    if (!is.null(baseMean2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'baseMean.2' >= baseMean2),]
    }
    if (!is.null(C1fpkm1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_fpkm.1' >= C1fpkm1),]
    }
    if (!is.null(C2fpkm1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_fpkm.1' >= C2fpkm1),]
    }
    if (!is.null(C1fpkm2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_fpkm.2' >= C1fpkm2),]
    }
    if (!is.null(C2fpkm2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_fpkm.2' >= C2fpkm2),]
    }
    if (!is.null(ORfpkm1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_fpkm.1' >= ORfpkm1 | filtered_ASdata$'C1_fpkm.1' >= ORfpkm1),]
    }
    if (!is.null(ORfpkm2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_fpkm.2' >= ORfpkm2 | filtered_ASdata$'C1_fpkm.2' >= ORfpkm2),]
    }
    if (!is.null(C1LocalExpr1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_local_expression.1' >= C1LocalExpr1),]
    }
    if (!is.null(C2LocalExpr1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_local_expression.1' >= C2LocalExpr1),]
    }
    if (!is.null(C1LocalExpr2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_local_expression.2' >= C1LocalExpr2),]
    }
    if (!is.null(C2LocalExpr2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_local_expression.2' >= C2LocalExpr2),]
    }
    if (!is.null(ORLocalExpr1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_local_expression.1' >= ORLocalExpr1 | 
                                                     filtered_ASdata$'C2_local_expression.1' >= ORLocalExpr1),]
    }
    if (!is.null(ORLocalExpr2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_local_expression.2' >= ORLocalExpr2 | 
                                                     filtered_ASdata$'C2_local_expression.2' >= ORLocalExpr2),]
    }
    if (!is.null(C1PSI1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_PSI_mean.1' >= C1PSI1/100),]
    }
    if (!is.null(C2PSI1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_PSI_mean.1' >= C2PSI1/100),]
    }
    if (!is.null(C1PSI2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C1_PSI_mean.2' >= C1PSI2/100),]
    }
    if (!is.null(C2PSI2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'C2_PSI_mean.2' >= C2PSI2/100),]
    }
    if (!is.null(l2fc1_min) & !is.null(l2fc1_max)){
        filtered_ASdata <- filtered_ASdata[which(abs(filtered_ASdata$'log2FoldChange.1') <= l2fc1_max &
                                                 abs(filtered_ASdata$'log2FoldChange.1') >= l2fc1_min),]
    }
    if (!is.null(l2fc2_min) & !is.null(l2fc2_max)){
        filtered_ASdata <- filtered_ASdata[which(abs(filtered_ASdata$'log2FoldChange.2') <= l2fc2_max &
                                                 abs(filtered_ASdata$'log2FoldChange.2') >= l2fc2_min),]
    }
    if (!is.null(padjExpr1)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'padj.1' >= padjExpr1),]
    }
    if (!is.null(padjExpr2)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'padj.2' >= padjExpr2),]
    }
    if (splicingFactor){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'isSplicingFactor' == 1),]
    }
    if (!is.null(vDegree)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'v_degree_max' >= vDegree),]
    }
    if (!is.null(hDegree)){
        filtered_ASdata <- filtered_ASdata[which(filtered_ASdata$'h_degree_max' >= hDegree),]
    }
    return(filtered_ASdata)
}

# filter the expression dataset with the threshold chosen by the user
filterExprData <- function(Exprdataset, baseMean, C1fpkm, C2fpkm, l2fc, padj, 
                           splicingFactor, vDegree=NULL, hDegree=NULL, ORfpkm=NULL){
    filtered_ExprData <- Exprdataset[which(Exprdataset$'baseMean' >= baseMean &
                                           Exprdataset$'C1_fpkm' >= C1fpkm &
                                           Exprdataset$'C2_fpkm' >= C2fpkm &
                                           abs(Exprdataset$'log2FoldChange') >= l2fc &
                                           Exprdataset$'padj' <= padj),
                                ]
    if (splicingFactor){
        filtered_ExprData <- filtered_ExprData[which(filtered_ExprData$'isSplicingFactor' == 1),]
    }
    if (!is.null(vDegree)){
        filtered_ExprData <- filtered_ExprData[which(filtered_ExprData$'v_degree_max' >= vDegree),]
    }
    if (!is.null(hDegree)){
        filtered_ExprData <- filtered_ExprData[which(filtered_ExprData$'h_degree_max' >= hDegree),]
    }
    if (!is.null(ORfpkm)){
        filtered_ExprData <- filtered_ExprData[which(filtered_ExprData$'C1_fpkm' >= ORfpkm | filtered_ExprData$'C2_fpkm' >= ORfpkm),]
    }
    return(filtered_ExprData)
}

# filter the readthrough dataset with the threshold chosen by the user
filterRTData <- function(RTdataset, padj, dRatio, splicingFactor, 
                         baseMean=NULL, l2fc_RT=NULL, l2fc_min=NULL, l2fc_max=NULL, 
                         padjExpr=NULL, ORfpkm=NULL, vDegree=NULL, hDegree=NULL){
    filtered_RTData <- RTdataset[which(RTdataset$'RT_padj' < padj &
                                      abs(RTdataset$'delta_ratio') > dRatio),
                                ]
    if (splicingFactor){
        filtered_RTData <- filtered_RTData[which(filtered_RTData$'isSplicingFactor' == 1),]
    }
    if (!is.null(baseMean)){
        filtered_RTData <- filtered_RTData[which(filtered_RTData$'baseMean' >= baseMean),]
    }
    if (!is.null(l2fc_RT)){
        filtered_RTData <- filtered_RTData[which(abs(filtered_RTData$'RT_log2FoldChange') >= l2fc_RT),]
    }
    if (!is.null(l2fc_min) & !is.null(l2fc_max)){
        filtered_RTData <- filtered_RTData[which(abs(filtered_RTData$'log2FoldChange') <= l2fc_max &
                                                 abs(filtered_RTData$'log2FoldChange') >= l2fc_min),]
    }
    if (!is.null(padjExpr)){
        filtered_RTData <- filtered_RTData[which(filtered_RTData$'padj' >= padjExpr),]
    }
    if (!is.null(ORfpkm)){
        filtered_RTData <- filtered_RTData[which(filtered_RTData$'U_mock_fpkm' >= ORfpkm | filtered_RTData$'U_virus_fpkm' >= ORfpkm),]
    }
    if (!is.null(vDegree)){
        filtered_RTData <- filtered_RTData[which(filtered_RTData$'v_degree_max' >= vDegree),]
    }
    if (!is.null(hDegree)){
        filtered_RTData <- filtered_RTData[which(filtered_RTData$'h_degree_max' >= hDegree),]
    }
    return(filtered_RTData)
}

# get a list of alternative splicing type (in the input data)
listTypeEvents <- function(dataset){
    return(unique(dataset$Event_type))
}

# get a column from a dataset
getColumn <- function(dataset, variable){
    return(dataset[,variable])
}

# get virhostnet data
getVirhostnetData <- function(){
    drv <- dbDriver("PostgreSQL")
    dbname <- "biomedomix_1_4_0118"
    con <- dbConnect(drv, dbname = dbname,host = "192.168.100.72", port = 5434, user = "shiny", password = "shiny")

    nodes_properties <- dbGetQuery(con,"SELECT distinct id, name,acc,xref_protein_acc as ensembl,taxid,div,h_degree, v_degree 
                                   FROM network.nodes_properties p,interactome.protein_has_xref phx 
                                   WHERE p.div= 'human' and phx.database_name='ensembl' and p.id=phx.protein_id and phx.xref_protein_acc like ('ENSG%');")
    dbDisconnect(con)
    nodes_properties <- nodes_properties[,c(3,4,7,8)]
    names(nodes_properties) <- c("UniprotID", "EnsemblID", "h_degree", "v_degree")
    virhostnet <- (nodes_properties %>% 
                       group_by(EnsemblID) %>% 
                       transmute(h_degree_max = max(h_degree), 
                                 v_degree_max = max(v_degree), 
                                 UniprotIDlist = paste(UniprotID, sep=",", collapse=",")) %>%
                       as.data.frame)
    # remove duplicated lines
    virhostnet <- virhostnet[!duplicated(virhostnet), ]
    return(virhostnet)
}

# merge data with virhostnet data
addVirhostnetInfo <- function(dataset, virhostnet){
    dataset$EnsemblID_simplified <- sapply(strsplit(dataset$EnsemblID, split = "\\."), `[`, 1)
    mergedData <- merge(dataset, virhostnet, by.x = "EnsemblID_simplified", by.y = "EnsemblID", all.x = TRUE)
    mergedData <- mergedData[-1]
    return(mergedData)
    
}

# Get the possible comparisons in function of the type of data chosen (function only used for PCA)
listPossibleComparisons <- function(dataType){
    comparisons <- switch(dataType,
                  "expression" = c("U virus VS mock", "C virus VS mock", "R virus VS mock", "UC virus VS mock",
                                   "UCR virus VS mock", "mock R VS C", "virus R VS C", "U virus VS mock + mock R VS C"),
                  "splicing" = c("U virus VS mock", "C virus VS mock", "R virus VS mock", "UC virus VS mock",
                                 "UCR virus VS mock", "mock R VS C", "virus R VS C", "U virus VS mock + mock R VS C"),
                  "readthrough" = c("U virus VS mock", "C virus VS mock"))
    return(comparisons)
}
