# if no dataset is chosen, show nothing
if (!isDatasetPCADefined()){
    return()
} else{
    return(
        pcaPlotPanelUI('plotPCA')
    )
}