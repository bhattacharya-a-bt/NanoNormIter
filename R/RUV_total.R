### INPUT: raw - p x n raw expressions with p genes and n samples
###        pData - phenotype metadata across samples
###        fData - feature metadata across genes
###        k - number of dimensions of unwanted variation estimated
###        exclude - vector of gene names to exclude

RUV_total <- function(raw,pData,fData,k,hkgenes = NULL,exclude = NULL){

    library(RUVSeq)
    library(DESeq2)
    library(limma)
    library(matrixStats)

    if (!is.null(hkgenes)){

        fData(set)$Class[rownames(set) %in% hkgenes] = 'Housekeeping'

    }

    fData = fData[rownames(raw),]
    int = intersect(rownames(raw),rownames(fData))
    fData = fData[int,]
    raw = raw[int,]

    set <- newSeqExpressionSet(as.matrix(round(raw)),
                               phenoData=pData,
                               featureData=fData)

    cIdx <- rownames(set)[fData(set)$Class == "Housekeeping"]
    cIdx = cIdx[!(cIdx %in% exclude)]
    x <- as.factor(pData$Group)
    set <- betweenLaneNormalization(set, which="upper")
    set <- RUVg(set, cIdx, k=k)
    dds <- DESeqDataSetFromMatrix(counts(set),colData=pData(set),design=~1)
    rowData(dds) <- fData
    dds <- estimateSizeFactors(dds)
    dds <- estimateDispersionsGeneEst(dds)
    cts <- counts(dds, normalized=TRUE)
    disp <- pmax((rowVars(cts) - rowMeans(cts)),0)/rowMeans(cts)^2
    mcols(dds)$dispGeneEst <- disp
    dds <- estimateDispersionsFit(dds, fitType="mean")
    vsd <- varianceStabilizingTransformation(dds, blind=FALSE)
    mat <- assay(vsd)
    covars <- as.matrix(colData(dds)[,grep("W",colnames(colData(dds))),drop=FALSE])
    mat <- removeBatchEffect(mat, covariates=covars)
    assay(vsd) <- mat
    return(list(set = set,vsd = vsd))

}
