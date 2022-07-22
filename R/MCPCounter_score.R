#' @title Run MCPCounter on a data.frame
#' @description Runs MCPCounter and produces a data.frame with abundance estimates.
#' @param x data.frame with features in rows and samples in columns.
#' @param feat_types type of identifiers for expression features. Defaults to "HUGO_symbols" (Official gene symbols) and "ENTREZ_ID" (Entrez Gene ID).
#' @param gene_props data.frame of genes transcriptomic markers (HUGO symbols or ENTREZ_ID) and corresponding cell populations.
#' @param out_digits integer indicating the number of decimal places (round). negative values are allowed.
#' @return data.frame of MCPCounter scores, where the first column is for each mixture.
#' @rdname MCPCounter_score
#' @export 
#' @importFrom MCPcounter MCPcounter.estimate
#' @importFrom tibble rownames_to_column
MCPCounter_score <- function(x, 
                             feat_types = c("HUGO_symbols","ENTREZ_ID"),
                             gene_pops = genes,
                             out_digits = 2){
    
    if(!any(class(x) %in% c("tbl_df", "tbl", "data.frame"))) stop("MCPCounter input is not a data.frame", call. = FALSE)
    
    if(!any(grepl("^gene$", colnames(x), ignore.case = T))) stop("Gene column not found in input data", call. = FALSE)
    
    feature_types <- match.arg(feat_types, choices = c("HUGO_symbols","ENTREZ_ID"),
                               several.ok = FALSE)
    
    score_data <- as.data.frame(x)
    
    gene_index <- grep("^gene$", colnames(score_data), ignore.case = T)
    
    rownames(score_data) <- score_data[[gene_index]]
    
    score_data <- score_data[,-gene_index]
    
    mcp_deconvolute <- MCPcounter::MCPcounter.estimate(score_data,
                                                       featuresType = feature_types,
                                                       genes = gene_pops)
    
    mcp_deconvolute <- as.data.frame(t(mcp_deconvolute))
    
    mcp_deconvolute <- tibble::rownames_to_column(mcp_deconvolute, 
                                                  var = "Mixture")
    
    mcp_deconvolute <-  mcp_deconvolute %>%
        mutate(across(-Mixture, ~round(.x, digits = out_digits)))
    
    return(mcp_deconvolute)
    
}
