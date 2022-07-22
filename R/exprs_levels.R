#' @title Convert normalised counts into log2 counts
#' @description Logs and un-Logs a normalised counts matrix. Also accounts for the complexity of single cell libraries transcripts. 
#' @param m matrix of counts 
#' @param bulk  divides the counts by 10 to estimate the complexity of single cell libraries in the order of 100,000 transcripts to avoid counting each transcript ~10 times, which may inflate the difference between the expression level of a gene in cells in which the gene is detected and those in which it is not detected.  Default: FALSE.
#' @param log_scale should the counts be placed on a log2 scale.  
#' @param unlog does the reverse of log_scale.
#' @return a matrix
#' @rdname exprs_levels
#' @export 
exprs_levels <- function(m, bulk = F, log_scale = F, unlog = F){
    
    if (!length(dim(m)) == 2) stop("m must be either a matrix or dataframe")
    
    m = as.matrix(m)
    
    if (bulk) x = 1 else x = 10
    
    if (log_scale) exprs =  log2((m/x) + 1)
    
    if (unlog) exprs = x * (2^(m) - 1)
    
    return(exprs)
}

