## approximate equality
#' gmap wrapper
#'
#' @param m a matrix or Matrix
#' @param ... further option to pass to reshape2::melt
#' @rdname ggmap
ggmap = function(m, ...) {
    gmap(reshape2::melt(as.matrix(m)), ...)
}

#' sapply wrapper
#' @param X a vector (atomic or list) or an expression object. 
#' @param FUN the function to be applied to each element of X.
#' @param simplify logical or character string; should the result be simplified to a vector.
#' @param USE.NAMES use the names of X? defualt: TRUE
#' @param ... optional arguments to FUN.
#' @rdname Sapply
Sapply = function(X, FUN, ..., simplify = FALSE, USE.NAMES = TRUE) {
    sapply(X, FUN, ..., simplify = simplify, USE.NAMES = TRUE)
}


about_equal = function(x,y,tol=1e-10) {
    stopifnot(is.numeric(x), is.numeric(y), length(x)==length(y), all(abs(x-y) < tol))
}


sort_by = function(..., which = 1, decreasing = T) {
    dots = list(...)
    if (any(which > length(dots))) stop('<which> vector index is larger than the number of vectors.')
    orderers = sapply(which, function(i) dots[[i]], simplify = F)
    Order = do.call(order, c(orderers, list(decreasing = decreasing)))
    sapply(1:length(dots), function(i) dots[[i]][Order], simplify = F)
}


is_p_value = function(x) {
    !is.null(x) && is.numeric(x) && x >= 0 & x <= 1
}


is_number = function(x) {
    is.numeric(x) & length(x) == 1
}


#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%->\%}} for details.
#' @importFrom zeallot %->%
#' @export
#' @rdname unpack-assign-back
#' @name %->%
#' @keywords internal
# `%->%`
NULL

#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%<-\%}} for details.
#' @importFrom zeallot %<-%
#' @export
#' @rdname unpack-assign
#' @name %<-%
#' @keywords internal
# `%<-%`
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @title Left-hand head a matrix or dataframe
#' @param m a matrix or dataframe
#' @param i number of columns to print
#' @rdname headl
#' @export
headl = function(m, i = 6L) {
    i = min(i, ncol(m))
    format(head(m[, 1:i]))
}

#' @title Right-hand head a matrix or dataframe
#' @param m a matrix or dataframe
#' @param i number of columns to print
#' @rdname headr
#' @export
headr = function(m, i = 6L) {
    i = min(i, ncol(m))
    i2 = ncol(m)
    i = i2 - i
    banner = paste0(rep("=", 40))
    message = paste('Head first', i, ' columns:')
    format(head(m[, i:i2]))
}

#' @title Left-hand tail a matrix or dataframe
#' @param m a matrix or dataframe
#' @param i number of columns to print
#' @rdname taill
#' @export
taill = function(m, i = 6L) {
    i = min(i, ncol(m))
    format(tail(m[, 1:i]))
}


#' @title Right-hand tail a matrix or dataframe
#' @param m a matrix or dataframe
#' @param i number of columns to print
#' @rdname tailr
#' @export
tailr = function(m, i = 6L) {
    i = min(i, ncol(m))
    i2 = ncol(m)
    i = i2 - i
    format(tail(m[, i:i2]))
}
