#' Cosine Similarity
#'
#' This function returns the cosine similarity value between two vectors.
#' @param a Vector one
#' @param b Vector two
#'
#' @return  Returns the cosine similarity value between two vectors
#' @export
#'
#' @examples
cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))}
