#' NA Values to Zero
#'
#'This function sets all na zalues in a spread matrix to zero
#' @param mat A spread matrix in the from users, items and ratings
#'
#' @return Returns the parameter spread matrix where all na values have been set to zero
#' @export
#'
#' @examples na_to_zero(book_ratings_matrix)
na_to_zero <- function(mat){
  mat[is.na(mat)] <- 0
  mat
}
