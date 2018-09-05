#' Make Spread Matrix
#'
#' Makes a spread matrix from a data frame of users, items and ratings.
#' The rows of the spread matrix represent users, the columns represent items and the values represent ratings.
#' @param frame Dataframe of users, items and ratings.
#' @param user Column name of users column.
#' @param item Column name of items column.
#' @param rating Column name of items column.
#'
#' @return Returns a spread matrix.
#' @export
#'
#' @examples  make_spread_matrix(book_ratings,'User.ID', 'ISBN', 'Book.Rating')
make_spread_matrix <- function(frame,user, item, rating){
  mat = frame %>% select(user, item, rating) %>% spread(key = item, value = rating)
  row.names(mat) <- mat[,1]
  mat = as.matrix(mat[,-1])
  mat
}
