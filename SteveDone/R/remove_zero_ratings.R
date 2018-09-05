#' Remove Zero Ratings
#'
#' Removes zero ratings from a dataframe containing users, items, and ratings.
#' @param frame This is a dataframe containing users, items and ratings
#' @param rating Column name of the column containing the ratings.
#'
#' @return Dataframe with zero ratings removed
#' @export
#'
#' @examples remove_zero_ratings(book_ratings, 'Book.Rating')
remove_zero_ratings <- function(frame, rating){
  frame <- frame[frame[,rating] != 0, ]
}
