#' New User Item Based Rating
#'
#'
#' This function adds a new user to the spread matrix of the item based recommender system.
#' Then the funciton returns the top 10 rated item rated items for the user.
#' @param user A dataframe in the form of items and ratings.
#' @param ratings Spread matrix in the form of users (rows), items (columns) and ratings (values).
#' @param items  Dataframe of items and information on the items.
#' @param similarities Similarity matrix of items in the system.
#' @param item_col The column name of the items column in the datafram containing the ratings.
#' @param rating_col The column name of the ratings column in the datafram containing the ratings
#'
#' @return Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
#' @export
#'
#' @examples new_user <- data.frame(ISBN= c('0440234743', '0971880107', '0345417623'), Book.Rating = c(2, 5, 3))
#'new_user_item_based(new_user, book_ratings_matrix, book_info, book_similarities, 'ISBN', 'Book.Rating')

new_user_item_based <- function(user, ratings, items, similarities, item_col, rating_col){

  # find the last user ID
  # assign the last user ID + 1 to the new user
  new_user_id = as.character(as.numeric(row.names(ratings)[nrow(ratings)]) + 1)

  # add new blank row to book ratings
  ratings <- rbind(ratings, 0)

  # add row name to new user's row
  row.names(ratings)[nrow(ratings)] = new_user_id

  # this loop adds the users's ratings to the ratings matrix in the correct place
  for (i in 1:nrow(user)) {
    ISBN = as.character(user[i, item_col])
    rating = user[i, rating_col]
    ratings[new_user_id, ISBN] <- rating
  }

  # then call current user function
  item_based_rating(new_user_id, ratings, items, similarities)
}
