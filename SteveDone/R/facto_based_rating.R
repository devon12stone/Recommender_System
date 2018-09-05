#' Matrix Factorization Based Rating
#'
#' @param user User ID.
#' @param ratings A dataset of users, items and ratings that has been removed of zero ratings.
#' @param changed A matrix in the form of the User ID, index of the user's first rating that was set to NA,
#' index of the user's second rating that was set to NA, original value of the user's first rating
#' and original value of the user's second rating.
#' @param k The integer of decomposition rank of the decomposition matrix.
#' @param user_col The name of the user ID column in the ratings dataframe.
#' @param item_col The name of the item ID column in the ratings dataframe.
#' @param rating_col The name of the ratings column in the ratings dataframe.
#' @param a The L2 regularisation of W latent factors for the matrix decomposition.
#' @param b The L2 regularisation of H latent factors for the matrix decomposition
#' @param items Dataframe of items and information on the items.
#'
#' @return Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
#' @export
#'
#' @examples facto_based_rating(277042 ,book_ratings, changed_inds,5,'User.ID', 'ISBN', 'Book.Rating', 0.2,0.2, book_info)
facto_based_rating <- function(user,ratings, changed, k_in ,user_col, item_col, rating_col, a, b, items){

  # turn into character if not already
  user = ifelse(is.character(user), user, as.character(user))

  # first complete the matrix decomposition
  decom = matrix_decomp(ratings, changed, k_in ,user_col, item_col, rating_col, a, b)

  # get a spread matrix of original ratings
  spread_mat = make_spread_matrix(ratings, user_col, item_col, rating_col)

  # get predictions
  preds = decom$predictions

  # get ratings
  user_ratings <- data.frame(ISBN = colnames(preds),
                            score = as.vector(preds[user,]),
                            seen = spread_mat[user,])

  # join the scores to item info dataframe to get the title of each item
  user_ratings <- left_join(user_ratings, items)

  # sort unseen items by score and remove the 'seen' column
  user_ratings %>%
    filter(is.na(seen)) %>%
    arrange(desc(score)) %>%
    select(-seen) %>%
    head(10)


}
