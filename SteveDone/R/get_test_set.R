#' Get Test Set
#'
#' This function is used to create a test/validation set for a matrix decomposition recommender system.
#' @param ratings A dataset of users, items and ratings that has been removed of zero ratings.
#' @param user_col The name of the user ID column in the ratings dataframe.
#' @param item_col The name of the item ID column in the ratings dataframe.
#' @param rating_col The name of the ratings column in the ratings dataframe.
#'
#' @return A matrix in the form of the User ID, index of the user's first rating that was set to NA,
#' index of the user's second rating that was set to NA, original value of the user's first rating
#' and original value of the user's second rating.
#' @export
#'
#' @examples get_test_set(book_ratings,'User.ID', 'ISBN', 'Book.Rating')
get_test_set <- function(ratings, user_col, item_col, rating_col){

  # get the frequency of users in the set
  counts <- data.frame(table(ratings[,user_col]))
  # only inlcude users who appear more than once
  counts <- counts[counts$Freq > 3, ]

  # set the matrix of changed indicies and ratings
  changed_indicies <- matrix(0, nrow=0, ncol=4)
  colnames(changed_indicies) <- c('index1', 'index2', 'rating1', 'rating2')

  spread_ratings <- make_spread_matrix(ratings,user_col, item_col, rating_col)

  # randomly change ratings to na where users have more than 3 ratings
  # this will be the testing set
  for (row in 1:nrow(counts)){
    # userID
    userID = as.character(counts[row,'Var1'])
    # all indicies of a user than is not na
    all_inds = which(!is.na(spread_ratings[userID,]))
    # randomly sample two indicies
    inds_to_change  = sample(all_inds, 2)
    # actual ratings at the indicies
    actuals = spread_ratings[userID, inds_to_change]
    # set the indicies at the rating to be 2
    spread_ratings[userID, inds_to_change] <- NA
    # store the indicies in a matrix
    changed_indicies <- rbind(changed_indicies, c(inds_to_change, actuals))
    # set the row name of the matrix to be the user
    row.names(changed_indicies)[row] <- userID
  }

  changed_indicies

}
