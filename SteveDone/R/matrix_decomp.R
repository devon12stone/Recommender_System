#' Matrix Decomposition
#'
#' This functions returns the predicted ratings and RMSE for a matrix decomposition recommender system.
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
#'
#' @return This functions returns both the predicted ratings and RMSE of a matrix decomposition recommender system.
#' @export
#'
#' @examples matrix_decomp(book_ratings, changed_inds,1,'User.ID', 'ISBN', 'Book.Rating', 0, 0)
matrix_decomp <- function(ratings, changed, k_in ,user_col, item_col, rating_col, a, b){

  # create a spread matrix from ratings
  test_ratings = make_spread_matrix(ratings, user_col, item_col, rating_col)

  # use the NNLM package to compute the matrix decomposition of the spread book ratings matrix
  decom = nnmf(test_ratings, k=k_in, loss='mse', alpha=c(a,0,0), beta=c(b,0,0),
               check.k=FALSE)

  # predictions for W x H
  predictions = decom$W %*% decom$H

  # get the rmse of the system

  errors <- NULL
  for (name in row.names(changed)){
    # rating 1 for user
    actual1 = changed[name, 'rating1']
    # squared error 1
    errors <- append(errors, (predictions[name, changed[name, 'index1']] - actual1)^2 )
    # rating 2 for user
    actual2 = changed[name, 'rating2']
    # squared error 2
    errors <- append(errors, (predictions[name,changed[name, 'index2']] - actual2)^2 )
  }

  rmse = sqrt(mean(errors))

  # put predictions and rmse in a list
  values <- list("predictions" = predictions , "rmse" = rmse)

  return(values)
}
