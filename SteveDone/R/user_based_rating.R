#' User Based Rating
#'
#' This funciton returns the top 10 rated user rated items of any user in already in the dataset used to create the recommender system.
#' @param user_id The ID of the user.
#' @param ratings Spread matrix in the form of users (rows), items (columns) and ratings (values).
#' @param items Dataframe of items and information on the items.
#'
#' @return Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
#' @export
#'
#' @examples user_based_rating(277042, book_ratings_matrix, book_info)
user_based_rating <- function(user_id, ratings, items){

  # turn into character if not already
  user = ifelse(is.character(user_id), user_id, as.character(user_id))

  users <- row.names(ratings)

  # get the similarities between the user and all other users
  similarities = unlist(lapply(X=users, FUN=all_sim, user, ratings))

  # set the similarity between the user and themselves to 0
  # if left would equal 1 and would skew the results
  # get the index of reader in
  user_index = match(user, users)
  similarities[user_index] = 0

  # get scores
  user_scores <- data.frame(ISBN = colnames(ratings),
                              score = as.vector(similarities %*% ratings),
                              seen = ratings[user,])

  # join the scores to item info dataframe to get the title of each item
  user_scores <- left_join(user_scores, items)

  # sort unseen items by score and remove the 'seen' column
  user_scores %>%
    filter(seen == 0) %>%
    arrange(desc(score)) %>%
    select(-seen) %>%
    head(10)
}
