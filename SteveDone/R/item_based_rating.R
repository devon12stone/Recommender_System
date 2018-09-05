#' Item Based Rating
#'
#' This funciton returns the top 10 rated item rated items of any user in already in the dataset used to create the recommender system.
#' @param user_id The ID of the user.
#' @param ratings Spread matrix in the form of users (rows), items (columns) and ratings (values).
#' @param items Dataframe of items and information on the items.
#' @param similarities Similarity matrix of items in the system.
#'
#' @return Returns a sorted dataframe of the top 10 rated items, the score of the item and information on each item.
#' @export
#'
#' @examples
item_based_rating <- function(user_id, ratings, items, similarities){

  # turn into character if not already
  user = ifelse(is.character(user_id), user_id, as.character(user_id))

  # transpose the ratings matrix
  trans_ratings = t(ratings)

  # the books read by the reader
  user_seen = row.names(trans_ratings)[trans_ratings[ , user] > 0]

  # get the scores
  user_scores <- tibble(ISBN = row.names(trans_ratings),
                          score = apply(similarities[,user_seen], 1, sum),
                          seen = trans_ratings[,user])

  # join the scores to item info dataframe to get the title of each item
  user_scores <- left_join(user_scores, items)

  # sort unseen items by score and remove the 'seen' column
  user_scores %>%
    filter(seen == 0) %>%
    arrange(desc(score)) %>%
    select(-seen) %>%
    head(10)
}
