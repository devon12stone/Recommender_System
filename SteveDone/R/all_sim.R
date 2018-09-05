#' Cosine similarty between two users in a recommender system.
#'
#' This function isolates two users in a recommender system and returns their cosine similarity.
#' @param a User ID of first user.
#' @param b User ID of second user.
#' @param mat Spread matrix in the form of users, items, ratings.
#'
#' @return Returns the cosine similarity of two users.
#' @export
#'
#' @examples
all_sim <- function(a, b, mat){
  vec1 = mat[a, ]
  vec2 = mat[b, ]
  cosine_sim(vec1, vec2)
}
