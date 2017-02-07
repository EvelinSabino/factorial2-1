#' @title Es una funcion?
#' @description Esto me permite saber si es un numero natural
#' @param n este es el numero
#' @export
#' @examples
#' is_natural(20)
#'
is_natural <- function(n){
  (!grepl("[^[:digit:]]", format(n,  digits = 20, scientific = FALSE)))&&(n>=0)
}

#' @title Factorial con loop
#' @description Esto me permite saber el factorial
#' @param n este es el numero
#' @export
#' @examples
#' Factorial_loop(20)
#'
#'
# Base version using a for loop
Factorial_loop <- function(n){
  if (!is_natural(n)) {
    stop("Need a Natural Number")
  }

  if (n == 0) {
    return (1)
  } else {
    fact <- 1
    for (x in 1:n) {
      fact <- fact*x
    }
    return (fact)
  }
}

#' @title Factorial
#' @description Esto me permite saber el factorial
#' @param n este es el numero
#' @importFrom purrr reduce map
#' @examples
#' Factorial_reduce(20)
#'
#'@export
# Using reduce functionality from purrr
Factorial_reduce <- function(n){
  if (!is_natural(n)) {
    stop("Need a Natural Number")
  }

  if (n == 0) {
    return (1)
  } else {
    fact <- purrr::reduce(as.double(1:n), function(x,y){x*y})
  }
  return (fact)
}



