#' Automatic coding for Cognitive Reflection Test LONG open-ended responses
#'
#' Applies coding logic to any number of provided CRT question responses and supports multiple coding schemes. This function can output original coded responses, binary-coded responses, and aggregate scores based on these binary codings.
#'
#' @param item1 Vector of responses to the first CRT question, or NULL if not provided.
#' @param item2 Vector of responses to the second CRT question, or NULL if not provided.
#' @param item3 Vector of responses to the third CRT question, or NULL if not provided.
#' @param item4 Vector of responses to the fourth CRT question, or NULL if not provided.
#' @param item5 Vector of responses to the fifth CRT question, or NULL if not provided.
#' @param item6 Vector of responses to the sixth CRT question, or NULL if not provided.
#' @param codingscheme A character string indicating the desired coding scheme. Options are "categorical" for the original 1, 2, 3 coding, "sum" for a sum of binary-coded correct answers, or "mean" for an average of binary-coded correct answers. The default is "categorical".
#'
#' @return A list containing the coded and, if applicable, binary-coded responses for each provided CRT question. For "sum" or "mean" coding schemes, additional vectors representing these aggregate scores are included.
#' @examples
#' reflectR::CRTtwo(
#'   item1 = c("al primo", "secondo", "1"),
#'   item2 = c("7", "otto", "sette"),
#'   item3 = c("primo", "carlo", "si chiama primo"),
#'   item4 = c("nulla", "27 metri cubi", "zero"),
#'   codingscheme = "mean"
#' )
#' @export

CRTlong <- function(item1 = NULL, item2 = NULL, item3 = NULL, item4 = NULL,
                   item5 = NULL, item6 = NULL,
                     codingscheme = "categ") {

  CRTcoder1 <- function(risposta) {
    risposta <- tolower(risposta)
    regex.corretto <- "\\bfive\\b|\\b5\\b|\\bfiver\\b"
    regex.impulsivo <- "\\b10\\b|\\bten\\b"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (stringr::str_detect(risposta[i], regex.impulsivo)) {
        result[i] <- 2
      } else if (stringr::str_detect(risposta[i], regex.corretto)) {
        result[i] <- 1
      } else {
        result[i] <- 3
      }
    }
    return(result)
  }

  CRTcoder2 <- function(risposta) {
    risposta <- tolower(risposta)
    regex.corretto <- "\\bfive\\b|\\b5\\b|\\bfiver\\b"
    regex.impulsivo <- "\\bhundred\\b|\\b100\\b"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (stringr::str_detect(risposta[i], regex.impulsivo)) {
        result[i] <- 2
      } else if (stringr::str_detect(risposta[i], regex.corretto)) {
        result[i] <- 1
      } else {
        result[i] <- 3
      }
    }
    return(result)
  }

  CRTcoder3 <- function(risposta) {
    risposta <- tolower(risposta)
    regex.corretto <- "\\bfortyseven\\b|\\b47\\b|\\bforty seven\\b|\\bforty-seven\\b"
    regex.impulsivo <- "\\btwentyfour\\b|\\b24\\b|\\btwenty-four\\b|\\btwenty four\\b"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (stringr::str_detect(risposta[i], regex.impulsivo)) {
        result[i] <- 2
      } else if (stringr::str_detect(risposta[i], regex.corretto)) {
        result[i] <- 1
      } else {
        result[i] <- 3
      }
    }
    return(result)
  }

  CRTcoder4 <- function(risposta) {
    risposta <- tolower(risposta)
    regex.corretto <- "\\bthree\\b|\\b3\\b"
    regex.impulsivo <- "\\bsix\\b|\\b6\\b"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (stringr::str_detect(risposta[i], regex.impulsivo)) {
        result[i] <- 2
      } else if (stringr::str_detect(risposta[i], regex.corretto)) {
        result[i] <- 1
      } else {
        result[i] <- 3
      }
    }
    return(result)
  }

  CRTcoder5 <- function(risposta) {
    risposta <- tolower(risposta)
    regex.corretto <- "\\btwentynine\\b|\\b29\\b|\\btwenty nine\\b|\\btwenty-nine\\b"
    regex.impulsivo <- "\\bthirty\\b|\\b30\\b"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (stringr::str_detect(risposta[i], regex.impulsivo)) {
        result[i] <- 2
      } else if (stringr::str_detect(risposta[i], regex.corretto)) {
        result[i] <- 1
      } else {
        result[i] <- 3
      }
    }
    return(result)
  }

  CRTcoder6 <- function(risposta) {
    risposta <- tolower(risposta)
    regex.corretto <- "\\bfifteen\\b|\\b15\\b"
    regex.impulsivo <- "\\btwenty\\b|\\b20\\b"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (stringr::str_detect(risposta[i], regex.impulsivo)) {
        result[i] <- 2
      } else if (stringr::str_detect(risposta[i], regex.corretto)) {
        result[i] <- 1
      } else {
        result[i] <- 3
      }
    }
    return(result)
  }

  responses <- list(item1 = item1, item2 = item2, item3 = item3, item4 = item4,
                    item5 = item5, item6 = item6)
  coded_responses <- list()

  # Apply individual coding functions to string vectors
  if (!is.null(item1)) coded_responses$item1_coded <- CRTcoder1(item1)
  if (!is.null(item2)) coded_responses$item2_coded <- CRTcoder2(item2)
  if (!is.null(item3)) coded_responses$item3_coded <- CRTcoder3(item3)
  if (!is.null(item4)) coded_responses$item4_coded <- CRTcoder4(item4)
  if (!is.null(item5)) coded_responses$item5_coded <- CRTcoder5(item5)
  if (!is.null(item6)) coded_responses$item6_coded <- CRTcoder6(item6)

  # For "sum" and "mean" coding schemes, also calculate binary correctness codings
  if (codingscheme %in% c("sum", "mean")) {
    binary_responses <- lapply(coded_responses, function(x) ifelse(x == 1, 1, 0))
    names(binary_responses) <- sub("_coded", "_binary", names(binary_responses))

    # Add any binary responses to the output
    coded_responses <- c(coded_responses, binary_responses)

    # Convert binary_responses to a binary matrix for rowMeans and rowSums
    binary_matrix <- do.call(cbind, binary_responses)

    # Calculate sum or mean vector
    if (codingscheme == "sum") {
      coded_responses$crt_sum <- rowSums(binary_matrix, na.rm = TRUE)
    } else if (codingscheme == "mean") {
      # Calculate mean considering the actual number of questions answered
      coded_responses$crt_mean <- rowMeans(binary_matrix, na.rm = TRUE)
    }
  }

  return(coded_responses)
}
