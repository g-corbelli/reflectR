#' Automatic coding for Cognitive Reflection Test 2 (Thomson & Oppenheimer, 2016) open-ended responses
#'
#' Applies coding logic to any number of provided CRT question responses and supports multiple coding schemes. This function can output original coded responses, binary-coded responses, and aggregate scores based on these binary codings.
#'
#' @importFrom stringr str_detect
#'
#' @param item1 Vector of responses to the first CRT question, or NULL if not provided.
#' @param item2 Vector of responses to the second CRT question, or NULL if not provided.
#' @param item3 Vector of responses to the third CRT question, or NULL if not provided.
#' @param item4 Vector of responses to the fourth CRT question, or NULL if not provided.
#' @param codingscheme A character string indicating the desired coding scheme. Options are "categ" for the original 1, 2, 3 coding, "sum" for a sum of binary-coded correct answers, or "mean" for an average of binary-coded correct answers. The default is "categ".
#'
#' @return A list containing the coded and, if applicable, binary-coded responses for each provided CRT question. For "sum" or "mean" coding schemes, additional vectors representing these aggregate scores are included.
#' @note Developed by Giuseppe Corbelli, email: giuseppe.corbelli@uninettunouniversity.net
#' @examples
#' # Automated scoring for CRTtwo responses using the categorical coding scheme:
#' reflectR::CRTtwo(
#' item1 = c("first place", "second place", "1", "seconddd", "meh"),
#' item2 = c("7", "eightt", "seven", NA, "8"),
#' item3 = c("emily", "i think emily", "JUNEE", "maybe june", "the name is emily"),
#' item4 = c("nothing", "27 sqmt", "0", "it's empty", "i suck at math"),
#' codingscheme = "categ")
#'
#' # Compute the sum score for CRTtwo responses based on binary-coded correctness:
#' reflectR::CRTtwo(
#' item1 = c("first place", "second place", "1", "seconddd", "meh"),
#' item2 = c("7", "eightt", "seven", NA, "8"),
#' item3 = c("emily", "i think emily", "JUNEE", "maybe june", "the name is emily"),
#' item4 = c("nothing", "27 sqmt", "0", "it's empty", "i suck at math"),
#' codingscheme = "sum")$crt_sum
#' @export

CRTtwo <- function(item1 = NULL, item2 = NULL, item3 = NULL, item4 = NULL,
                     codingscheme = "categ") {

  CRTcoder1 <- function(risposta) {
    risposta <- tolower(risposta)
    regex.corretto <- "second|two|2"
    regex.impulsivo <- "first|one|win|1"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i])) {
        result[i] <- 3
      } else if (stringr::str_detect(risposta[i], regex.impulsivo)) {
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
    regex.corretto <- "eight|8"
    regex.impulsivo <- "seven|7"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i])) {
        result[i] <- 3
      } else if (stringr::str_detect(risposta[i], regex.impulsivo)) {
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
    regex.corretto <- "emily"
    regex.impulsivo <- "june"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i])) {
        result[i] <- 3
      } else if (stringr::str_detect(risposta[i], regex.impulsivo)) {
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
    regex.corretto <- "0|zero|nothing|not|empty|doesnt|any"
    regex.impulsivo <- "^(?!.*\\b(0|zero|nothing|not|empty|doesnt|any)\\b).*\\d+.*"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i])) {
        result[i] <- 3
      } else if (stringr::str_detect(risposta[i], regex.impulsivo)) {
        result[i] <- 2
      } else if (stringr::str_detect(risposta[i], regex.corretto)) {
        result[i] <- 1
      } else {
        result[i] <- 3
      }
    }
    return(result)
  }

  responses <- list(item1 = item1, item2 = item2, item3 = item3, item4 = item4)
  coded_responses <- list()

  # Apply individual coding functions to string vectors
  if (!is.null(item1)) coded_responses$item1_coded <- CRTcoder1(item1)
  if (!is.null(item2)) coded_responses$item2_coded <- CRTcoder2(item2)
  if (!is.null(item3)) coded_responses$item3_coded <- CRTcoder3(item3)
  if (!is.null(item4)) coded_responses$item4_coded <- CRTcoder4(item4)

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
