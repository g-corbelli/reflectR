#' Automatic coding for Cognitive Reflection Test 2 (Thomson & Oppenheimer, 2016) open-ended responses (Italian language)
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
#' @param na.rm Logical, indicating whether to treat missing values as empty responses or preserve them as missing information. When TRUE, NAs are coded as "other" incorrect responses; when FALSE, NAs are preserved. Default is TRUE.
#'
#' @return A list containing the coded and, if applicable, binary-coded responses for each provided CRT question. For "sum" or "mean" coding schemes, additional vectors representing these aggregate scores are included.
#' @note Developed by Giuseppe Corbelli, email: giuseppe.corbelli@uninettunouniversity.net, giuseppe.corbelli@uniroma1.it
#' @examples
#' # Automated scoring for itaCRTtwo responses using the categorical coding scheme:
#' reflectR::itaCRTtwo(
#' item1 = c("al primo", "secondo", "1", NA),
#' item2 = c("7", "non so", "sette", "otto"),
#' item3 = c("pprimo", "carlo", "CARLOO", "si chiama boh"),
#' item4 = c("nulla", "27 metri cubi", "mille", "zero"),
#' codingscheme = "categ",
#' na.rm = TRUE)
#'
#' # Compute the sum score for itaCRTtwo responses based on binary-coded correctness:
#' reflectR::itaCRTtwo(
#' item1 = c("al primo", "secondo", "1", NA),
#' item2 = c("7", "non so", "sette", "otto"),
#' item3 = c("primo", "carlo", "CARLOO", "si chiama boh"),
#' item4 = c("nulla", "27 metri cubi", "mille", "zero"),
#' codingscheme = "sum",
#' na.rm = FALSE)$crt_sum
#' @export

itaCRTtwo <- function(item1 = NULL, item2 = NULL, item3 = NULL, item4 = NULL,
                     codingscheme = "categ", na.rm = TRUE) {

  CRTcoder1 <- function(risposta, na.rm) {
    risposta <- tolower(risposta)
    regex.impulsivo <- "prim|1|uno|testa"
    regex.corretto <- "second|2|due"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i]) && !na.rm) {
        result[i] <- NA
      } else if (is.na(risposta[i])) {
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

  CRTcoder2 <- function(risposta, na.rm) {
    risposta <- tolower(risposta)
    regex.impulsivo <- "sette|7"
    regex.corretto <- "otto|8"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i]) && !na.rm) {
        result[i] <- NA
      } else if (is.na(risposta[i])) {
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

  CRTcoder3 <- function(risposta, na.rm) {
    risposta <- tolower(risposta)
    regex.impulsivo <- "prim"
    regex.corretto <- "carlo"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i]) && !na.rm) {
        result[i] <- NA
      } else if (is.na(risposta[i])) {
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

  CRTcoder4 <- function(risposta, na.rm) {
    risposta <- tolower(risposta)
    regex.impulsivo <- "\\b[1-9]\\d*(\\.\\d+)?\\b|\\b0\\.\\d+\\b"
    regex.corretto <- "zero|niente|nulla|vuot\\b|\\b0\\b"
    result <- integer(length(risposta))
    for (i in seq_along(risposta)) {
      if (is.na(risposta[i]) && !na.rm) {
        result[i] <- NA
      } else if (is.na(risposta[i])) {
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
  if (!is.null(item1)) coded_responses$item1_coded <- CRTcoder1(item1, na.rm)
  if (!is.null(item2)) coded_responses$item2_coded <- CRTcoder2(item2, na.rm)
  if (!is.null(item3)) coded_responses$item3_coded <- CRTcoder3(item3, na.rm)
  if (!is.null(item4)) coded_responses$item4_coded <- CRTcoder4(item4, na.rm)

  # For "sum" and "mean" coding schemes, also calculate binary correctness codings
  if (codingscheme %in% c("sum", "mean")) {
    binary_responses <- lapply(coded_responses, function(x) ifelse(x == 1, 1, ifelse(x == 0 | is.na(x), NA, 0)))
    names(binary_responses) <- sub("_coded", "_binary", names(binary_responses))

    # Add any binary responses to the output
    coded_responses <- c(coded_responses, binary_responses)

    # Convert binary_responses to a binary matrix for rowMeans and rowSums
    binary_matrix <- do.call(cbind, binary_responses)

    # Calculate sum or mean vector
    if (codingscheme == "sum") {
      coded_responses$crt_sum <- rowSums(binary_matrix, na.rm = na.rm)
    } else if (codingscheme == "mean") {
      # Calculate mean considering the actual number of questions answered
      coded_responses$crt_mean <- rowMeans(binary_matrix, na.rm = na.rm)
    }
  }

  return(coded_responses)
}
