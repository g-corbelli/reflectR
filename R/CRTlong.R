#' Automatic coding for Cognitive Reflection Test LONG (Primi et al., 2016) open-ended responses
#'
#' Applies coding logic to any number of provided CRT question responses and supports multiple coding schemes. This function can output original coded responses, binary-coded responses, and aggregate scores based on these binary codings.
#'
#' @importFrom stringr str_detect
#'
#' @param item1 Vector of responses to the first CRT question, or NULL if not provided.
#' @param item2 Vector of responses to the second CRT question, or NULL if not provided.
#' @param item3 Vector of responses to the third CRT question, or NULL if not provided.
#' @param item4 Vector of responses to the fourth CRT question, or NULL if not provided.
#' @param item5 Vector of responses to the fifth CRT question, or NULL if not provided.
#' @param item6 Vector of responses to the sixth CRT question, or NULL if not provided.
#' @param codingscheme A character string indicating the desired coding scheme. Options are "categ" for the original 1, 2, 3 coding, "sum" for a sum of binary-coded correct answers, or "mean" for an average of binary-coded correct answers. The default is "categ".
#' @param na.rm Logical, indicating whether to treat missing values as empty responses or preserve them as missing information. When TRUE, NAs are coded as "other" incorrect responses; when FALSE, NAs are preserved. Default is TRUE.
#'
#' @return A list containing the coded and, if applicable, binary-coded responses for each provided CRT question. For "sum" or "mean" coding schemes, additional vectors representing these aggregate scores are included.
#' @note Developed by Giuseppe Corbelli, email: giuseppe.corbelli@uninettunouniversity.net, giuseppe.corbelli@uniroma1.it
#' @examples
#' # Automated scoring for CRTlong responses using the categorical coding scheme:
#' reflectR::CRTlong(
#' item1 = c("five", "5 cents", "10"),
#' item2 = c("5", "one hundred", "100"),
#' item3 = c("47", "24", "forty seven"),
#' item4 = c("3 elves", "dunno", "six"),
#' item5 = c("29", "thirty", "30"),
#' item6 = c("15", "fifteen", "0"),
#' codingscheme = "categ",
#' na.rm = TRUE)
#'
#' # Compute the sum score for CRTlong responses based on binary-coded correctness:
#' reflectR::CRTlong(
#' item1 = c("five", "5 cents", "10"),
#' item2 = c("5", "one hundred", "100"),
#' item3 = c("47", "24", "forty seven"),
#' item4 = c("3 elves", "dunno", "six"),
#' item5 = c("29", "thirty", "30"),
#' item6 = c("15", "fifteen", "0"),
#' codingscheme = "sum",
#' na.rm = FALSE)$crt_sum
#' @export

CRTlong <- function(item1 = NULL, item2 = NULL, item3 = NULL, item4 = NULL,
                   item5 = NULL, item6 = NULL,
                     codingscheme = "categ", na.rm = TRUE) {

  CRTcoder1 <- function(risposta, na.rm) {
    risposta <- tolower(risposta)
    regex.corretto <- "five|5"
    regex.impulsivo <- "ten|10"
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
    regex.corretto <- "five|5"
    regex.impulsivo <- "hundred|\\b100\\b"
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
    regex.corretto <- "fortyseven|forty-seven|forty seven|47"
    regex.impulsivo <- "twentyfour|twenty-four|twenty four|24"
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
    regex.corretto <- "three|3"
    regex.impulsivo <- "six|6"
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

  CRTcoder5 <- function(risposta, na.rm) {
    risposta <- tolower(risposta)
    regex.corretto <- "twentynine|twenty-nine|twenty nine|29"
    regex.impulsivo <- "thirty|30"
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

  CRTcoder6 <- function(risposta, na.rm) {
    risposta <- tolower(risposta)
    regex.corretto <- "fifteen|15"
    regex.impulsivo <- "twenty|20"
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

  responses <- list(item1 = item1, item2 = item2, item3 = item3, item4 = item4,
                    item5 = item5, item6 = item6)
  coded_responses <- list()

  # Apply individual coding functions to string vectors
  if (!is.null(item1)) coded_responses$item1_coded <- CRTcoder1(item1, na.rm)
  if (!is.null(item2)) coded_responses$item2_coded <- CRTcoder2(item2, na.rm)
  if (!is.null(item3)) coded_responses$item3_coded <- CRTcoder3(item3, na.rm)
  if (!is.null(item4)) coded_responses$item4_coded <- CRTcoder4(item4, na.rm)
  if (!is.null(item5)) coded_responses$item5_coded <- CRTcoder5(item5, na.rm)
  if (!is.null(item6)) coded_responses$item6_coded <- CRTcoder6(item6, na.rm)

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
