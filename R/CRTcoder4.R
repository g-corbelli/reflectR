#' Automatic coding for Cognitive Reflection Test 2 open-ended response n.4
#'
#' Automatically codes responses to the fourth question of the Cognitive Reflection Test 2 (CRT-2), distinguishing between intuitive but incorrect numerical responses and the correct response "zero". It categorizes responses as: 2 for any calculation or non-zero number, 1 for the correct answer "zero", and 3 for any other type of response.
#'
#' @param risposta The textual response to the CRT-2 fourth question, as a character string.
#' @return An integer code representing the categorization of the response: 1 for correct (specifically "zero"), 2 for impulsive but incorrect numerical answers, and 3 for other responses.
#' @examples
#' reflectR::CRTcoder4(c("nulla", "27 metri cubi", "zero"))
#' @export
#' @importFrom stringr str_detect
CRTcoder4 <- function(risposta) {
  # Convert to lowercase
  risposta <- tolower(risposta)

  # Regex patterns for intuitive-incorrect answers and the correct answer
  regex.calcoli <- "(?<!\\d)(?!0+(?:\\.0+)?\\b)(\\d+|zero)(\\.\\d+)?(\\s*(cm3|m3|metri cubi|mq|cm|metri|m|cubi))?|\\d+\\^\\d+|\\,\\d+|\\.\\d+"
  regex.zero <- "\\b0\\b|\\bzero\\b"

  # Initialize the result vector
  result <- integer(length(risposta))

  # Apply the coding logic to each response
  for (i in seq_along(risposta)) {
    if (stringr::str_detect(risposta[i], regex.calcoli)) {
      result[i] <- 2
    } else if (stringr::str_detect(risposta[i], regex.zero)) {
      result[i] <- 1
    } else {
      result[i] <- 3
    }
  }

  return(result)
}

