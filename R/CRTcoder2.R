#' Automatic coding for Cognitive Reflection Test 2 open-ended response n.2
#'
#' Automatically codes responses to the second question of the Cognitive Reflection Test 2 (CRT-2) using predefined regex patterns. It categorizes responses into: 2 for intuitive but incorrect answers related to "7", 1 for the correct answer "8", and 3 for any other responses.
#'
#' @param risposta The textual response to the CRT-2 second question, as a character string.
#' @return An integer code representing the categorization of the response: 1 for correct (representing "eight"), 2 for impulsive but incorrect (representing "seven"), and 3 for other types of responses.
#' @examples
#' reflectR::CRTcoder2(c("7", "otto", "sette"))
#' @export
#' @importFrom stringr str_detect
CRTcoder2 <- function(risposta) {
  # Convert to lowercase
  risposta <- tolower(risposta)

  # Regex patterns
  regex.sette <- "\\bsette\\b|\\b7(\\.0+)?\\b"
  regex.otto <- "\\botto\\b|\\b8(\\.0+)?\\b"

  # Initialize the result vector
  result <- integer(length(risposta))

  # Apply the coding logic to each response
  for (i in seq_along(risposta)) {
    if (stringr::str_detect(risposta[i], regex.sette)) {
      result[i] <- 2
    } else if (stringr::str_detect(risposta[i], regex.otto)) {
      result[i] <- 1
    } else {
      result[i] <- 3
    }
  }

  return(result)
}

