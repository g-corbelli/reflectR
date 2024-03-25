#' Automatic coding for Cognitive Reflection Test 2 open-ended response n.3
#'
#' Automatically codes responses to the third question of the Cognitive Reflection Test 2 (CRT-2) based on predefined regex patterns. It categorizes responses into: 2 for the common intuitive but incorrect answer (e.g., "primo"), 1 for the correct answer ("carlo"), and 3 for any other type of response.
#'
#' @param risposta The textual response to the CRT-2 third question, as a character string.
#' @return An integer code representing the categorization of the response: 1 for correct (indicating "carlo"), 2 for impulsive but incorrect (e.g., "primo" or similar), and 3 for other responses.
#' @examples
#' reflectR::CRTcoder3(c("primo", "carlo", "si chiama primo"))
#' @export
#' @importFrom stringr str_detect
CRTcoder3 <- function(risposta) {
  # Convert to lowercase
  risposta <- tolower(risposta)

  # Regex patterns for the intuitive-incorrect and correct responses
  regex.primo <- "\\bprim[ao]?\\b"
  regex.carlo <- "\\bcarlo\\b"

  # Initialize the result vector
  result <- integer(length(risposta))

  # Apply the coding logic to each response
  for (i in seq_along(risposta)) {
    if (stringr::str_detect(risposta[i], regex.primo)) {
      result[i] <- 2
    } else if (stringr::str_detect(risposta[i], regex.carlo)) {
      result[i] <- 1
    } else {
      result[i] <- 3
    }
  }

  return(result)
}

