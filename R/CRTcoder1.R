#' Automatic coding for Cognitive Reflection Test 2 open-ended response n.1
#'
#' This function automatically codes responses to the first question of the Cognitive Reflection Test 2 (CRT-2), based on predefined regex patterns. It categorizes responses as: 2 for intuitive but incorrect (e.g., "first"), 1 for correct (e.g., "second"), and 3 for any other response.
#'
#' @param risposta The textual response to the CRT-2 first question, as a character string.
#' @return An integer code representing the categorization of the response: 1 for correct, 2 for impulsive but incorrect, and 3 for other responses.
#' @examples
#' reflectR::CRTcoder1(c("al primo", "secondo", "1"))
#' @export
#' @importFrom stringr str_detect
CRTcoder1 <- function(risposta) {

  risposta <- tolower(risposta)

  regex.primoposto <- "\\bprim[ao]?\\b|\\b1\\b|\\buno\\b|\\b1\\b|\\bcima\\b|\\bfronte\\b|\\bvinc\\b|\\bdavanti\\b|\\bno\\.? 1\\b|\\bin testa\\b|\\bprima posizione\\b"
  regex.secondoposto <- "\\bsecondo\\b|\\b2\\b|\\bdue\\b|\\bal secondo posto\\b|\\bsecond\\b|\\bseconda\\b"

  result <- integer(length(risposta))

  for (i in seq_along(risposta)) {
    if (stringr::str_detect(risposta[i], regex.primoposto)) {
      result[i] <- 2
    } else if (stringr::str_detect(risposta[i], regex.secondoposto)) {
      result[i] <- 1
    } else {
      result[i] <- 3
    }
  }

  return(result)
}

