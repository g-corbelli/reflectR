#' Automatic coding for Cognitive Reflection Test 2 open-ended responses
#'
#' Applies coding logic to any number of provided CRT question responses and supports multiple coding schemes. This function can output original coded responses, binary-coded responses, and aggregate scores based on these binary codings.
#'
#' @param crt1 Vector of responses to the first CRT question, or NULL if not provided.
#' @param crt2 Vector of responses to the second CRT question, or NULL if not provided.
#' @param crt3 Vector of responses to the third CRT question, or NULL if not provided.
#' @param crt4 Vector of responses to the fourth CRT question, or NULL if not provided.
#' @param codingscheme A character string indicating the desired coding scheme. Options are "categorical" for the original 1, 2, 3 coding, "sum" for a sum of binary-coded correct answers, or "mean" for an average of binary-coded correct answers. The default is "categorical".
#'
#' @return A list containing the coded and, if applicable, binary-coded responses for each provided CRT question. For "sum" or "mean" coding schemes, additional vectors representing these aggregate scores are included.
#' @examples
#' reflectR::CRTcoder(
#'   crt1 = c("al primo", "secondo", "1"),
#'   crt2 = c("7", "otto", "sette"),
#'   crt3 = c("primo", "carlo", "si chiama primo"),
#'   crt4 = c("nulla", "27 metri cubi", "zero"),
#'   codingscheme = "mean"
#' )
#' @export

CRTcoder <- function(crt1 = NULL, crt2 = NULL, crt3 = NULL, crt4 = NULL,
                     codingscheme = "categorical") {
  responses <- list(crt1 = crt1, crt2 = crt2, crt3 = crt3, crt4 = crt4)
  coded_responses <- list()

  # Apply individual coding functions to string vectors
  if (!is.null(crt1)) coded_responses$crt1_coded <- CRTcoder1(crt1)
  if (!is.null(crt2)) coded_responses$crt2_coded <- CRTcoder2(crt2)
  if (!is.null(crt3)) coded_responses$crt3_coded <- CRTcoder3(crt3)
  if (!is.null(crt4)) coded_responses$crt4_coded <- CRTcoder4(crt4)

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
