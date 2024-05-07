#' Automatic Scoring of the Cognitive Reflection Test
#'
#' The reflectR package provides automated coding tools for open-ended responses
#' to various forms of the Cognitive Reflection Test (CRT). This package facilitates
#' the standardization of Cognitive Reflection Test responses analysis across
#' large datasets in cognitive psychology, decision-making, and related fields.
#' By automating the coding process, it not only reduces manual effort but also
#' aims to reduce the variability introduced by subjective interpretation of
#' open-ended responses, contributing to a more consistent and reliable analysis.
#'
#' The package includes functions to code responses from:
#' \itemize{
#'   \item \code{\link{CRT}}: Original CRT version by Frederick (2005)
#'   \item \code{\link{CRT4}}: 4-item CRT version by Toplak et al. (2014)
#'   \item \code{\link{CRT7}}: 7-item expanded CRT version by Toplak et al. (2014)
#'   \item \code{\link{CRTlong}}: CRT LONG version by Primi et al. (2016)
#'   \item \code{\link{CRTtwo}}: CRT version 2 by Thomson & Oppenheimer (2016)
#'   \item \code{\link{itaCRTtwo}}: Italian version of CRT version 2 by Thomson & Oppenheimer (2016)
#' }
#'
#' @section Note:
#' While reflectR draws inspiration from the scientific literature on the CRT,
#' it has been independently developed and does not hold any affiliation with
#' the original authors of these tests.
#'
#' @section Acknowledgments:
#' The development of this package benefited significantly from the kind insight
#' and suggestion provided by Dr. Keela Thomson, whose contribution is gratefully
#' acknowledged.
#' Special thanks are also due to Dr. Paolo Giovanni Cicirelli,
#' Prof. Marinella Paciello, Dr. Carmela Sportelli, and Prof. Francesca D'Errico.
#'
#' @references
#' Frederick, S. (2005). Cognitive Reflection and Decision Making. \emph{Journal of Economic Perspectives}, 19(4), 25-42. doi:10.1257/089533005775196732
#'
#' Toplak, M. E., West, R. F., & Stanovich, K. E. (2014). Assessing miserly information processing: An expansion of the Cognitive Reflection Test. \emph{Thinking & Reasoning}, 20(2), 147-168. doi:10.1080/13546783.2013.844729
#'
#' Primi, C., Morsanyi, K., Chiesi, F., Donati, M. A., & Hamilton, J. (2016). The development and testing of a new version of the Cognitive Reflection Test applying item response theory (IRT). \emph{Journal of Behavioral Decision Making}, 29(5), 453-469. doi:10.1002/bdm.1883
#'
#' Thomson, K. S., & Oppenheimer, D. M. (2016). Investigating an alternate form of the cognitive reflection test. \emph{Judgment and Decision Making}, 11(1), 99-113. doi:10.1017/s1930297500007622
#'
#' @docType package
#' @name reflectR
#' @aliases reflectR-package reflectR
"_PACKAGE"
