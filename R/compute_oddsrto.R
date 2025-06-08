#' the compute_oddsrto is used to generate the data frame with odds ratio by adverse event
#'
#' @param .data pass any data frame
#' @param aeterm pass any ae column name
#'
#' @import dplyr
#' @import purrr
#' @import epiR
#' @import tibble
#'
#' @importFrom stats setNames
#' @importFrom rlang ensym
#' @importFrom magrittr `%>%`
#'
#' @returns a data frame
#'
#' @examples
#' compute_oddsrto(.data=ae01, aeterm='aebodsys')
#' @export
compute_oddsrto <- function(.data, aeterm){

  if (is.null(.data )){
    stop('data frame is empty')
  }

  rlang::arg_match(aeterm, names(.data) )

  aeterm1 <- rlang::ensym(aeterm)

  listoflsit <- .data %>% group_split(!!aeterm1) %>% setNames(unique(.data[[aeterm]]))


  purrr::map2_dfr(1:length(listoflsit), names(listoflsit) ,.f= function(x,y) {
  # browser()
  a <- listoflsit[[x]][4]$Event[1]
  b <- listoflsit[[x]][3]$No_Event[1]
  c <- listoflsit[[x]][4]$Event[2]
  d <- listoflsit[[x]][3]$No_Event[2]


  mat <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE,
                dimnames = list(Treatment = c("Drug", "Placebo"),
                                Outcome = c("Event", "No Event")))


  lst <- epiR::epi.2by2(mat, method = "cohort.count", conf.level = 0.95)

  df <- tibble::tibble(aebodsys=y,
                       estimate=lst$massoc.detail$OR.strata.wald$est,
                       lower=lst$massoc.detail$OR.strata.wald$lower,
                       upper=lst$massoc.detail$OR.strata.wald$upper,
                       pvalue=lst$massoc.detail$chi2.strata.fisher$p.value.2s)

  return(df)

})
}


