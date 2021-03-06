#' @title Read structure q files
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' strc_readq(.) read structure q file as produce by faststructure
#'
#' @details
#' more details to come ...
#'
#' @param file.path Path to the file
#'
#' @examples
#' # provide some examples of how to use your function
#' path.to.file <- system.file("extdata", "results_job_k2_r1_q", package = "QuickPop")
#' strc_readq(path.to.file)
#' @export

strc_readq <- function(file.path) {
  struct.int <- utils::read.table(file.path, header = F ) 
  names(struct.int) <- c("ID", "PopNum", paste0("Q", 1:(base::ncol(struct.int) - 2)))

  struct.int$K <- (ncol(struct.int) - 2)
  
  res <- tidyr::pivot_longer(struct.int, stringr::str_subset(names(struct.int), "ID|PopNum|K", negate = T),
         names_to = "Qpop", values_to = "Qvalue")

      return(res)
}

#' @title Read structure summary file
#'
#' @description
#' strc_readsummary(.) read structure summary file as produced by faststructure
#'
#' @details
#' Read directly the file as produced by faststructure package
#'
#' @param file.path Path to the file
#'
#' @examples
#' # provide some examples of how to use your function
#' #path.to.file <- "data-raw/results_summary.csv"
#' path.to.file <- system.file("extdata", "results_summary.csv", package = "QuickPop")
#' strc_readsummary(path.to.file)
#' @export

strc_readsummary <- function(file.path) {
  readr::read_csv(file.path,
    skip = 1#, show_col_types = FALSE
  ) %>% dplyr::select(-c(.data$`1`))
}
