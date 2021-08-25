#' @title Read structure q files
#'
#' @description
#' strc_readq(.) read structure q file as produce by faststructure
#'
#' @details
#' more details to come ...
#'
#' @param file.path Path to the file
#'
#' @examples
#' # provide some examples of how to use your function
#' path.to.file <- "data-raw/results_job_k2_r1_q"
#' strc_readq(path.to.file)
#' @export

strc_readq <- function(file.path) {
  struct.int <- read.table(file.path, header = F ) 
  names(struct.int) <- c("ID", "PopNum", paste0("Q", 1:(base::ncol(struct.int) - 2)))

  struct.int$K <- (ncol(struct.int) - 2)
  
  res <- tidyr::pivot_longer(struct.int, stringr::str_subset(names(struct.int), "ID|PopNum|K", negate = T),
         names_to = "Qpop", values_to = "Qvalue")

      return(res)
}

#' @title Read structure summary file
#'
#' @description
#' strc_readsummary(.) read structure  summary file as produced by faststructure
#'
#' @details
#' more details to come ...
#'
#' @param file Path to the file
#'
#' @examples
#' # provide some examples of how to use your function
#' file.path <- ".results_job_k3_r1_q"
#' strc_readsummary(file.path)
#' @export

strc_readsummary <- function(file) {
  readr::read_csv(file,
    skip = 1
  ) %>% dplyr::select(-c(.data$`1`))
}
