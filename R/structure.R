#' @title Read structure q files
#'
#' @description
#' strc_readq(.) read structure q file as produce by faststructure
#'
#' @details
#' more details to come ...
#'
#' @param file Path to the file
#'
#' @examples
#' # provide some examples of how to use your function
#' file.path <- "results_job_k3_r1_q"
#' strc_readq(file.path)
#' @export

strc_readq <- function(file) {
  struct.int <- readr::read_delim(file,
    delim = " ", trim_ws = T,
    col_names = F
  ) %>%
    dplyr::select(-c(paste0("X", ncol(.data))))
  names(struct.int) <- c("ID_GQ", "PopNum", paste0("Q", 1:(base::ncol(struct.int) - 2)))

  res <- struct.int %>%
    dplyr::mutate(K = ncol(.) - 2) %>%
    tidyr::pivot_longer(stringr::str_subset(names(.data), "ID_GQ|PopNum|K", negate = T),
      names_to = "Qpop", values_to = "Qvalue"
    )

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
