#' @title Compute Evanno
#'
#' @description
#' Compute classical Evanno from structure summary file and return a graphic
#'
#' @details
#' more details to come ...
#'
#' @param df Dataframe with structure results
#'
#' @examples
#' # provide some examples of how to use your function
#' #summary.str <- data(structure)
#' strc_evanno(summary.str)
#'
#' @export
strc_evanno <- function(df){

  evanno.res <- df %>% dplyr::group_by(k) %>%
    dplyr::summarise(mean_ln_prob_data = base::mean(.data$ln_prob_data),
              sd_ln_prob_data = stats::sd(.data$ln_prob_data)) %>%
    dplyr::mutate(drv1K = NaN,
           drv1K_sd = NaN,
           abs_drv2K = NaN,
           abs_drv2K_sd = NaN)

  # first derive
  for(x in 2:(base::max(evanno.res$k))){
    evanno.res$drv1K[x]    <- evanno.res$mean_ln_prob_data[x] - evanno.res$mean_ln_prob_data[x-1]
    evanno.res$drv1K_sd[x] <- abs(evanno.res$sd_ln_prob_data[x] - evanno.res$sd_ln_prob_data[x-1])
  }

  # abs second derive
  for(x in 2:(max(evanno.res$k)-1)){
    evanno.res$abs_drv2K[x]    <- abs(evanno.res$drv1K[x+1] - evanno.res$drv1K[x])
    evanno.res$abs_drv2K_sd[x] <- abs(evanno.res$drv1K_sd[x+1] - evanno.res$drv1K_sd[x])
  }

  # Delta k
  evanno.res <-  dplyr::mutate(evanno.res,
                               delta_K = .data$abs_drv2K / .data$sd_ln_prob_data)


  ev_graph.1 <-  ggplot(evanno.res, aes(x=.data$k, y=.data$mean_ln_prob_data)) +
    geom_line()+
    geom_point()+
    geom_errorbar(aes(x=.data$k, ymax=.data$mean_ln_prob_data+.data$sd_ln_prob_data, ymin=.data$mean_ln_prob_data-.data$sd_ln_prob_data, width=0.1))+
    scale_x_continuous(breaks=1:10,labels=1:10)+
    labs(x=expression(paste(italic(K))),y=expression(paste("Mean L(",italic(K),") " %+-% " SD"))) +
    theme_bw()

  ev_graph.1

  ev_graph.2 <-  ggplot(data = evanno.res, aes(x=k, y=drv1K)) +
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    ggplot2::geom_errorbar(aes(x=.data$k, ymax=.data$drv1K+.data$drv1K_sd, ymin=.data$drv1K-.data$drv1K_sd, width=0.1))+
    ggplot2::scale_x_continuous(breaks=1:10,labels=1:10)+
    ggplot2::labs(x=expression(paste(italic(K))),y=expression(paste("L'(",italic(K),") " %+-% " SD"))) +
    ggplot2::theme_bw()

  ev_graph.2

  ev_graph.3 <-  ggplot(evanno.res, aes(x=.data$k, y=.data$abs_drv2K)) +
    geom_line()+
    geom_point()+
    geom_errorbar(aes(x=.data$k, ymax=.data$abs_drv2K+.data$abs_drv2K_sd, ymin=.data$abs_drv2K-.data$abs_drv2K_sd, width=0.1))+
    scale_x_continuous(breaks=1:10,labels=1:10)+
    labs(x=expression(paste(italic(K))),y=expression(paste("|L\"(",italic(K),")| " %+-% " SD"))) +
    theme_bw()

  ev_graph.3

  ev_graph.4 <- ggplot(data = evanno.res, aes(x=k, y=delta_K)) +
    geom_line()+
    geom_point()+
    scale_x_continuous(breaks=1:10,labels=1:10)+
    labs(x=expression(paste(italic(K))),y=expression(paste(Delta,italic(K)))) +
    theme_bw()

  ev_graph.4


  graph.Evanno <- ggpubr::ggarrange(ev_graph.1, ev_graph.2, ev_graph.3, ev_graph.4,
                            nrow = 2, ncol = 2, align = c("hv"), labels = c("A", "B", "C", "D")
  )

  graph.Evanno

  return(graph.Evanno)

}
