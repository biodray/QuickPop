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
#' summary.str <- data(structure)
#' str_evanno(summary.str)
#'
#' @export
str_evanno <- function(df){

  evanno.res <- df %>% dplyr::group_by(k) %>%
    dplyr::summarise(mean_ln_prob_data = mean(ln_prob_data),
              sd_ln_prob_data = sd(ln_prob_data)) %>%
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
  evanno.res <- evanno.res %>% dplyr::mutate(delta_K = abs_drv2K / sd_ln_prob_data)


  ev_graph.1 <- evanno.res %>% ggplot2::ggplot(aes(x=k, y=mean_ln_prob_data)) +
    geom_line()+
    geom_point()+
    geom_errorbar(aes(x=k, ymax=mean_ln_prob_data+sd_ln_prob_data, ymin=mean_ln_prob_data-sd_ln_prob_data, width=0.1))+
    scale_x_continuous(breaks=1:10,labels=1:10)+
    labs(x=expression(paste(italic(K))),y=expression(paste("Mean L(",italic(K),") " %+-% " SD"))) +
    theme_bw()

  ev_graph.1

  ev_graph.2 <- evanno.res %>% ggplot2::ggplot(aes(x=k, y=drv1K)) +
    geom_line()+
    geom_point()+
    geom_errorbar(aes(x=k, ymax=drv1K+drv1K_sd, ymin=drv1K-drv1K_sd, width=0.1))+
    scale_x_continuous(breaks=1:10,labels=1:10)+
    labs(x=expression(paste(italic(K))),y=expression(paste("L'(",italic(K),") " %+-% " SD"))) +
    theme_bw()

  ev_graph.2

  ev_graph.3 <- evanno.res %>% ggplot2::ggplot(aes(x=k, y=abs_drv2K)) +
    geom_line()+
    geom_point()+
    geom_errorbar(aes(x=k, ymax=abs_drv2K+abs_drv2K_sd, ymin=abs_drv2K-abs_drv2K_sd, width=0.1))+
    scale_x_continuous(breaks=1:10,labels=1:10)+
    labs(x=expression(paste(italic(K))),y=expression(paste("|L\"(",italic(K),")| " %+-% " SD"))) +
    theme_bw()

  ev_graph.3

  ev_graph.4 <- evanno.res %>% ggplot2::ggplot(aes(x=k, y=delta_K)) +
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
