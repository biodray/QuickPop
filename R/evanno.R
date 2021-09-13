#' @title Compute Evanno
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Compute classical Evanno from structure summary file and return a graphic
#'
#' @details
#' more details to come ...
#'
#' @param df Dataframe with structure results
#' @param plot If it return a plot, with full view (4 panels) or all within the same figure
#' @param plot.max in the graph, return the max value
#' 
#' @examples
#' # How to data should look
#' head(strcSummary.data)
#' 
#' # To perform Evanno without plot
#' res <- strc_evanno(strcSummary.data, plot = "none")
#' names(res)
#' 
#' # Different plot options
#' res <- strc_evanno(strcSummary.data, plot = "full", plot.max = FALSE)
#' res <- strc_evanno(strcSummary.data, plot = "resume", plot.max = TRUE)
#'
#' @export
strc_evanno <- function(df, plot = c ("none", "full", "resume"), plot.max = TRUE){

  evanno.res <- df %>% dplyr::group_by(.data$k) %>%
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
    geom_line(na.rm=TRUE)+
    geom_point(na.rm=TRUE)+
    geom_errorbar(aes(x=.data$k, ymax=.data$mean_ln_prob_data+.data$sd_ln_prob_data, ymin=.data$mean_ln_prob_data-.data$sd_ln_prob_data, width=0.1))+
    # Max
    geom_point(data = dplyr::filter(evanno.res, mean_ln_prob_data == max(.data$mean_ln_prob_data)), col = "red" )+
    geom_errorbar(data = dplyr::filter(evanno.res, mean_ln_prob_data == max(.data$mean_ln_prob_data)), col = "red" ,
                  aes(x=.data$k, ymax=.data$mean_ln_prob_data+.data$sd_ln_prob_data, ymin=.data$mean_ln_prob_data-.data$sd_ln_prob_data, width=0.1))+
    
    scale_x_continuous(n.breaks = length(unique(evanno.res$k))) +
    labs(x=expression(paste(italic(K))),y=expression(paste("Mean L(",italic(K),") " %+-% " SD"))) +
    theme_bw()

  ev_graph.2 <-  ggplot(data = evanno.res, aes(x=.data$k, y=.data$drv1K)) +
    ggplot2::geom_line(na.rm=TRUE)+
    ggplot2::geom_point(na.rm=TRUE)+
    ggplot2::geom_errorbar(aes(x=.data$k, ymax=.data$drv1K+.data$drv1K_sd, ymin=.data$drv1K-.data$drv1K_sd, width=0.1))+
    scale_x_continuous(n.breaks = length(unique(evanno.res$k))) +
    ggplot2::labs(x=expression(paste(italic(K))),y=expression(paste("L'(",italic(K),") " %+-% " SD"))) +
    ggplot2::theme_bw()

  ev_graph.3 <-  ggplot(evanno.res, aes(x=.data$k, y=.data$abs_drv2K)) +
    geom_line(na.rm=TRUE)+
    geom_point(na.rm=TRUE)+
    geom_errorbar(aes(x=.data$k, ymax=.data$abs_drv2K+.data$abs_drv2K_sd, ymin=.data$abs_drv2K-.data$abs_drv2K_sd, width=0.1))+
    scale_x_continuous(n.breaks = length(unique(evanno.res$k))) +
    labs(x=expression(paste(italic(K))),y=expression(paste("|L\"(",italic(K),")| " %+-% " SD"))) +
    theme_bw()

  ev_graph.4 <- ggplot(data = evanno.res, aes(x=.data$k, y=.data$delta_K)) +
    geom_line(na.rm=TRUE)+
    geom_point(na.rm=TRUE)+
    scale_x_continuous(n.breaks = length(unique(evanno.res$k))) +
    labs(x=expression(paste(italic(K))),y=expression(paste(Delta,italic(K)))) +
    theme_bw()

  if(plot.max == T){
    
    ev_graph.1 <-  ev_graph.1  +      # Max
      geom_point(data = dplyr::filter(evanno.res, mean_ln_prob_data == max(.data$mean_ln_prob_data)), col = "red" )+
      geom_errorbar(data = dplyr::filter(evanno.res, mean_ln_prob_data == max(.data$mean_ln_prob_data)), col = "red" ,
                    aes(x=.data$k, ymax=.data$mean_ln_prob_data+.data$sd_ln_prob_data, ymin=.data$mean_ln_prob_data-.data$sd_ln_prob_data, width=0.1))
    
    ev_graph.4 <- ev_graph.4 +
      # max
      geom_point(data = dplyr::filter(evanno.res, delta_K == max(.data$delta_K, na.rm = T)), col = "red", na.rm = T )

  }
  
  

  graph.full <- ggpubr::ggarrange(ev_graph.1, ev_graph.2, ev_graph.3, ev_graph.4,
                            nrow = 2, ncol = 2, align = c("hv"), labels = c("A", "B", "C", "D")
  )


  
  
  graph.resume <- ggpubr::ggarrange(ev_graph.1, ev_graph.4,
                                  nrow = 1, ncol = 2, align = c("hv")
                                  )
  

  if(plot == "full"){
    print(graph.full)
  } 
  
  if(plot == "resume"){
    print(graph.resume)
  }
  
  return(list(evanno = evanno.res, plot.full = graph.full, plot.resume = graph.resume))

}
