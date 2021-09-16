#' @title Compute variance explained by first PCA axis
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Compute variance explained by a few PCA axis.
#'
#' @details
#' PCA from glPca from the package Adegenet
#'
#' @param x glPCA object
#' @param naxe number of axis for which you want to know explained variance
#' 
#' @examples
#' ## simulate a toy dataset
#' x <- adegenet::glSim(50,4e3, 50, ploidy=2)
#' ## perform PCA
#' pca1 <- adegenet::glPca(x, nf=3)
#' ## Extract variance
#' res <- pca_var(pca1, naxe=3)
#' res
#' @export

pca_var <- function(x, naxe=10){
  res <- data.frame(axis = c(1:naxe),
                eig = x$eig[c(1:naxe)]) %>% 
    dplyr::mutate(p.eig = eig / sum(x$eig))
  return(res)
}

#' @title Plot variance explained by PCA axis
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Compute variance explained by a few PCA axis
#'
#' @details
#' PCA from glPca
#'
#' @param x glPCA object
#' @param naxe number of axis for which you want to know explained variance
#' 
#' @examples
#' ## simulate a toy dataset
#' x <- adegenet::glSim(50,4e3, 50, ploidy=2)
#' ## perform PCA
#' pca1 <- adegenet::glPca(x, nf=3)
#' ## Extract variance
#' pca_varplot(pca1, naxe=3)
#' @export
pca_varplot <- function(x, naxe=10){
  graph <- pca_var(x, naxe) %>% 
    ggplot(aes(x = factor(axis), y = p.eig)) +
    geom_bar(stat = "identity") +
    labs(x = "Axis", y = "Percentage of variance",
         title = "Perc. of variance explained by first axes") +
    theme_bw()
  
  return(graph)
}

#' @title Extract PCA score table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Compute variance explained by a few PCA axis
#'
#' @details
#' PCA from glPca. It will return a dataframe, including ID and pca.score for the desire number of axis
#'
#' @param x glPCA object
#' @param naxe number of axis for which you want to extract PCA scores
#' 
#' @examples
#' ## simulate a toy dataset
#' x <- adegenet::glSim(50,4e3, 50, ploidy=2)
#' ## perform PCA
#' pca1 <- adegenet::glPca(x, nf=3)
#' ## Extract variance
#' res <- pca_scoretable(pca1, naxe=3)
#' head(res)
#' @export

pca_scoretable <- function(x, naxe = 1){
  res <-  data.frame(ID = row.names(x$scores),
                     score = x$scores[,1:naxe]) 
  return(res)
} 

