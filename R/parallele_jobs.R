#' @title Create a job file for faststructure package
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Create a job file for faststructure package
#'
#' @details
#' more details to come ...
#'
#' @param nk Numeric vector of the K values to be tested
#' @param nrep Number of repetition at each K values
#' @param popnum Numeric vector of the pop that you want to use
#' @param burn Number of burnin
#' @param iteration Number of iteration
#'  
#' @examples
#' # create a new job
#' job1 <- strc_create_jobs(nk = 1:2, nrep = 3, popnum = 1:2, burn = 1000, iteration = 10000)
#' job1
#' # write(t(jobs1), ncol=length(jobs1[1,]), file='path/to/file.txt')
#' @export

strc_create_jobs <- function(nk = 1:4, nrep = 3, popnum = 1, burn = 1000, iteration = 10000){
  
  # Prevent scientific notation
  burn <- base::format(burn, scientific=F)
  iteration <-  base::format(iteration, scientific=F)
  
  base::cat("\nCreating", base::length(nk) * nrep, "jobs:",
      "\nK =", min(nk), "-", base::max(nk),
      "\nN rep =", nrep,
      "\nBurning =", burn,
      "\nMCMC =", iteration,
      "\n\n"
  )
  
  # Job list
  jobs_df <- base::data.frame(jobID = character(),
                        pop =  character(),
                        k = numeric(),
                        burnin = character(),
                        MCMC = character(),
                        stringsAsFactors = F)
  
  
  for(x in 1:nrep){
    jobs_df.int <- base::data.frame(jobID = base::paste(base::paste0("k",nk), x, sep="_r"),
                              pop = base::paste(popnum, collapse = ","),
                              k = nk,
                              burnin = burn,
                              MCMC = iteration) # doit être en texte
    
    jobs_df <- base::rbind(jobs_df, jobs_df.int)
  }
  
  return(jobs_df)
}
