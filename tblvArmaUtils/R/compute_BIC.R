#' Compute the BIC based on the marginal likelihood with respect to latent variables
#' for the btblv model.
#'
#' @param btblv_posterior `btblv_posterior` object generated 
#' by `btblv::extract_posterior`. 
#' @param approx_mloglike `approx_mloglike` object generated by `tblvArmaUtils::approx_mlog_like`. Default is `NULL`.
#' @param N integer value with the Monte Carlo sample size. Default is `NULL`.
#' @param seed integer value with seed for the random generating system.
#' @param cores number of cores for the computation.
#'
#' @return numeric value with the BIC.
#' @export
#'
#' @examples
#' # 
compute_BIC = function(btblv_posterior, 
                       approx_mloglike = NULL, 
                       N = NULL, 
                       seed, 
                       cores = 1) {
  
  if(is.null(approx_mloglike)) {
    mloglike = btblv_posterior %>% 
      approx_mloglike(N = N, cores = cores, seed = seed) %>%
      .$mloglike
    
  }else{
    mloglike = approx_mloglike %>%
      .$mloglike
  }
  
  data_list_stan = btblv_posterior$btblv_data$data_list_stan
  
  n_obs = data_list_stan$N
  n = data_list_stan$n
  J = data_list_stan$J
  K = data_list_stan$K
  Ng = data_list_stan$Ng
  
  num_param = n*K + J*K + J + 2*Ng 
  
  if(btblv_posterior$precision == "single") {
    num_param = num_param + 1
    
  }else if(btblv_posterior$precision == "specific") {
    num_param = num_param + J
  }
  
  bic = -2*sum(mloglike) + num_param*log(n_obs)
  return(bic)
  
}