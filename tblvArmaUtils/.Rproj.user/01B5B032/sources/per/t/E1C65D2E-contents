library(rstan)
library(devtools)
library(magrittr)

try(roxygen2::roxygenize(load_code = rstantools_load_code), silent = TRUE)

Rcpp::compileAttributes()


roxygen2::roxygenize()
devtools::check()
devtools::build()
devtools::document()
devtools::load_all()
devtools::test()
devtools::install()
usethis::use_test("compute_BIC")
devtools::test(filter = "compute_BIC")


fit = readRDS("../../analysis/models/btblv-precision=single-K=2.rds")
post_sample = fit %>% extract_posterior()
post_summ = post_sample %>% posterior_summary()
post_pred = post_sample %>% posterior_predict(seed = 1)

compute_BIC(post_sample, N = 10000, seed = 1, cores = 4)
app = approx_mloglike(post_sample, N = 1000, cores = 1)
app$
sum(app$mloglike)


btblv::check_fit(post_pred, post_summ)$global_metrics

calc_cWAIC(post_sample)







?rcpp_mc_log_mlike

tblvArmaUtils::rcpparma_hello_world()

tblvArmaUtils::LSE(c(10, 20, 30))

?mc_log_mlike

post_sample = btblv::example_fit$specific_K1 %>% extract_posterior()
post_sample$btblv_data$data_list_stan$Ng

btblv::example_fit$single_K1 %>%
  extract_posterior() %>%
approx_mlog_like(N = 1000)

btblv_fit = base::readRDS("../../analysis/models/btblv-precision=single-K=4.rds")

btblv_fit = btblv::example_fit$single_K1 

post_sample = btblv_fit |> btblv::extract_posterior()
post_summ = post_sample |> btblv::posterior_summary()


data_wide = post_sample$btblv_data$data |>
  dplyr::select(ind_num, group_num, item, y) |>
  dplyr::arrange(ind_num) |>
  tidyr::spread(item, y)

data_matrix = post_sample$btblv_data$data_list_stan$x

K = post_sample$btblv_data$data_list_stan$K

alpha = post_summ$posterior_mean$alpha
beta = post_summ$posterior_mean$beta
kappa = post_summ$posterior_mean$kappa

if(post_sample$precision == "single") {
  kappa = kappa * (beta/beta)
}

Ng = post_sample$btblv_data$data_list_stan$Ng

g = 1
cores = 3
N = 1000

if(cores == 1) {
  approx_mlog_like = foreach::foreach(g=1:Ng, .combine = "c") %do% {
    cat(g, "\r")
    
    inds = data_wide |>
      dplyr::filter(group_num == g) |> 
      dplyr::select(ind_num) |> 
      base::unlist() 
    
    data_group = data_matrix[inds, ]
    
    phi = post_summ$posterior_mean$phi[g, 1]
    sigma = post_summ$posterior_mean$sigma[g, 1]
    
    E_post_sample_list = lapply(inds, function(i){
      cbind(post_sample$post_sample_array$rot_E[, i, ])
    })
    
    log_mlike_sample = tblvArmaUtils::rcpp_mc_log_mlike(
      N=N, 
      x = data_group, 
      alpha = alpha, 
      beta = beta, 
      kappa = kappa, 
      phi = phi,
      sigma = sigma, 
      E_post_sample_list = E_post_sample_list
    )
    
    tblvArmaUtils::LSE(log_mlike_sample) - log(N)
  }
}else{
  
  pkgs = c("dplyr", "Rcpp", "tblvArmaUtils")
  funcs = c("mc_log_mlike")
  
  cl = parallel::makeCluster(cores)  
  doParallel::registerDoParallel(cl)  
  
  approx_mlog_like = foreach::foreach(g=1:Ng, .packages = pkgs, .export = funcs, .combine = "c") %do% {
    
    inds = data_wide |>
      dplyr::filter(group_num == g) |> 
      dplyr::select(ind_num) |> 
      base::unlist() 
    
    data_group = data_matrix[inds, ]
    
    phi = post_summ$posterior_mean$phi[g, 1]
    sigma = post_summ$posterior_mean$sigma[g, 1]
    
    E_post_sample_list = lapply(inds, function(i){
      cbind(post_sample$post_sample_array$rot_E[, i, ])
    })
    
    log_mlike_sample = tblvArmaUtils::rcpp_mc_log_mlike(
      N=N, 
      x = data_group, 
      alpha = alpha, 
      beta = beta, 
      kappa = kappa, 
      phi = phi,
      sigma = sigma, 
      E_post_sample_list = E_post_sample_list
    )
    
    tblvArmaUtils::LSE(log_mlike_sample) - log(N)
  }
  
  parallel::stopCluster(cl)
  
}

approx_mlog_like %>% sum()

cl = parallel::makeCluster(cores)  
registerDoParallel(cl)  

pkgs = c("tidyverse", "Rcpp")
funcs = c("approx_logml")


Rcpp::sourceCpp("rscripts/smc_marginal_ll.cpp", )




approx_full_logml = function(N, fit, data_df, cores) {
  sm = fit %>% transform_params()
  post = sm %>% get_posterior_mean()
  
  data_wide = data_df %>%
    select(ind, group, item, mx) %>%
    arrange(ind) %>%
    spread(item, mx)
  
  data_matrix = data_wide %>% select(-ind, -group) %>% as.matrix()
  
  K = sm$theta_tr %>% dim() %>% .[3]
  
  alpha = post$alpha_post
  beta = post$beta_post
  
  if(is.null(dim(post$kappa_post))) {
    kappa = post$kappa_post*post$beta_post/post$beta_post
  }else{
    kappa = post$kappa_post
  }
  
  Ng = data_wide$group %>% unique() %>% length()
  
  cl = makeCluster(cores)  
  registerDoParallel(cl)  
  
  pkgs = c("tidyverse", "Rcpp")
  funcs = c("approx_logml")
  
  mc_logml = foreach(g=1:Ng, .packages = pkgs, .export = funcs, .combine = "c") %do% {
    cat(g, "\r")
    ind = data_wide %>% filter(group == g) %>% .$ind
    data_ind = data_matrix[ind, ]
    
    phi = post$phi_post[g, 1]
    sigma = post$sigma_post[g, 1]
    
    E_post_sample_list = lapply(ind, function(x){
      cbind(sm$E_tr[, x, ])
    })
    
    mc_logml = approx_logml(
      N=N, 
      x = data_ind, 
      alpha = alpha, 
      beta = beta, 
      kappa = kappa, 
      phi = phi,
      sigma = sigma, 
      E_post_sample_list = E_post_sample_list
    )
    
    LSE(mc_logml) - log(N)
  }
  
  stopCluster(cl)
  
  return(mc_logml)
}














