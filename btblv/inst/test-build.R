library(rstan)
library(devtools)
library(magrittr)
library(tidyverse)
devtools::install_github(repo = "pedroaraujo9/tblv", subdir = "packages/btblv")

try(roxygen2::roxygenize(load_code = rstantools_load_code), silent = TRUE)

roxygen2::roxygenize()
devtools::check()
devtools::build()
devtools::document()
devtools::load_all()
devtools::test(filter = "plot_latent_effects")
devtools::test(filter = "plot_trend")
devtools::test()
devtools::install()

devtools::install()
devtools::test()
devtools::clean_dll()
pkgbuild::compile_dll()


devtools::install_github(repo = "pedroaraujo9/btblv", subdir = "btblv")
devtools::install_github(repo = "pedroaraujo9/btblv", subdir = "tblvArmaUtils")

devtools::load_all()
example_fit$single_K1$btblv_data$df %>%
  create_btblv_data("mx", "age", "country", "year") %>%
  plot_corrmatrix(transform = NULL)

example_fit$single_K1$btblv_data$df %>%
  create_btblv_data("mx", "age", "country", "year") %>%
  plot_trend(transform = NULL)

usethis::use_test("plot_trend")


plot_latent_effects = function(posterior_summary, highlight = NULL) {

  theta_df = posterior_summary$posterior_summary_df$theta
  group_col_name = names(theta_df)[7]
  time_col_name = names(theta_df)[8]

  names(theta_df)[7:8] = c("group", "time")

  theta_wide = theta_df %>%
    dplyr::select(mean, group, time, K) %>%
    tidyr::spread(K, mean)

  if(!is.null(highlight)) {
    pl = theta_df %>%
      mutate(sel_group = ifelse(group %in% highlight, group, "Others")) %>%
      mutate(sel_alpha = ifelse(sel_group == "Others", "Others", "Sel")) %>%
      mutate(K = paste0("Dimension ", K)) %>%
      ggplot(aes(x=time, y=mean, group=group, color=sel_group, alpha = sel_alpha)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_alpha_manual(values = c(0.16, 1), guide = "none") +
      facet_wrap(K ~ ., scales = "free_y") +
      labs(x=time_col_name,
           y=latex2exp::TeX("$\\theta_{ik}^{(t)}$"),
           color=paste0(group_col_name, ":")) +
      theme(legend.position = "top", text = element_text(size = 9))
  }else{
    pl = theta_df %>%
      mutate(K = paste0("Dimension ", K)) %>%
      ggplot(aes(x=time, y=mean, group=group)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(K ~ ., scales = "free_y") +
      labs(x=time_col_name,
           y=latex2exp::TeX("$\\theta_{ik}^{(t)}$"),
           color=paste0(group_col_name, ":")) +
      theme(legend.position = "top", text = element_text(size = 9))
  }

  return(pl)
}

summ %>% plot_latent_effects(highlight_group = c("Russia", "Ukraine", "Belarus"))
summ %>% plot_latent_effects()

usethis::use_test("plot_latent_effects")


summ = example_fit$specific_K1 %>%
  extract_posterior() %>%
  posterior_summary()


usethis::use_test("imifa_to_blv")

library(IMIFA)
imifa_fit = readRDS("../../analysis/models/bfa-K=1-10.rds")
imifa_result = IMIFA::get_IMIFA_results(imifa_fit, Q = 1)

lf = readRDS("../../analysis/data/data_model.rds")
lf %>% glimpse()

btblv_data = btblv::create_btblv_data(
  lf, "mx", "age", "country", "year"
)



bfa = imifa_to_blv(btblv_data, imifa_result)
bfa$post_sample_array$alpha[,,1] %>% colMeans() %>% plot()
bfa$post_sample_chains$beta[,,1] %>% colMeans() %>% plot()


imifa_fit = readRDS("../../analysis/models/bfa-K=1-10.rds")

imifa_result = list()

btblv_data = btblv::create_btblv_data(
  lf, "mx", "age", "country", "year"
)









data("example_fit")

imifa_result = list(
  fits = list(
    imifa_result_K1 = IMIFA::get_IMIFA_results(imifa_fit, Q = 1),
    imifa_result_K2 = IMIFA::get_IMIFA_results(imifa_fit, Q = 2)
  ),
  btblv_data = example_fit$single_K1$btblv_data
)

saveRDS(imifa_result, "inst/imifa_result.rds")



imifa_result = IMIFA::get_IMIFA_results(imifa_fit, Q = 2)
saveRDS(imifa_result, "inst/bfa_K=1.rds")


post = example_fit$single_K1 %>%
  extract_posterior(alpha_reference = "mode", apply_varimax = TRUE) %>%
  posterior_summary()

.x = example_fit$single_K1

post_pred = .x %>%
  extract_posterior() %>%
  posterior_predict(seed = 1)

post_pred$pred_post_sample[1:10, 1:10]

post_summ = .x %>%
  extract_posterior()

post_summ$post_sample_array$

check_fit(post_pred, post_summ)

imifa_result = readRDS("tests/imifa_result.rds")

devtools::load_all()

test = imifa_to_blv(imifa_result$btblv_data, imifa_result$fits$imifa_result_K2)
test$post_sample_array$kappa %>% dim()

test$precision

class(test)
summ = test %>% posterior_summary()
pred = test %>% posterior_predict(seed = 1)

check_fit(pred, summ)

tt$pred_post_sample[1:10, 1:10]

test$post_sample_array$kappa[, 1] %>% sqrt() %>% hist()

btblv_posterior = test

iters = btblv_posterior$post_sample_array$beta %>% dim() %>% .[1]
N = btblv_posterior$btblv_data$data_list_stan$N


iters
N

pred_post_sample = matrix(nrow = iters, ncol = N)

theta = btblv_posterior$post_sample_array$theta
alpha = btblv_posterior$post_sample_array$alpha
beta = btblv_posterior$post_sample_array$beta
kappa = btblv_posterior$post_sample_array$kappa
precision = btblv_posterior$precision
btblv_posterior$post_sample_array$
theta %>% dim()

item = btblv_posterior$btblv_data$data$item_num
ind = btblv_posterior$btblv_data$data$ind_num







