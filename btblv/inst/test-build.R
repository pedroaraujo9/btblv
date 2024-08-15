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
devtools::test(filter = "posterior_predict")
devtools::test(filter = "check_fit")
devtools::test()
devtools::install()

devtools::install()
devtools::test()
devtools::clean_dll()
pkgbuild::compile_dll()


devtools::install_github(repo = "pedroaraujo9/btblv", subdir = "btblv")
devtools::install_github(repo = "pedroaraujo9/btblv", subdir = "tblvArmaUtils")




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







