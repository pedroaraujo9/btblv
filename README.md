## R package to fit Bayesian time-dependent beta latent variable models

- With the package `btblv`, you can fit the time-dependent beta latent variable models.
- `tblvArmaUtils` contains functions that depend on `RcppArmadillo`; you have to install it if you want, for instance, to calculate the BIC for the models.

#### Installing the packages

To install both packages, use the package `devtools`.
```r
devtools::install_github(repo = "pedroaraujo9/btblv", subdir = "btblv")
devtools::install_github(repo = "pedroaraujo9/btblv", subdir = "tblvArmaUtils")
```

#### Preparing the data

```r
library(tidyverse)
library(btblv)

# mortality data 
data("hmd_data")

mort_data = hmd_data$life_tables_5x5 %>%
  dplyr::filter(year %in% seq(1950, 2015, 5)) %>%
  dplyr::filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                                 "New Zealand Non-Maori", "England and Wales (Total Population)",
                                 "England and Wales (Civilian Population)",
                                 "Scotland", "Northern Ireland", "Wales")))

model_data = btblv::create_btblv_data(
  df = mort_data, 
  resp_col_name = "mx",
  item_col_name = "age",
  group_col_name = "country",
  time_col_name = "year"
)
```

#### Fitting the model

```r
fit = btblv::fit_btblv(
  btblv_data = model_data, 
  K = 2,
  precision = "single",
  iter = 1000,
  warmup = 500,
  thin = 2,
  chains = 3,
  cores = 3,
  seed = 1
)

# extracts the posterior
post = fit |> btblv::extract_posterior()
```

#### Checking convergence with the $\hat{R}$ and the effective sample size
```r
# get convergence metrics for the parameters
post |> btblv::check_convergence()
```
#### Posterior summaries and posterior predicted distribution 
```r
post_summ = post |> btblv::posterior_summary()
post_pred = post |> btblv::posterior_predict(seed = 1)
```

#### Model fit metrics 
```r
btblv::check_fit(post_pred, post_summ)
```

#### WAIC and BIC
```r

# BIC with an approximation for the marginal likelihood
tblvArmaUtils::compute_BIC(post, N = 100000, cores = 4, seed = 1)

# WAIC with the package loo
btblv::compute_WAIC(post)
```


