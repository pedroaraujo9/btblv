test_that("inputs", {
  purrr::map(example_fit, ~{
    .x %>%
      extract_posterior() %>%
      posterior_summary() %>%
      plot_latent_effects() %>%
      expect_no_error()
  })

  purrr::map(example_fit, ~{
    .x %>%
      extract_posterior() %>%
      posterior_summary() %>%
      plot_latent_effects(highlight = c("Ireland")) %>%
      expect_no_error()
  })
})
