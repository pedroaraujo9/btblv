test_that("input", {
  example_fit$single_K1$btblv_data$df %>%
    create_btblv_data("mx", "age", "country", "year") %>%
    plot_corrmatrix() %>%
    expect_no_error()

  example_fit$single_K1$btblv_data$df %>%
    create_btblv_data("qx", "age", "country", "year") %>%
    plot_corrmatrix() %>%
    expect_no_error()

  example_fit$single_K1 %>%
    create_btblv_data("qx", "age", "country", "year") %>%
    plot_corrmatrix() %>%
    expect_error()
})
