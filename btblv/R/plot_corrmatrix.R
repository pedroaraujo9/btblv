#' Plot correlation matrix
#'
#' @param btblv_data `btblv_data` object create with `btblv::create_btblv_data`
#' @param transform function to transform the data. Default is `btblv::logit`.
#' If you do not want transformations use `NULL`.
#'
#' @return `ggplot2` plot with the correlation matrix.
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis

#' @examples
#' example_fit$single_K2$btblv_data |> plot_corrmatrix()
#'
plot_corrmatrix = function(btblv_data, transform = btblv::logit) {

  assertthat::assert_that(
    is(btblv_data, "btblv_data"),
    msg = "`btblv_data` should be a `btblv_data` obejct."
  )

  levels = btblv_data$data$item %>% unique()

  if(is.null(transform)) {
    corrplot = btblv_data$data %>%
      dplyr::select(group, time, item, y) %>%
      tidyr::spread(item, y) %>%
      dplyr::select(-group, -time) %>%
      base::as.matrix() %>%
      transform() %>%
      stats::cor() %>%
      base::as.data.frame() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(item1 = colnames(.)) %>%
      tidyr::gather(item2, value, -item1) %>%
      dplyr::mutate(value = as.numeric(value),
                    item1 = factor(item1, levels = btblv_data$data$item %>% unique()),
                    item2 = factor(item2, levels = btblv_data$data$item %>% unique())) %>%
      ggplot2::ggplot(ggplot2::aes(x=item1, y=item2, fill=value)) +
      ggplot2::geom_tile(color="white") +
      viridis::scale_fill_viridis() +
      ggplot2::guides(color = "none") +
      ggplot2::scale_color_manual(values = c("black", "white")) +
      ggplot2::labs(x="", y="", fill="Corr")
  }else{

    assertthat::assert_that(
      is(transform, "function"),
      msg = "`transform` should be a function object."
    )

    corrplot = btblv_data$data %>%
      dplyr::select(group, time, item, y) %>%
      tidyr::spread(item, y) %>%
      dplyr::select(-group, -time) %>%
      base::as.matrix() %>%
      btblv::logit() %>%
      stats::cor() %>%
      base::as.data.frame() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(item1 = colnames(.)) %>%
      tidyr::gather(item2, value, -item1) %>%
      dplyr::mutate(value = as.numeric(value),
                    item1 = factor(item1, levels = btblv_data$data$item %>% unique()),
                    item2 = factor(item2, levels = btblv_data$data$item %>% unique())) %>%
      ggplot2::ggplot(ggplot2::aes(x=item1, y=item2, fill=value)) +
      ggplot2::geom_tile(color="white") +
      viridis::scale_fill_viridis() +
      ggplot2::guides(color = "none") +
      ggplot2::scale_color_manual(values = c("black", "white")) +
      ggplot2::labs(x="", y="", fill="Corr")
  }



  return(corrplot)
}
