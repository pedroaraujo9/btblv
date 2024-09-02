#' Plot Kendall's trend coefficient
#'
#' @param btblv_data `btblv_data` object create with `btblv::create_btblv_data`

#' @return `ggplot2` plot with the correlation matrix.
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#'
#' @examples
#' example_fit$single_K2$btblv_data |> plot_trend()
#'
plot_trend = function(btblv_data) {

  assertthat::assert_that(
    is(btblv_data, "btblv_data"),
    msg = "`btblv_data` should be a `btblv_data` obejct."
  )

  tau_over_time = btblv_data$data %>%
    dplyr::select(group, item, time, y) %>%
    dplyr::arrange(group, time, item) %>%
    dplyr::group_by(group, item) %>%
    dplyr::summarise(tau = .get_tau(y))

  pl = tau_over_time %>%
    ggplot2::ggplot(ggplot2::aes(x=factor(item), y=group, fill=tau)) +
    ggplot2::geom_tile(color="white") +
    viridis::scale_fill_viridis() +
    ggplot2::labs(fill=expression(tau))

  return(pl)
}
