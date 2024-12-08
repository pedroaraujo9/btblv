#' Compute posterior summary for the `btblv_posterior` object
#'
#' @param btblv_posterior object of the class `btblv_posterior`
#' generated by `btblv::extract_posterior`.
#' @param cred_mass numeric value between 0 and 1.
#' @param pars string vector with the parameters to summarize. Default is `NULL`,
#' and it summarizes all parameters.
#'
#' @return object of the class `btblv_posterior_summary` with the attributes:
#'
#' @export
#'
#' @examples
#' #
posterior_summary = function(btblv_posterior, cred_mass = 0.95, pars = NULL) {

  # param names
  if(is.null(pars)) {
    params = names(btblv_posterior$post_sample_array)
    params = params[!(params %in% c("theta", "alpha", "E", "lp__"))]
  }

  # computes posterior mean
  posterior_mean = list()

  for(param in params) {
    posterior_mean[[param]] = .compute_posterior_mean(
      btblv_posterior$post_sample_array[[param]]
    )
  }

  names(posterior_mean) = names(posterior_mean) %>% stringr::str_remove("rot\\_")

  # original columns names
  item_col_name = btblv_posterior$btblv_data$columns[1]
  group_col_name = btblv_posterior$btblv_data$columns[2]
  time_col_name = btblv_posterior$btblv_data$columns[3]

  # maps names for the inds
  ind_id = btblv_posterior$btblv_data$data %>%
    dplyr::select(ind_num, group, time) %>%
    dplyr::distinct()

  colnames(ind_id)[-1] = c(group_col_name, time_col_name)

  # maps names for the item
  item_id = btblv_posterior$btblv_data$data %>%
    dplyr::select(item_num, item) %>%
    dplyr::distinct()

  colnames(item_id)[-1] = item_col_name

  # maps names for the groups
  group_size = btblv_posterior$btblv_data$data %>%
    dplyr::select(ind_num, group_num) %>%
    dplyr::distinct() %>%
    dplyr::group_by(group_num) %>%
    dplyr::summarise(N = n()) %>%
    dplyr::ungroup()

  group_id = btblv_posterior$btblv_data$data %>%
    dplyr::select(group_num, group) %>%
    dplyr::distinct() %>%
    dplyr::left_join(group_size, by = c("group_num"))

  colnames(group_id)[2] = group_col_name

  # compute posterior summaries
  posterior_summary_df = list()

  for(param in params) {
    posterior_summary_df[[param]] = .get_summary_df(
      btblv_posterior$post_sample_array[[param]],
      cred_mass = cred_mass
    )
  }

  # adding user label for the parameters
  posterior_summary_df$beta = posterior_summary_df$beta %>%
    dplyr::right_join(item_id, by = c("id" = "item_num")) %>%
    dplyr::select(-id)

  if(!is.null(posterior_summary_df$kappa)) {

    if(nrow(posterior_summary_df$kappa) == 1) {

      posterior_summary_df$kappa = posterior_summary_df$kappa

    }else{

      posterior_summary_df$kappa = posterior_summary_df$kappa %>%
        dplyr::right_join(item_id, by = c("id" = "item_num")) %>%
        dplyr::select(-id)
    }
  }

  if(!is.null(posterior_summary_df$log_kappa)) {

    if(nrow(posterior_summary_df$log_kappa) == 1) {

      posterior_summary_df$log_kappa = posterior_summary_df$log_kappa

    }else{

      posterior_summary_df$log_kappa = posterior_summary_df$log_kappa %>%
        dplyr::right_join(item_id, by = c("id" = "item_num")) %>%
        dplyr::select(-id)
    }
  }

  if(!is.null(posterior_summary_df$rot_theta)) {
    posterior_summary_df[["theta"]] = posterior_summary_df$rot_theta %>%
      dplyr::right_join(ind_id, by = c("id" = "ind_num")) %>%
      dplyr::select(-id)

    posterior_summary_df[["rot_theta"]] = NULL
  }

  if(!is.null(posterior_summary_df$rot_E)) {
    posterior_summary_df[["E"]] = posterior_summary_df$rot_E %>%
      dplyr::right_join(ind_id, by = c("id" = "ind_num")) %>%
      dplyr::select(-id)

    posterior_summary_df[["rot_E"]] = NULL

  }

  if(!is.null(posterior_summary_df$rot_alpha)) {

    posterior_summary_df[["alpha"]] = posterior_summary_df$rot_alpha %>%
      dplyr::right_join(item_id, by = c("id" = "item_num")) %>%
      dplyr::select(-id)

    posterior_summary_df[["rot_alpha"]] = NULL

  }

  if(!is.null(posterior_summary_df$phi)) {
    posterior_summary_df$phi = posterior_summary_df$phi %>%
      dplyr::right_join(group_id, by = c("id" = "group_num")) %>%
      dplyr::select(-id)
  }

  if(!is.null(posterior_summary_df$sigma)) {
    posterior_summary_df$sigma = posterior_summary_df$sigma %>%
      dplyr::right_join(group_id, by = c("id" = "group_num")) %>%
      dplyr::select(-id)
  }

  btblv_posterior_summary = list(
    posterior_mean = posterior_mean,
    posterior_summary_df = posterior_summary_df
  )

  class(btblv_posterior_summary) = "btblv_posterior_summary"

  return(btblv_posterior_summary)
}
