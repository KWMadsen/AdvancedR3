#' Calculate descriptive statistics of each metabolite
#'
#' @param data Lipidomics dataset
#'
#' @return "A dara.frame/tibble
#' @export
#'

descriptive_stats <-   function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}


#' Plot for basic distribution of metabolite
#'
#' @param dataThe lipidomics dataset
#'
#' @return A plot object

plot_distributions <- function(data) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}
