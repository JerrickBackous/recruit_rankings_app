#' Create rankings table
#'
#' @param input_df dataframe of rankings
#' @param input_position position to filter
#' @param input_player players to filter
#' @param input_rankers rankers to filter
#'
#' @return dataframe of rankings
#' @importFrom dplyr filter select
#' @importFrom tidyselect all_of
#' @export
rankings_table <- function(input_df, input_position, input_player, input_rankers) {
    if (is.null(input_rankers)) {
      if (is.null(input_player)) {
        df <- input_df |>
          dplyr::filter(.data$Pos %in% input_position) |>
          dplyr::select(c(.data$Player:.data$`AVG Grade`))
      } else {
        df <- input_df |>
          dplyr::filter(.data$Player %in% input_player) |>
          dplyr::select(c(.data$Player:.data$`AVG Grade`))
      }
    } else {
      if (is.null(input_player)) {
        df <- input_df |>
          dplyr::filter(.data$Pos %in% input_position) |>
          dplyr::select(c(.data$Player:.data$`AVG Grade`), all_of(input_rankers))
      } else {
        df <- input_df |>
          dplyr::filter(.data$Player %in% input_player) |>
          dplyr::select(c(.data$Player:.data$`AVG Grade`), all_of(input_rankers))
      }
    }

  return(df)
}
