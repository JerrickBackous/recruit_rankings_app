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
          dplyr::select(c(.data$Player:.data$`AVG Grade`), all_of(input_rankers)) |>
          dplyr::mutate(`AVG Grade` = round(rowMeans(select(filter(input_df, .data$Pos %in% input_position), all_of(input_rankers)), na.rm = TRUE), 3),
                        `ADJ Grade` =  dplyr::case_when(.data$Pos == "QB" ~ round(rowMeans(select(filter(input_df, .data$Pos %in% input_position), all_of(input_rankers)), na.rm = TRUE) * 1.06, 3),
                                                        .data$Pos == "RB" ~ round(rowMeans(select(filter(input_df, .data$Pos %in% input_position), all_of(input_rankers)), na.rm = TRUE) * 1.03, 3),
                                                        .data$Pos == "WR" ~ round(rowMeans(select(filter(input_df, .data$Pos %in% input_position), all_of(input_rankers)), na.rm = TRUE) * 1.00, 3),
                                                        .data$Pos == "TE" ~ round(rowMeans(select(filter(input_df, .data$Pos %in% input_position), all_of(input_rankers)), na.rm = TRUE) * 0.96, 3))) |>
          dplyr::arrange(dplyr::desc(.data$`ADJ Grade`)) |>
          dplyr::mutate(Rank = 1:dplyr::n())
      } else {
        df <- input_df |>
          dplyr::select(c(.data$Player:.data$`AVG Grade`), all_of(input_rankers)) |>
          dplyr::mutate(`AVG Grade` = round(rowMeans(select(input_df, all_of(input_rankers)), na.rm = TRUE), 3),
                        `ADJ Grade` =  dplyr::case_when(.data$Pos == "QB" ~ round(rowMeans(select(input_df, all_of(input_rankers)), na.rm = TRUE) * 1.06, 3),
                                                        .data$Pos == "RB" ~ round(rowMeans(select(input_df, all_of(input_rankers)), na.rm = TRUE) * 1.03, 3),
                                                        .data$Pos == "WR" ~ round(rowMeans(select(input_df, all_of(input_rankers)), na.rm = TRUE) * 1.00, 3),
                                                        .data$Pos == "TE" ~ round(rowMeans(select(input_df, all_of(input_rankers)), na.rm = TRUE) * 0.96, 3))) |>
          dplyr::arrange(dplyr::desc(.data$`ADJ Grade`)) |>
          dplyr::mutate(Rank = 1:dplyr::n()) |>
          dplyr::filter(.data$Player %in% input_player)
      }
    }

  return(df)
}
