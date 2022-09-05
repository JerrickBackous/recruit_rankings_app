#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data
#' @importFrom dplyr select filter
#' @importFrom sever sever
#' @importFrom lubridate year today
#' @importFrom reactable renderReactable reactable colDef
#' @importFrom shinyWidgets updateAwesomeCheckboxGroup
#' @importFrom utils write.csv
#' @importFrom aws.s3 get_bucket s3readRDS
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logicsever::sever()

  rankings_data <- reactive({
    c2cbucket <- aws.s3::get_bucket(bucket = "campus2canton", region = "")

    aws.s3::s3readRDS(object = "app_data/recruit_rankings_list.rds", bucket = c2cbucket, region = "")
  })

  filtered_players <- reactive({
      rankings_data() |>
        dplyr::filter(.data$Pos %in% input$position) |>
        dplyr::select(.data$Player)

  })

  rankers <- reactive({
    rankings_data() |>
      dplyr::select(-c(.data$Player:.data$`AVG Grade`), -.data$`Player ID`)
  })

  # columns <- reactive({
  #   rankings_data() |>
  #     dplyr::select(c(.data$Player:.data$Weight))
  # })

  observe({
    updateSelectizeInput(session, "player", choices = c(filtered_players()), selected = NULL, server = TRUE)
  })

  # observe({
  #   shinyWidgets::updatePickerInput(session, "columns", choices = colnames(columns()), selected = colnames(columns()))
  # })

  observe({
    shinyWidgets::updateAwesomeCheckboxGroup(session, "rankers", choices = colnames(rankers()), inline = TRUE, selected = NULL) # "Select Metric",)
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0) {
        shinyWidgets::updateAwesomeCheckboxGroup(session, "rankers", choices = colnames(rankers()), inline = TRUE, selected = NULL) # "Select Metric",)
      } else {
        shinyWidgets::updateAwesomeCheckboxGroup(session, "rankers", choices = colnames(rankers()), inline = TRUE, selected = colnames(rankers())) # "Select Metric",)
      }
    }
  })

  output$rankings_table <- reactable::renderReactable(
    reactable::reactable(
      {
        rankings_table(rankings_data(), input$position, input$player, input$rankers)
      },
      filterable = TRUE,
      defaultColDef = reactable::colDef(sortNALast = TRUE),
      showPageSizeOptions = TRUE
    )
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("rankings-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(rankings_table(rankings_data(), input$position, input$player, input$rankers), file)
    })

  # Stop the app timing out
  observe({
    invalidateLater(10000, session)
    cat(".")
  })
}
