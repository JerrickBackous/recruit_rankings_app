#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_google
#' @importFrom waiter use_waiter waiter_on_busy spin_hexdots transparent
#' @importFrom sever useSever
#' @importFrom shinyWidgets prettyRadioButtons awesomeCheckboxGroup sliderTextInput
#' @importFrom reactable reactableOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(
        bg = "#FFF",
        fg = "#6c0000",
        primary = "#6c0000",
        secondary = "#6c0000",
        base_font = bslib::font_google("Roboto")
      ),
      tags$head(
        # Styling Well
        tags$style(
          type = "text/css",
          ".well {
               background-color: #FFF;
               }",
          ".btn-light {
               color: #984D4D;
               background-color: #FFF;
               border-color: #C49999;
               }"
        ),
        tags$style(HTML(
          "table.dataTable tr.selected td, table.dataTable td.selected {
               background-color: #6c0000 !important;
               }"
        ))
      ),
      waiter::use_waiter(),
      waiter::autoWaiter(html = waiter::spin_hexdots(), color = waiter::transparent(0.3)),
      sever::useSever(),
      p(style = "text-align: right;", paste0("Updated: ", update_date())),
      fluidRow(
        column(
          width = 4,
          shinyWidgets::awesomeCheckboxGroup("position", label = "Select Position", choices = c("QB", "RB", "WR", "TE"), selected = c("QB", "RB", "WR", "TE"), inline = TRUE)
        ),
        column(
          width = 4,
          selectizeInput("player", label = "Select Player(s)", choices = NULL, multiple = TRUE)#,
          # shinyWidgets::pickerInput("columns", label = "Select Columns", choices = NULL, multiple = TRUE)
        ),
        column(
          width = 4,
          shinyWidgets::awesomeCheckboxGroup("rankers", label = "Select Ranker(s)", choices = NULL, inline = TRUE, selected = NULL),
          actionButton("selectall", label = "Select/Deselect All")
        )
      ),
      fluidRow(
        column(
          width = 12,
          reactable::reactableOutput("rankings_table")
        )
      ),
      fluidRow(
        column(
          offset = 9,
          width = 3,
          align = "right",
          downloadButton('downloadData', 'Download data')
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "recruit_rankings_app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
