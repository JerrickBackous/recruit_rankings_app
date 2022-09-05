#' Update date
#'
#' @return Last date the data was updated
#'
#' @importFrom stringr str_sub
#' @importFrom aws.s3 head_object
update_date <- function() {
  lastmod <- aws.s3::head_object("app_data/recruit_rankings_list.rds", bucket = "campus2canton", region ="")

  df <- stringr::str_sub(attr(lastmod, "date"), end = -14L)

  return(df)
}
