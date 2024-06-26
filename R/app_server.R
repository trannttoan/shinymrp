#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  options(shiny.maxRequestSize = 100*1024^2)

  global <- reactiveValues(
    session = session,
    input = input,
    static = list(
      bounds = list(
        covid = list(
          age = c(0, 18, 35, 65, 75)
        ),
        poll = list(
          age = c(18, 30, 40, 50, 60, 70)
        )
      ),
      levels = list(
        covid = list(
          sex = c("male", "female"),
          race = c("white", "black", "other"),
          age = c("0-17", "18-34", "35-64", "65-74", "75+")
        ),
        poll = list(
          sex = c("male", "female"),
          race = c("white", "black", "other"),
          age = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
          edu = c("no hs", "hs", "some college", "4-year college", "post-grad")
        )
      ),
      expected_columns = list(
        covid = c("sex", "race", "age", "zip", "time", "date", "total", "positive"),
        poll = c("sex", "race", "age", "edu", "state", "total", "positive")
      ),
      ui = list(
        max_model = 5,
        iter_range = c(100, 5000),
        chain_range = c(1, 8),
        plot_height = 500,
        subplot_height = 300
      )
    ),
    extdata = list(
      covid = list(
        zip_tract = readr::read_csv(app_sys("extdata/zip_tract.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c")),
        tract_data = readr::read_csv(app_sys("extdata/acs_data.csv"), show_col_types = FALSE),
        map_geojson = readRDS(app_sys("extdata/county_geojson.RDS")),
        fips = readr::read_csv(app_sys("extdata/fips.csv"), show_col_types = FALSE)
      ),
      poll = list(
        pstrat_data = readr::read_csv(app_sys("extdata/pstrat.csv"), show_col_types = FALSE),
        map_geojson = readRDS(app_sys("extdata/state_geojson.RDS")),
        fips = readr::read_csv(app_sys("extdata/fips.csv"), show_col_types = FALSE) |> mod_fips()
      )
    ),
    mrp_input = NULL,
    plotdata = NULL,
    models = NULL,
    model_count = 0
  )

  mod_home_server(module_ids$home, global)
  mod_analyze_upload_server(module_ids$analyze$upload, global)
  mod_analyze_visualize_server(module_ids$analyze$visualize, global)
  mod_analyze_model_server(module_ids$analyze$model, global)
  mod_analyze_result_server(module_ids$analyze$result, global)
  mod_learn_interface_server(module_ids$learn$interface, global)
}
