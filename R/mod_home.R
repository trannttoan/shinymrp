#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(class = "landing_container",
      tags$h1("M.R.P.", class = "landing_header"),
      tags$p("A user-friendly graphical interface for applying Multilevel Regression and Poststratification (MRP) to your dataset.", class = "landing_subheader"),
      tags$div(class = "landing_buttons justify",
        actionButton(
          inputId = ns("set_poll"),
          label = tags$h1("Cross-sectional Data")
        ),
        actionButton(
          inputId = ns("set_covid"),
          label = tags$h1("Spatio-temporal Data", tags$br(), "with Measurement Error")
        )
      ),
      tags$div(class = "pad_top",
        tags$p(tags$i("Note: This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau."), style = "text-align: center")
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$set_poll, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")

      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")

      show_notif("Switched to interface for cross-sectional data", global$session)

      global$covid <- FALSE
    })


    observeEvent(input$set_covid, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")

      updateNavbarPage(global$session,
                       inputId = "navbar_analyze",
                       selected = "nav_analyze_upload")


      show_notif("Switched to interface for spatio-temporal data with measurement error", global$session)

      global$covid <- TRUE
    })


  })
}
