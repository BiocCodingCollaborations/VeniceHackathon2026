## run shiny assuming images and polygons are aligned
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(ggspatial)
library(ggplot2)

library(plotly)
library(shiny)
library(shinycssloaders)

draw_poly <- function(img_rast) {
  # Validate input
  stopifnot("Input must be a SpatRaster" = inherits(img_rast, "SpatRaster"))
  
  ui <- fluidPage(
    titlePanel("Generate polygon"),

    sidebarLayout(
      sidebarPanel(
        actionButton("done", "Return polygon"),
        hr(),
        verbatimTextOutput("poly_coords")
      ),

      mainPanel(
        width = 9,
        shinycssloaders::withSpinner(
        plotlyOutput(
          outputId = "spatraster",
          height   = "600px"
        ), type=8, size=0.5)
    )
  )
)
  
  server <- function(input, output, session) {
    
    output$spatraster <- renderPlotly({

      # create static plot
      pl <- ggplot() +
        geom_spatraster(data = img_rast, maxcell=224*224) +
        scale_fill_viridis_c() +
        theme_minimal() +
        theme(legend.position = "none")

      # make plot interactive
      pl_interactive <- ggplotly(pl, tooltip = FALSE, source = "interactive_plot") |>
        layout(dragmode="drawrect", 
      newshape=list(line = list(color="white"))) 

      # customise modebar buttons 
      pl_interactive <- pl_interactive |>
        config(modeBarButtonsToAdd = c(
                                 "drawclosedpath", 
                                 "drawcircle", 
                                 "drawrect", 
                                 "eraseshape"),
              modeBarButtonsToRemove = c("toImage",
      "hoverClosestCartesian", "hoverCompareCartesian"))
    })

    poly_out <- reactiveVal(NULL)
    shape_type <- reactiveVal(NULL)

    observeEvent(event_data(event = "plotly_relayout", source = "interactive_plot"), {
      ed <- event_data(event = "plotly_relayout", source = "interactive_plot")
      req(!is.null(ed$shapes$type)) # validate
      shape_type(ed$shapes$type)

      if (shape_type() == "rect") {
        poly_out(c(
          xmin = min(ed$shapes$x0, ed$shapes$x1),
          xmax = max(ed$shapes$x0, ed$shapes$x1),
          ymin = min(ed$shapes$y0, ed$shapes$y1),
          ymax = max(ed$shapes$y0, ed$shapes$y1)
        ))
      }

      if (shape_type() == "circle") {
        poly_out(c(
          cx = (ed$shapes$x0 + ed$shapes$x1)/2,
          cy = (ed$shapes$y0 + ed$shapes$y1)/2,
          rx = abs(ed$shapes$x1 - ed$shapes$x0)/2,
          ry = abs(ed$shapes$y1 - ed$shapes$y0)/2
        ))
      }

      if (shape_type() == "path") {
        path_str <- ed$shapes$path
        path_num <- as.numeric(str_extract_all(path_str, "[0-9.]+", simplify = TRUE))
        xs = path_num[seq(1, length(path_num), by=2)]
        ys = path_num[seq(2, length(path_num), by=2)]
        mat <- cbind(xs, ys)
        mat <- rbind(mat, mat[1, ]) # close the polygon
        colnames(mat) <- c("x", "y")
        poly_out(mat)
      }
    })

      # show coordinates
      output$poly_coords <- renderPrint({
        req(poly_out()) # validate
        # read the value
        poly_out()
      })
    
    observeEvent(input$done, {
      if (!is.null(poly_out())) {

        if (shape_type() == "rect") {
          rect <- poly_out()
          sf_df <- sf::st_bbox(rect) |>
              sf::st_as_sfc() |>
              sf::st_as_sf()
        }

        if (shape_type() == "circle") {
          circle <- poly_out()
          cx <- circle["cx"]
          cy <- circle["cy"]
          rx <- circle["rx"]
          ry <- circle["ry"]
          theta <- seq(0, 2 * pi, length.out = 100)
          ellipse_mx <- cbind(
            x = cx + rx * cos(theta),
            y = cy + ry * sin(theta)
          )
          # close the polygon
          ellipse_mx <- rbind(ellipse_mx, ellipse_mx[1, ])
          
          sf_df <- sf::st_polygon(list(ellipse_mx)) |>
            sf::st_sfc() |>
            sf::st_as_sf()
        }

        if (shape_type() == "path") {
          path <- poly_out()
          
          sf_df <- sf::st_polygon(list(path)) |>
            sf::st_sfc() |>
            sf::st_as_sf()
      }
        stopApp(sf_df)
      }
  })
  }
  runApp(shinyApp(ui, server))
}