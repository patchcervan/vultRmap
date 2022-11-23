require(shiny)
require(vultRmap)
require(dplyr, warn.conflicts = FALSE)
require(sf)
require(MASS)
require(raster)
require(leaflet)
# require(mapview)


# Set future maxsize to 850MB
options(future.globals.maxSize = 850*1024^2)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Cape Vulture activity simulations"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      numericInput(inputId = "seed",
                   label = "Seed",
                   value = 1979),
      numericInput(inputId = "ncores",
                   label = "n. vultures (cores)",
                   value = 6),
      checkboxGroupInput(inputId = "col_indx",
                         label = "Colonies",
                         choices = c("1","2","3","4","5"),
                         inline = FALSE),
      radioButtons(inputId = "age",
                   label = "Vulture age",
                   choices = list(adult = "ad", juvenile = "juv"),
                   selected = NULL,
                   inline = FALSE),
      numericInput(inputId = "max_dist",
                   label = "Max. dist. (km)",
                   value = 1000),
      numericInput(inputId = "min_dist",
                   label = "Trip dist. (km)",
                   value = 5),
      numericInput(inputId = "total_steps",
                   label = "n. steps",
                   value = 1000),
      actionButton("do", "Simulate"),
      downloadButton("download", "Save map"),

      width = 2),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("simsPlot"),

      width = 10)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Create data loading function --------------------------------------------

  load_data <- reactive({

    list(
      # Test colony data
      col_all = vultRmap::test_colonies,
      # Test supplementary feeding sites data
      sfs = vultRmap::test_sup_feeding,
      # Sample random coefficients from fitted random effects. We choose 6 in this case
      # because we will be using 6 cores for the simulations and it will be convenient
      ssf_coef = vultRmap::sampleSsfCoef(vultRmap::ssf_fit_summary,
                                         nind = input$ncores,
                                         seed = input$set_seed)
    )

  }) %>%
    bindEvent(input$do)



  # Create simulation function ----------------------------------------------

  run_sims <- reactive({

    # Load data
    gen_data <- load_data()

    message("Busy simulating, please be patient...")

    # Define the total number of steps and calculate the number of steps per core
    nsteps <- rep(ceiling(input$total_steps/input$ncores), input$ncores) # rounded up, so that we get AT LEAST the desired number of steps


    # Prepare habitat for simulations -----------------------------------------

    # Here we need to define the habitat around each colony to simulate activity
    # simulating over the entire world would be impossible, so we need to define a
    # maximum distance from the colony a vulture is allowed to travel. Here we set
    # this distance to 1000 km.

    # Select colony of interest
    idxs <- as.integer(input$col_indx)
    simss <- vector("list", length = length(idxs))

    for(i in seq_along(idxs)){

      col_sel <- gen_data$col_all[idxs[i],]

      # We define the habitat surrounding the colony of interest in our test data.
      # The argument scale will scale the value of the covariates to match the scaling
      # of the SSF model.
      hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[1, c("lon", "lat")]),
                                  max_range = input$max_dist + 100,
                                  col_all = gen_data$col_all,
                                  sfs = gen_data$sfs,
                                  scale = "ssf")

      # Complete data frame with interactions and indicator variables
      hab <- vultRmap::completeDataFrame(hab, names(gen_data$ssf_coef[[1]]), 0)


      # Simulate activity -------------------------------------------------------

      max_dist <- input$max_dist
      set_seed <- input$set_seed
      age <- input$age
      min_dist <- input$min_dist

      gc()

      # Configure multicore
      future::plan("multisession", workers = input$ncores)

      # Simulate
      simss[[i]] <- furrr::future_map2_dfr(nsteps, gen_data$ssf_coef,
                                           ~vultRmap::simTrips(.nsteps = .x, .ssf_coef = .y,
                                                               .age = age, .hab = hab,
                                                               .mov_ker = vultRmap::mov_kernel,
                                                               .col_sel = col_sel,
                                                               .maxdist = max_dist,
                                                               .mindist = min_dist),
                                           .options = furrr::furrr_options(seed = set_seed))

      future::plan("sequential")

    }

    dplyr::bind_rows(simss)

  }) %>%
    bindEvent(input$do)


  # Plotting function -------------------------------------------------------

  make_plot <- reactive({

    # Run simulations
    sims <- run_sims()

    # Load data
    gen_data <- load_data()

    message("Plotting, almost there...")

    # Make spatial objects to plot
    col_sf <- st_as_sf(gen_data$col_all, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    sfs_sf <- st_as_sf(gen_data$sfs, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

    # Estimate point density
    sims_sf <- st_as_sf(sims, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

    coords <- st_coordinates(sims_sf)

    matrix <- MASS::kde2d(coords[,1], coords[,2], n = 1000,
                          lims = c(range(coords[,1])+c(-1,1),
                                   range(coords[,2])+c(-1,1)))

    dens <- raster::raster(matrix)

    # Plot leaflet
    nlevels <- 6
    udlevels <- c(0, 0.25, 0.5, 0.75, 0.9, 1)
    bins <- calcUDquantile(raster::getValues(dens), udlevels)
    bins[which.min(bins)] <- 0

    rcl_vals <- rep(rev(bins), each = 2)
    rcl_vals <- rcl_vals[c(-1, -length(rcl_vals))]
    rcl_mat <- matrix(rcl_vals, ncol = 2, byrow = TRUE)
    rcl_mat <- cbind(rcl_mat, rev(udlevels[-1]*100))

    dens_discrete <- reclassify(dens, rcl_mat)
    dens_discrete[dens_discrete > 90] <- NA

    qpal <- colorFactor(rev(viridis::plasma(nlevels-2)), unique(getValues(dens_discrete)), #bins = bins,
                        na.color = "#00000000", reverse = TRUE)

    # Function to modify legend circle size
    addLegendCustom <- function(map, colors, labels, sizes, ...){
      colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px; border:1px solid black")
      labelAdditions <- paste0("<div style='display: inline-block;height: ",
                               sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
                               labels, "</div>")

      return(addLegend(map, colors = colorAdditions,
                       labels = labelAdditions, ...))
    }

    # Define simulations point size and opacitiy
    sim_size <- ifelse(input$total_steps < 3000, 2, 1)
    sim_opac <- ifelse(input$total_steps < 3000, 0.5, 0.2)

    leaflet() %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      addRasterImage(dens_discrete, colors = qpal(rev(unique(getValues(dens_discrete)))), opacity = 0.5) %>%
      # addPolygons(data = dens_pol) %>%
      # addCircleMarkers(data = sims_sf,
      #                  label = sims_sf$id,
      #                  radius = sim_size,
      #                  color = "black", weight = 1, stroke = F, opacity = sim_opac) %>%
      addCircleMarkers(data = col_sf,
                       label = col_sf$id,
                       radius = 5,
                       color = "black", fillColor = "red", weight = 2, stroke = TRUE,
                       opacity = 1, fillOpacity = 0.7) %>%
      addCircleMarkers(data = sfs_sf,
                       label = col_sf$id,
                       radius = 5,
                       color = "blue", weight = 1, stroke = F,
                       opacity = 1, fillOpacity = 0.7) %>%
      addLegend("bottomright", pal = qpal, values = unique(getValues(dens_discrete)),
                title = "Cumulative </br> distribution",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1) %>%
      addLegendCustom(colors = c("red", "blue"),
                      labels = c("Colony", "Sup. feeding"),
                      sizes = c(6, 6),
                      opacity = 1,
                      # stroke = c(TRUE, FALSE),
                      position = "topright",
                      group = "circles",
                      title = "")




  }) %>%
    bindEvent(input$do)


  # Simulate and plot -------------------------------------------------------

  observe({
    output$simsPlot <- renderLeaflet({
      make_plot()
    })
  }) %>% bindEvent(input$do)

  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  observe({
    output$map <- downloadHandler(
      filename = paste0(Sys.Date(), "_capeVulture_Leafletmap", ".html"),
      # content = function(file) {
      #   mapshot( x = make_plot(), file = file,
      #            cliprect = "viewport", # the clipping rectangle matches the height & width from the viewing port
      #            selfcontained = FALSE) # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      #
      #   # saveWidget(vult_plot, file = file)

        # }
    )
  }) %>% bindEvent(input$download)


}


# Run the application
shinyApp(ui = ui, server = server)
