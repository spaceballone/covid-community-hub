mapUI <- function(id){
  ns <- NS(id)
  
  fluidPage(theme = "bootstrap.css",
    fixedRow(style = "height:96px; max-height:96px",
      column(width = 7,
             wellPanel(uiOutput(ns("selector")))),
      column(width = 5,
             wellPanel(uiOutput(ns("timelineSlider"))))
    ),
    fluidRow(class="myRow1",
      column(width = 6, 
             wellPanel(leafletOutput(outputId = ns("outmap")))),
      column(width = 6,
             fluidRow(
                column(width = 4, 
                       wellPanel(gt_output(outputId = ns("confirmedTable")), class="smallMetrics")),
                column(width = 4, 
                       wellPanel(gt_output(outputId = ns("deathsTable")), class="smallMetrics")),
                column(width = 4, 
                       wellPanel(gt_output(outputId = ns("recoveredTable")), class="smallMetrics"))
             ),
             fluidRow(
               column(width = 12, 
                      wellPanel(class="tabMetrics",
                        tabsetPanel(type = "tabs",
                                tabPanel("Confirmed", plotlyOutput(outputId = ns("confirmedPlot"),width="100%",height="175px")),
                                tabPanel("Active", plotlyOutput(outputId = ns("activePlot"),width="100%",height="175px")),
                                tabPanel("Deaths", plotlyOutput(outputId = ns("newCasesPlot"),width="100%",height="175px"))
                        
               ))
      ))
    )
    ),
    fluidRow(
      column(width = 12,
      wellPanel(style="height: 450px",
          radioButtons(ns("forecastRadioButtons"),"Forecast:",c("King, Washington"),inline = TRUE),
          plotlyOutput(outputId = ns("forecastPlot"), width = "100%"))
      )
      #        wellPanel("Forecasting", plotlyOutput(outputId = ns("active_per_inhabitant"), height="auto", width="auto",inline=T))
    ),
    tags$head(tags$style("
      .myRow1{height:600pxpx; max-height:600px;} 
      .smallMetrics{height:150px;
          padding-right: 5px;
          padding-left: 5px;
          padding-top: 10px;
          padding-bottom: 10px;
          width: 100%;
        }
      .smallTable{height:120px}
      .tabMetrics{height:250px}
      .inputStyle{height:96px;width:100%;padding-left:3%;padding-right:3%}
      .inputStyle > .selectize-control.multi .selectize-input.has-items {
           background-color: red;
        }"
    )
  )

  )
  
}

map <- function(input, output, session,all_dates = NULL, map_data = NULL) {
  
  session$userData$showEx <- reactiveVal(TRUE)
  
  default_countries <- c("US")
  
  default_counties <- c("King, Washington", "Williamson, Tennessee","Lincoln, Tennessee")

  output$selector = renderUI({
    unique_entries <- unique(full_data()$Combined_Key)
    
    tagList(
      div(class="inputStyle",
          selectInput(inputId = session$ns('countries'), width="100%",
                  'Select Counties you want to add:', sort(unique_entries),
                  selected = default_counties, multiple = TRUE)),
    #  div(style="clear:both;height:20px;")
    )
  })
  
  output$timelineSlider <- shiny::renderUI({
    
    div(style="height:96px;width:100%;padding-left:3%;padding-right:3%",
      shiny::sliderInput(inputId = session$ns("datum"),
                         min = as.POSIXct("2020-03-23"),
                         max = max(all_dates()),
                         value = max(all_dates()),
                         step = 86400,
                         label = "Date", timeFormat="%Y-%m-%d", width = "100%",
                         animate = animationOptions(interval = 1000))
    )
  })
  
  
  curr_date <- reactiveVal( as.character(Sys.Date() + 1))
  
  full_data <- reactive({
    get_dat <- map_data() %>%
      filter(country.x == "US") %>%
      filter(!grepl("County",Combined_Key))
    return(get_dat)
    })
  
  output$outmap <- renderLeaflet({
    
    mapout <- leaflet_static <- leaflet() %>% addProviderTiles(providers$ CartoDB.Positron) %>%
      setView(-98.58,39.82,3)
    
    return(mapout)
  })

  us_data <- reactive({
    full_data() %>% 
      filter(country.x %in% default_countries)%>% # & (Combined_Key %in% default_counties)) %>% 
      filter(date < curr_date())  %>%
      group_by(country.x,date) %>%
      summarise(confirmed = sum(confirmed), deaths = sum(deaths), active = sum(active)) %>%
      # This is a hack
      mutate(Combined_Key = "US")
  })
  
  # ---- plot_data_intern ----
  plot_data_intern <- reactive({
    
    if (!is.null(input$countries)){
      full_data() %>% filter(Combined_Key %in% input$countries)%>% 
        filter(date < curr_date())
    }else {
      # Let's default to the united states
      us_data()
    }
  })
  
  #Colors for the charts
  palette_col <- reactive({
    plot_data_intern2 <- plot_data_intern() 
    viridisLite::viridis(n = length(unique(plot_data_intern2$Combined_Key)))
  })
  
  # ---- plotLyGroup ----
  plotly_group <- reactive({
    
    plot_data_intern2 <- plot_data_intern() 
    # generate bins based on input$bins from ui.R
    
    palette <- palette_col()
    plot_ly(
      data = plot_data_intern2,
      hoverinfo = "",
      type = "scatter",
      transforms = list(
        list(
          type = 'groupby',
          groups = plot_data_intern2$Combined_Key,
          styles = lapply(seq_along(unique(plot_data_intern2$Combined_Key)), function(x){
            
            list(target = unique(plot_data_intern2$Combined_Key)[x], value = list(line = list(color = palette[x]), 
                                                                             marker = list(color = alpha(palette[x], 0.6))))
          })
        )
      )
    )
  })

  output$confirmedPlot <- renderPlotly({
    plt_out <- plotly_group() %>%
      add_trace(
        x = ~date,
        y = ~confirmed,
        name = "Confirmed cases",
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        xaxis = list(
          title = "Date",
          range = c(as.POSIXct("2020-03-23"),max(all_dates()))
        ),
        yaxis = list(
          title = "Total cases",
          range = c(0, max(as.numeric(plot_data_intern()$value), na.rm = TRUE) + 1)
        ),
        showlegend = FALSE
      )
    return(plt_out)
  })
  
  #This is the same value. Need to change it.
  output$activePlot <- renderPlotly({
    
    plt_out <- plotly_group() %>%
      add_trace(
        x = ~date,
        y = ~active,
        name = "Active cases",
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        xaxis = list(
          title = "Date",
          range = c(as.POSIXct("2020-03-23"),max(all_dates()))
        ),
        yaxis = list(
          title = "Total cases",
          range = c(0, max(as.numeric(plot_data_intern()$value), na.rm = TRUE) + 1)
        ),
        showlegend = FALSE
      )
    return(plt_out)
  })
  
  #This is the same value. Need to change it.
  output$newCasesPlot <- renderPlotly({
    
    plt_out <- plotly_group() %>%
      add_trace(
        x = ~date,
        y = ~deaths,
        name = "Deaths",
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        xaxis = list(
          title = "Date",
          range = c(as.POSIXct("2020-03-23"),max(all_dates()))
        ),
        yaxis = list(
          title = "Total deaths",
          range = c(0, max(as.numeric(plot_data_intern()$value), na.rm = TRUE) + 1)
        ),
        showlegend = FALSE
      )
    return(plt_out)
  })
  
  gt_confirmed_tbl <- reactive({
    df_data <- plot_data_intern()
    
    gt <- df_data %>% mutate(Combined_Key = substr(Combined_Key,0,10)) %>% group_by(Combined_Key) %>% summarize(confirmed = last(confirmed)) %>% gt() %>% 
      tab_options(column_labels.hidden = TRUE,table.border.top.style = 'none',table.font.size = 10,container.width = pct(100),table.width = pct(100),data_row.padding = px(3)) %>%
      tab_header(
        title = md("Confirmed")
      ) %>%
      opt_align_table_header(align = "left") %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "#FFFFFF",
          weight = px(1.0), 
          style = "solid") ,
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))  %>% cols_move_to_end(
           columns = vars(Combined_Key)
      )
    
    return(gt)
    })
  
  output$confirmedTable <- render_gt(
    expr = gt_confirmed_tbl()
  )
  
  gt_deaths_tbl <- reactive({
    df_data <- plot_data_intern()
    gt <- df_data %>% mutate(Combined_Key = substr(Combined_Key,0,10)) %>% group_by(Combined_Key) %>% summarize(deaths = last(deaths)) %>% gt() %>% 
      tab_options(column_labels.hidden = TRUE,table.border.top.style = 'none',table.font.size = 10,container.width = pct(100),table.width = pct(100),data_row.padding = px(3)) %>%
      tab_header(
        title = md("Deaths")
      ) %>%
      opt_align_table_header(align = "left") %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "#FFFFFF",
          weight = px(1.0), 
          style = "solid") ,
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))  %>% cols_move_to_end(
          columns = vars(Combined_Key)
        )
    
    return(gt)
  })
  
  output$deathsTable <- render_gt(
    expr = gt_deaths_tbl()
  )
  
  gt_recovered_tbl <- reactive({
    df_data <- plot_data_intern()
    gt <- df_data %>% mutate(Combined_Key = substr(Combined_Key,0,10)) %>% group_by(Combined_Key) %>% summarize(recovered = last(recovered)) %>% gt() %>% 
      tab_options(column_labels.hidden = TRUE,table.border.top.style = 'none',table.font.size = 10,container.width = pct(100),table.width = pct(100),data_row.padding = px(3)) %>%
      tab_header(
        title = md("Recovered")
      ) %>%
      opt_align_table_header(align = "left") %>%
      tab_style(
        style = cell_borders(
          sides = "all",
          color = "#FFFFFF",
          weight = px(1.0), 
          style = "solid") ,
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))  %>% cols_move_to_end(
          columns = vars(Combined_Key)
        )
    
    return(gt)
  })
  
  
  output$recoveredTable <- render_gt(
    expr = gt_recovered_tbl()
  )
  
  observeEvent(input$datum, {
     
     date_to_choose <- if (is.null(input$datum)) {
       as.character(Sys.Date() - 1)
     } else {
       as.character(input$datum)
     }
     
     only_numeric <- sort(as.numeric(unique(full_data()$active)))
     
     col_pal <- colorNumeric(
       rev(viridisLite::magma(99)[1:78]),
       domain = c(min(only_numeric, na.rm = TRUE), max(only_numeric, na.rm = T))
     )
     
     max_val <- max(only_numeric, na.rm = T)
     
     data_for_display <- full_data() %>%
       filter(date == as.character(date_to_choose)) %>%
       select(Lat, Long, active, date, Combined_Key) %>%
       filter(active > 0) %>%
       filter(!is.na(Long) & !is.na(Lat)) %>%
       mutate(active_scaled = case_when(
         grepl(pattern = "\\,\\s{0,1}US", x = Combined_Key) &
           as.Date(date_to_choose, origin = "1970-01-01") > as.Date("2020-03-21", origin = "1970-01-01") ~ scales::rescale(
             x = active, from = c(0, max_val), to = c(12000, 650000)
           ),
         TRUE ~ scales::rescale(x = active, from = c(0, max_val), to = c(60000, 450000))
       ),
       text = paste0(as.character(Combined_Key), "\n", active),
       color = col_pal(active)
       ) %>%
       arrange(active)
     material_spinner_hide(session, session$ns("wait"))
     
     leafletProxy(mapId = "outmap") %>%
       clearGroup(curr_date()) %>%
       addCircles(data = data_for_display, lng = ~Long, lat = ~Lat, layerId = ~Combined_Key,
                  radius = ~active_scaled, popup = ~text, fillColor = ~color, stroke = FALSE, fillOpacity = 0.5,
                  group = stringr::str_match(date_to_choose, "\\d{4}\\-\\d{2}\\-\\d{2}")[1,1]
     )
     
     curr_date(stringr::str_match(date_to_choose, "\\d{4}\\-\\d{2}\\-\\d{2}")[1,1])
     
     
   })
  
  observe({
    updateRadioButtons(session,"forecastRadioButtons",
                       label = "Forecast: ",
                       unique(plot_data_intern()$Combined_Key),
                       inline = TRUE)
  })
  
  output$forecastPlot <- renderPlotly({
    forecasted_county <- input$forecastRadioButtons
    
    if(is.null(forecasted_county) | forecasted_county == "Item A")
      forecasted_county <- "King, Washington, US"
    
    us_deaths <- full_data() %>%
      filter(Combined_Key == forecasted_county) %>%
      top_n(21,date) %>%
      select(date,confirmed)

    us_ts <- log(ts(us_deaths$confirmed,start=1,end=21,frequency = 1))

    days_to_forecast <- 5

    f <- us_ts %>%
      ets(model="AAN", damped=FALSE) %>%
      forecast::forecast(h=days_to_forecast)


    f$upper  <- exp(f$upper)
    f$lower  <- exp(f$lower)
    f$mean <- exp(f$mean)
    f$fitted <- exp(f$fitted)
    f$x <- exp(f$x)

    dates <- seq(max(us_deaths$date)+1,by=1,len=days_to_forecast)

    #f %>% autoplot()
    palette <- palette_col()

    colChoice <- match(input$forecastRadioButtons,unique(plot_data_intern()$Combined_Key))
    
    
    plot <- plot_ly() %>%
      add_lines(x = us_deaths$date, y = us_deaths$confirmed,
                name = "observed",
                marker=list(mode='lines',color = alpha(palette[colChoice], 0.6)),
                line = list(color = palette[colChoice])) %>%
      add_lines(x = dates, y = f$mean, color = I("blue"), name = "prediction") %>%
      add_ribbons(x = dates,
                  ymin = f$lower[, 2],
                  ymax = f$upper[, 2],
                  color = I("gray95"),
                  name = "95% confidence") %>%
      add_ribbons(x = dates,
                  ymin = f$lower[, 1],
                  ymax = f$upper[, 1],
                  color = I("gray80"), name = "80% confidence")
    # %>%
      # layout(
      #   showlegend = FALSE
      #   )
    return(plot)
  })
  
  observeEvent(input$outmap_shape_click, { 
    e <- input$outmap_shape_click  
    if (is.null(e))
      return()
    currentSelection <- input$countries
    updateSelectInput(session,"countries",selected = c(currentSelection, e$id))
  })
  
}

