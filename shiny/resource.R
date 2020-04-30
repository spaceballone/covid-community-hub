resourceUI <- function(id) {
  
  ns <- NS(id)
  fluidPage(
    tagList(
      fluidRow(
        wellPanel(width = 12,

                  tagList(
                    tags$p("All public COVID datasets across all Socrata customers"),
                  uiOutput(ns("selector")))
        )
      ),
      fluidRow(
        wellPanel(
            leafletOutput(outputId = ns("outmap"))
            )
        ),
      fluidRow(
        wellPanel(width=12,
                        div(style="",
                            #gt_output(outputId = ns("covidDatasets"))
                            DT::dataTableOutput(ns("covidDatasets"))
                        )
        )
      )
    )
  )
}

resource <- function(input, output, session) {
  
  catalog_url <- "http://api.us.socrata.com/api/catalog/v1?q=COVID"

  df_response <- fromJSON(catalog_url)
  
  crm_data <- read.csv("./dat/metadb.csv") %>%
    rename(account_name = `Account.Name`) %>%
    select(cname,account_name)

  account_loc <- read.csv("./dat/account_location.csv") %>%
    rename(account_name = `Account.Name`,
           location = `New.Georeferenced.Column`,
           long = lon) %>%
    select(account_name,lat,long) %>%
    distinct() 

  client_data <- reactive({
    df_data <- cbind(df_response$results$resource,df_response$results$metadata$domain,df_response$results$permalink) %>% rename(link = `df_response$results$permalink`,domain = `df_response$results$metadata$domain`) %>% 
      mutate(formatted_link = paste0("<a href='",link,"'>link</a>")) %>%
      arrange(link) %>%
      select(name,domain,description,createdAt,updatedAt,formatted_link) %>%
      left_join(crm_data,by = c("domain" = "cname")) %>%
      left_join(account_loc,by="account_name") %>%
      mutate(description = substr(description,0,1000)) 
  })

  filtered_data <- reactive({
    df_data <- client_data()
    if(is.null(input$clients))
      return(df_data)
    else
      return(df_data %>% filter(account_name %in% input$clients))
  })
  
  output$covidDatasets <- renderDT(
    {filtered_data() %>% select(account_name,name,description,createdAt,updatedAt,domain,formatted_link)}, options = list(lengthChange = FALSE,scrollX = TRUE,pageLength = 50),rownames = FALSE, escape = FALSE
  )
  
  # gt_tbl <- reactive({
  #   filtered_data() %>% group_by(account_name) %>% gt(groupname_col = "account_name") %>% tab_options(column_labels.hidden = TRUE,row_group.font.size = 24,  table.border.top.style = 'none',row_group.border.top.style = 'none')
  # })
  # 
  # output$covidDatasets <- render_gt(
  #   expr = gt_tbl()
  # )
  
  output$outmap <- renderLeaflet({
    df_results <- client_data()
    mapout <- leaflet_static <- leaflet() %>% 
      addProviderTiles(providers$ CartoDB.Positron) %>%
      setView(-98.58,39.82,3) %>%
      addCircles(data = df_results, lng = ~long, lat = ~lat,popup = ~account_name,layerId=~account_name)
    
    return(mapout)
  })
  
  output$selector = renderUI({
    df_results <- client_data()
    unique_entries <- unique(df_results$account_name)
    
    tagList(
      div(class="inputStyle",
          selectInput(inputId = session$ns('clients'), width="100%",
                      'Clients:', sort(unique_entries),
                       multiple = TRUE))
    )
  })
  
  observeEvent(input$outmap_shape_click, { 
    e <- input$outmap_shape_click 
    if (is.null(e))
      return()
    currentSelection <- input$clients
    updateSelectInput(session,"clients",selected = c(currentSelection, e$id))
  })
    
  
}