library(shiny)
library(plotly)

data(diamonds, package = "ggplot2")


nm <- names(diamonds)

runApp(
  list(ui = fluidPage(
    title = 'Diamonds',
    sidebarLayout(
      sidebarPanel(
        sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(diamonds),
                    value = 1000, step = 500, round = 0),
        checkboxInput('jitter','jitter',value=TRUE),
        checkboxInput('smooth','smooth',value=TRUE),
        selectInput('x', 'X', choices = nm, selected = "carat"),
        selectInput('y', 'Y', choices = nm, selected = "price"),
        selectInput('color', 'Color', choices = nm, selected = "clarity"),
        selectInput('facet_row', 'Facet Row', c(None = '.', nm), selected = "clarity"),
        selectInput('facet_col', 'Facet Column', c(None = '.', nm)),
        sliderInput('plotHeight', 'Height of plot (in pixels)', 
                    min = 100, max = 2000, value = 1000)),
      mainPanel(
        plotlyOutput('trendPlot', height = "900px"),
        tabsetPanel(
          id = 'dataset',
          tabPanel('diamonds', dataTableOutput('mytable1'))
        )
      )
    )
  ),
  
  server = shinyServer(function(input, output) {
    
    dataset <- reactive({
      diamonds[sample(nrow(diamonds), input$sampleSize),]
    })
    # a large table, reative to input$show_vars
    output$mytable1 <- renderDataTable({
      dataset[, input$show_vars, drop = FALSE]
    })
    output$trendPlot <- renderPlotly({
      
      # build graph with ggplot syntax
      p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
        geom_point()
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      # smoothing
      if (input$jitter)
        p<-p+geom_jitter()
      if (input$smooth)
        p<- p+geom_smooth()
      
      ggplotly(p) %>% 
        layout(height = input$plotHeight, autosize=TRUE)})
  })
  )
)
