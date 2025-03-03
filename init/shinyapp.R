#

library(shiny)
library(metafor)
library(glue)
library(ggplot2)
require(meta)
require(grid)
library(dplyr)


ui <- fluidPage(
  titlePanel("Forest Plot Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV Data",
                accept = c(".csv")),
      actionButton("run_meta", "Run Meta-Analysis"),
      checkboxInput("save_plot", "Save Plot", value = FALSE),
      checkboxInput("alt_plot", "Alternative Plot", value = FALSE),
      textInput("plot_title", "Plot Title", "My Title"),
      textInput("reference", "Reference", "Reference"),
      textInput("comparator", "Comparator", "Comparator"),
      textInput("filename", "Save as (if enabled)", "forest.png"),
      sliderInput("plot_width", "Plot Width", min = 5, max = 20, value = 12),
      sliderInput("plot_height", "Plot Height", min = 5, max = 20, value = 11),
      downloadButton("download_plot", "Download Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Forest Plot", plotOutput("forestPlot")),
        tabPanel("Input Data", tableOutput("data_table"))
      )
    )
  )
)

# column headings boxes
# export to ppt (make sure to be able to edit)
# tables
# column, footer spacing





server <- function(input, output, session) {
  
  dat2 <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  output$data_table <- renderTable({
    req(dat2())
    dat2()
  })
  
  # Run meta-analysis
  meta_out <- eventReactive(input$run_meta, {
    req(dat2())
    metafor::rma(yi, vi, data = dat2(), method = "DL")
  })
  
  # Render forest plot
  output$forestPlot <- renderPlot({
    req(meta_out())
    
    forest_plot(meta_out(),
                save = input$save_plot,
                altplot = input$alt_plot,
                title_text = input$plot_title,
                ref = input$reference,
                comp = input$comparator,
                filename = input$filename,
                width = input$plot_width,
                height = input$plot_height)
  }, width = function() input$plot_width * 96,
     height = function() input$plot_height * 96)
  
  # Download functionality
  output$download_plot <- downloadHandler(
    filename = function() { input$filename },
    content = function(file) {
      png(file, width = input$plot_width * 96,
          height = input$plot_height * 96, res = 96)
      forest_plot(meta_out(),
                  save = TRUE,
                  altplot = input$alt_plot,
                  title_text = input$plot_title)
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
