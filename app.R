library(shiny)
library(stringr)
library(seewave)
library(tuneR)

counts <- read.csv("data/flytunes-classifications-processed-counts.csv")
audio_files <- list.files("data/audio", pattern = "*.mp3")
counts <- counts[counts$file %in% audio_files,]

bins <- names(counts)[!(names(counts) %in% c("workflow_name", "workflow_version", "file"))]
n_bins <- str_replace(bins, "don.t", "don't")
n_bins <- str_replace_all(n_bins, "n\\.", "")
n_bins <- str_replace_all(n_bins, "\\.", " ")

ui <- fluidPage(
    titlePanel("FlyTunes classification review"),
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "workflow",
            "Workflow:",
            choices = unique(counts$workflow_name),
            selected = unique(counts$workflow_name)[1]),
          uiOutput("workflow_version"),
          uiOutput("file")
        ),

        mainPanel(
           plotOutput("spectrogram"),
           plotOutput("barPlot"),
        )
    )
)

server <- function(input, output) {
  output$workflow_version <- renderUI({
    selectInput(
      "workflow_version",
      "Workflow version:",
      choices = unique(counts[counts$workflow_name == input$workflow,]$workflow_version),
      selected = unique(counts[counts$workflow_name == input$workflow,]$workflow_version)[1])
  })
  
  output$file <- renderUI({
    selectizeInput(
      "file",
      "File:",
      choices = unique(counts[counts$workflow_name == input$workflow & counts$workflow_version == input$workflow_version,]$file),
      selected = unique(counts[counts$workflow_name == input$workflow & counts$workflow_version == input$workflow_version,]$file)[1])
  })
  
  output$barPlot <- renderPlot({
    tryCatch({
      counts_subset <- counts[counts$workflow_name == input$workflow & counts$workflow_version == input$workflow_version,]
      if (nrow(counts_subset) == 0) {
        return()
      }
      max_bins <- max(counts_subset[, bins])
      counts_subset <- counts_subset[counts_subset$file == input$file, bins]
      barplot(as.numeric(counts_subset), names.arg = n_bins, main = "Classification counts", las=2, ylim=c(0, max_bins))
    })
  })

  output$spectrogram <- renderPlot({
    tryCatch({
      file_path <- paste0("data/audio/", input$file)
      if (file_path == "data/audio/") {
        return()
      }
      w <- readMP3(paste0(file_path))
      spectro(w, osc=TRUE)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
