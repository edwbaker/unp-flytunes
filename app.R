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
          selectInput(
            "workflow_version",
            "Workflow version:",
            choices = unique(counts$workflow_version),
            selected = unique(counts$workflow_version)[1]),
          selectizeInput(
            "file",
            "Select audio file:",
            choices = unique(counts$file),
            selected = unique(counts$file)[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("spectrogram"),
           plotOutput("barPlot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$barPlot <- renderPlot({
        counts_subset <- counts[counts$workflow_name == input$workflow & counts$workflow_version == input$workflow_version,]
        max_bins <- max(counts_subset[, bins])
        counts_subset <- counts_subset[counts_subset$file == input$file, bins]
        barplot(as.numeric(counts_subset), names.arg = n_bins, main = "Classification counts", las=2, ylim=c(0, max_bins))
    })

    output$spectrogram <- renderPlot({
        w <- readMP3(paste0("data/audio/", input$file))
        spectro(w, osc=TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
