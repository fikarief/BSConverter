library(shiny)
library(Biostrings)

bisulfite_conversion <- function(seq_file) {
  sequences <- readLines(seq_file, warn = FALSE)

  # Concatenate into single string
  concatenated_sequence <- paste(sequences, collapse = "")

  # Forward conversion
  converted_sequence <- gsub("C(?![G])", "T", concatenated_sequence, perl = TRUE)
  converted_trimmed <- gsub("\\s+", "", converted_sequence)

  # Reverse orientation
  dna_sequence <- DNAString(concatenated_sequence)
  reverse_sequence <- reverseComplement(dna_sequence)
  reverse_sequence <- toString(reverse_sequence)
  reverse_converted <- gsub("C(?![G])", "T", reverse_sequence, perl = TRUE)
  reverse_converted <- reverseComplement(DNAString(reverse_converted))
  reverse_converted_trimmed <- gsub("\\s+", "", reverse_converted)

  return(list(
    forward = converted_trimmed,
    reverse = reverse_converted_trimmed
  ))
}

ui <- fluidPage(
  titlePanel("Bisulfite Conversion Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("seqfile", "Upload DNA sequence (.txt)",
        accept = c(".txt")
      ),
      downloadButton("download_forward", "Download Forward Conversion"),
      downloadButton("download_reverse", "Download Reverse Conversion")
    ),
    mainPanel(
      h4("Preview of Converted Sequences (First 500 bp)"),
      tabsetPanel(
        tabPanel("Forward", verbatimTextOutput("forward_preview")),
        tabPanel("Reverse", verbatimTextOutput("reverse_preview"))
      )
    )
  )
)

server <- function(input, output, session) {
  converted <- reactive({
    req(input$seqfile)
    bisulfite_conversion(input$seqfile$datapath)
  })

  # Preview first 500 characters
  output$forward_preview <- renderText({
    seq <- converted()$forward
    substr(seq, 1, 500)
  })

  output$reverse_preview <- renderText({
    seq <- converted()$reverse
    substr(seq, 1, 500)
  })

  # Download handlers
  output$download_forward <- downloadHandler(
    filename = function() {
      "output_forward.txt"
    },
    content = function(file) {
      writeLines(converted()$forward, file)
    }
  )

  output$download_reverse <- downloadHandler(
    filename = function() {
      "output_reverse.txt"
    },
    content = function(file) {
      writeLines(converted()$reverse, file)
    }
  )
}

shinyApp(ui, server)