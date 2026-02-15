library(shiny)

ui <- fluidPage(
  h3("App validação release - upload"),
  fileInput("foto", "Enviar imagem", accept = c("image/png", "image/jpeg")),
  actionButton("salvar", "Salvar"),
  tags$hr(),
  verbatimTextOutput("out"),
  tags$hr(),
  uiOutput("galeria")
)

server <- function(input, output, session) {

  uploads_dir <- file.path("www", "uploads")
  dir.create(uploads_dir, recursive = TRUE, showWarnings = FALSE)

  saved <- reactiveVal(character(0))

  observeEvent(input$salvar, {
    req(input$foto)

    ext <- tools::file_ext(input$foto$name)
    if (tolower(ext) %in% c("jpeg")) ext <- "jpg"

    fname <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", basename(input$foto$name))
    dest  <- file.path(uploads_dir, fname)

    file.copy(input$foto$datapath, dest, overwrite = FALSE)

    saved(c(saved(), fname))
  })

  output$out <- renderText({
    if (length(saved()) == 0) "Nenhum arquivo salvo ainda."
    else paste("Salvos:\n", paste(saved(), collapse = "\n"))
  })

  output$galeria <- renderUI({
    if (length(saved()) == 0) return(NULL)
    tagList(lapply(rev(saved()), function(f) {
      tags$div(style="margin-bottom:10px;",
               tags$img(src = file.path("uploads", f), height = 120))
    }))
  })
}

shinyApp(ui, server)
