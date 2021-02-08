library(shiny)
library(rclipboard)
source("utils.R")

ui <- fluidPage(

  rclipboardSetup(),
  windowTitle = "TopicTagger",
  title = tags$head(tags$link(
    rel = "icon",
    href = "https://raw.githubusercontent.com/lubianat/titlematch/master/assets/favicon-32x32.png",
    type = "image/x-icon"
  ), ),

  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "term",
        label = "Term",
        value = "",
        width = NULL,
        placeholder = NULL
      ),
      dataTableOutput("candidate_qids"),
      uiOutput("search"),
      textInput(
        inputId = "term_qid",
        label = "Term QID",
        value = "",
        width = NULL,
        placeholder = NULL
      ),
      numericInput(
        inputId = "n_articles",
        label = "Number of articles",
        value = 200,
        min = 1,
        max = 200,
        step = 1
      )
    ),
    mainPanel(

      # UI ouputs for the copy-to-clipboard buttons
      uiOutput("clip"),
      tags$a(href = "https://quickstatements.toolforge.org/#/batch", "Go to Quickstatements!"),
      textOutput("summary"),
      verbatimTextOutput("qs")
    ),
  )
)

server <- function(input, output) {
  output$search <- renderUI({
    term <- input$term
    url <- a("Search on Wikidata",
      href = paste0("https://www.wikidata.org/w/index.php?search=", term)
    )
    print("Here")
    tagList("URL link:", url)
  })
  output$summary <- renderText({
    term <- input$term
    n <- input$n_articles
    paste("Obtaining up to", n, "articles about", term)
  })


  output$candidate_qids <- renderDataTable(
    {
      term <- input$term
      term_qid <- input$term_qid
      n_articles <- input$n_articles
      url <- prepare_url_for_search(term, n_results = n_articles)
      ids <- pull_related_ids(url)
      articles <- filter_for_instances_of_article(ids)
      descriptions <- get_top_descriptions(
        ids = ids,
        article_ids = articles
      )
      descriptions$itemDescription <- NULL
      return(descriptions)
    },
    escape = FALSE,
    options = list(dom = "t")
  )


  output$qs <- renderText({
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    url <- prepare_url_for_search(term, n_results = n_articles)
    ids <- pull_related_ids(url)
    articles <- filter_for_instances_of_article(ids)
    result <- prepare_qs_to_render(
      article_qids = articles,
      term = term,
      term_id = term_qid
    )

    if (term_qid == "") {
      return("Waiting for a term QID")
    } else {
      return(result)
    }
  })


  output$clip <- renderUI({
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    url <- prepare_url_for_search(term, n_results = n_articles)
    ids <- pull_related_ids(url)
    articles <- filter_for_instances_of_article(ids)
    result <- prepare_qs_to_render(
      article_qids = articles,
      term = term,
      term_id = term_qid
    )

    result <- paste(result, collapse = "")
    rclipButton("clipbtn", "Copy", result, icon("clipboard"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
