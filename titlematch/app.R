library(shiny)
library(rclipboard)
source("utils.R")

ui <- fluidPage(
  rclipboardSetup(),
  windowTitle = "TopicTagger",
  title = tags$head(
    tags$link(rel = "icon",
              href = "https://raw.githubusercontent.com/lubianat/titlematch/master/assets/favicon-32x32.png",
              type = "image/x-icon"),
    
  ),
  titlePanel("TopicTagger"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "term",
        label = "Term for search",
        value = "lyme disease",
        width = NULL,
        placeholder = NULL
      ),
      p("Term is quoted before the search. System is capitalization-independent"),
      submitButton(text = "Submit term", icon = NULL, width = NULL),
      br(),
      p("Candidates for QIDS:"),
      dataTableOutput("candidate_qids"),
      p("Your QID is not there? Then:"),
      uiOutput("search"),
      textInput(
        inputId = "term_qid",
        label = "Term QID",
        value = "Q201989",
        width = NULL,
        placeholder = NULL
      ),
      submitButton(text = "Submit QID", icon = NULL, width = NULL),
      numericInput(
        inputId = "n_articles",
        label = "Number of articles to retrieve",
        value = 10,
        min = 0,
        max = 1000,
        step = 1,
        width = NULL
      ),
      tags$a(target="_blank",
             href = "https://github.com/lubianat/topictagger",
             "GitHub Repository")
    ),
    
    mainPanel(
      # UI ouputs for the copy-to-clipboard buttons
      h4("Tag articles on Wikidata based on their titles"),
      uiOutput("clip"),
      tags$a( target="_blank",
              href = "https://quickstatements.toolforge.org/#/batch",
             "Go to Quickstatements!"),
      textOutput("summary"),
      
      verbatimTextOutput("qs")
    ),
  )
)

server <- function(input, output) {
  output$search <- renderUI({
    term <- input$term
    url <- a(
      "Search on Wikidata",
      target="_blank",
      href = paste0("https://www.wikidata.org/w/index.php?search=", term)
    )
    print("Here")
    tagList(url)
  })
  output$summary <- renderText({
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    paste(
      "Obtaining up to",
      as.character(n_articles),
      "articles about",
      term,
      "that do not have",
      term_qid,
      "as main subject"
    )
  })
  
  
  output$candidate_qids <- renderDataTable({
    term <- input$term
    term_qid <- input$term_qid
    url <- prepare_url_for_search(term)
    ids <- pull_related_ids(url)
    articles <- filter_for_instances_of_article(ids)
    descriptions <- get_top_descriptions(ids = ids,
                                         article_ids = articles)
    descriptions$itemDescription <- NULL
    return(head(descriptions))
  },
  escape = FALSE,
  options = list(dom = "t"))
  
  
  output$qs <- renderText({
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    if (term_qid == "") {
      return("Waiting for a term QID")
    } else {
      article_qids <-
        get_article_qids_via_maintenance_query(term, term_qid, n_articles)
      result <- prepare_qs_to_render(article_qids = article_qids,
                                     term = term,
                                     term_id = term_qid)
      return(result)
    }
  })
  
  
  output$clip <- renderUI({
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    if (term_qid == "") {
      result <- "Waiting for a term QID"
    } else {
      article_qids <-
        get_article_qids_via_maintenance_query(term, term_qid, n_articles)
      result <- prepare_qs_to_render(article_qids = article_qids,
                                     term = term,
                                     term_id = term_qid)
    }
    result <- paste(result, collapse = "")
    rclipButton("clipbtn", "Copy Quickstatements commands", result, icon("clipboard"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
