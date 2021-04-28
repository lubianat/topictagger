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
      p(
        "Term is quoted before the search. System is capitalization-independent"
      ),
      p( "Also search for the term with an 's' in the end?"),
      checkboxInput(inputId="plural", label="get plural", value = FALSE, width = NULL),
      p("Don't know the QID?"),
      uiOutput("search"),
      br(),
      textInput(
        inputId = "term_qid",
        label = "Term QID",
        value = "Q201989",
        width = NULL,
        placeholder = NULL
      ),
      actionButton(inputId = "submit", label = "Submit QID", icon = NULL, width = NULL),
      sliderInput(
        inputId = "n_articles",
        label = "Number of articles to retrieve",
        value = 10,
        min = 0,
        max = 5000,
        step = 20,
        width = NULL
      ),
      br(),
      tags$a(target = "_blank",
             href = "https://github.com/lubianat/topictagger",
             "GitHub Repository"),
      uiOutput("query_url")
    ),
    
    mainPanel(
      # UI ouputs for the copy-to-clipboard buttons
      h4("Tag articles on Wikidata based on their titles"),
      uiOutput("clip"),
      tags$button(tags$a(target = "_blank",
             href = "https://quickstatements.toolforge.org/#/batch",
             "Go to Quickstatements!")),
      textOutput("summary"),
      
      verbatimTextOutput("qs")
    ),
  )
)

server <- function(input, output, session) {
  

  
  output$search <- renderUI({
    term <- input$term
    url <- a(
      "Search term on Wikidata",
      target="_blank",
      href = paste0("https://www.wikidata.org/w/index.php?search=", term)
    )
    tagList(url)
  })

    # Prepare summary for user ------------
  
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
  
  output$query_url <- renderUI({
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    print("Here")
    if (term_qid != "") {
      link = get_maintenance_query_url(term, term_qid, n_articles)
      query_url <- a("See query on the Wikidata Query Service",
                     target = "_blank",
                     href = link)
      print(query_url)
    } else {
      query_url = ""
    }
    
    return(tagList(query_url))
  })
  
  
  # Prepare the Quickstatements to return ----------
  
  render_text_reactive <- eventReactive(input$submit, {
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    plural <- input$plural
    if (term_qid == "") {
      return("Waiting for a term QID")
    } else {
      article_qids <-
        get_article_qids_via_maintenance_query(term, term_qid, n_articles)
      
      if (plural){
      article_qids_plural <-
        get_article_qids_via_maintenance_query(paste0(term,"s"), term_qid, n_articles)
      
      article_qids <- c(article_qids, article_qids_plural)
      }
      
      result <- prepare_qs_to_render(article_qids = article_qids,
                                     term = term,
                                     term_id = term_qid)
      return(result)
    }
  })
  
  output$qs <- renderText({
    render_text_reactive()
  })
  
  
  output$clip <- renderUI({
    term <- input$term
    term_qid <- input$term_qid
    n_articles <- input$n_articles
    plural <- input$plural
    if (term_qid == "") {
      result <- "Waiting for a term QID"
    } else {
      article_qids <-
        get_article_qids_via_maintenance_query(term, term_qid, n_articles)
      print(article_qids)
      if (plural){
        article_qids_plural <-
          get_article_qids_via_maintenance_query(paste0(term,"s"), term_qid, n_articles)
        
        article_qids <- c(article_qids, article_qids_plural)
      }
      print(article_qids)
      
      result <- prepare_qs_to_render(article_qids = article_qids,
                                     term = term,
                                     term_id = term_qid)
    }
    result <- paste(result, collapse = "")
    rclipButton("clipbtn",
                "Copy Quickstatements commands",
                result,
                icon("clipboard"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
