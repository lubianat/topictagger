#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("utils.R")
CLIPR_ALLOW=TRUE

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    windowTitle = "TopicTagger",
    title = tags$head(tags$link(rel="icon", 
                                href="https://raw.githubusercontent.com/lubianat/titlematch/master/favicon-32x32.png", 
                                type="image/x-icon")
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId="term",
                      label="Term",
                      value = "",
                      width = NULL,
                      placeholder = NULL),
            textInput(inputId="term_qid",
                      label="Term QID",
                      value = "",
                      width = NULL,
                      placeholder = NULL),
            numericInput(inputId="n_articles", 
                         label="Number of articles", 
                         value=200, 
                         min = 1, 
                         max = 200, 
                         step = 1)
            
            
        ),
        mainPanel(
            actionButton("copyButton", "Copy!"),
            textOutput("summary"),
            verbatimTextOutput("qs") 
            ),


    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$summary <- renderText({
        term = input$term
        n = input$n_articles
        paste("Obtaining up to", n, "articles about", term)
    })
    
    output$qs <- renderText({
        term = input$term
        term_qid = input$term_qid
        n_articles = input$n_articles
        url = prepare_url_for_search(term, n_results = n_articles)
        ids = pull_related_ids(url)
        articles = filter_for_instances_of_article(ids)
        result = prepare_qs_to_render(article_qids=articles,
                           term=term,
                           term_id=term_qid)
        print(result)
        return(result)
    })
    
    
    observeEvent(input$copyButton, {
        term = input$term
        term_qid = input$term_qid
        n_articles = input$n_articles
        url = prepare_url_for_search(term, n_results = n_articles)
        ids = pull_related_ids(url)
        articles = filter_for_instances_of_article(ids)
        result = prepare_qs_to_render(article_qids=articles,
                                      term=term,
                                      term_id=term_qid)
        clipr::write_clip(result, allow_non_interactive = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
