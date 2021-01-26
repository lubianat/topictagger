#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(httr)

prepare_url_for_search <- function(term)
{
    term_for_url = str_replace(term," ","%20")
    term_for_url = paste0("%22", term_for_url, "%22")
    
    url = paste0("https://www.wikidata.org/w/api.php?action=query&list=search&srsearch=",
                 term_for_url,
                 "&srlimit=500&srprop=size&formatversion=2&format=json")
    return(url)
}

pull_related_ids <- function(url)
{
    resp = GET(url)
    obj = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
    search = obj[["query"]][["search"]]
    ids = c()
    for (item in search)
    {
        ids <- c(ids, item$title)
    }
    return(ids)
}
url = prepare_url_for_search("osteocyte")
pull_related_ids(url)
    


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
