library(stringr)
library(httr)
library(WikidataQueryServiceR)

get_article_qids_via_maintenance_query <- function(topic_term, topic_qid, n_articles=300){
topic_term <- tolower(topic_term)
property = "P921"
maintenance_query <- paste0('
SELECT 
  DISTINCT 
  # Remove ?item and ?title to generate table
  # in Quickstatements V1 syntax
  # ?item ?title
  (REPLACE(STR(?item), ".*Q", "Q") AS ?qid) 
  ("', property, '" AS ?property)
   # ============= Want to change the topic? Change the next line! =============
  ("', topic_qid, '" AS ?topic)
  ("S887" AS ?heuristic)
  ("Q69652283" AS ?deduced)

WHERE {
  hint:Query hint:optimizer "None".
  
{  SERVICE wikibase:mwapi {
    bd:serviceParam wikibase:api "Search";
                    wikibase:endpoint "www.wikidata.org";
                    
   # ============= Want to change the topic? Change the next line! =============
                    mwapi:srsearch "\\"', topic_term,'\\" haswbstatement:P31=Q13442814 -haswbstatement:P921=', topic_qid,'".
      ?page_title wikibase:apiOutput mwapi:title.
  }
 }
  BIND(IRI(CONCAT(STR(wd:), ?page_title)) AS ?item)
  
  ?item wdt:P1476 ?title.
  # ============= Want to change the topic? Change the next line! =============
  FILTER CONTAINS(LCASE(?title), "', topic_term,'"). # also check for variants, e.g. without the dash and/ or with "2019"
}
LIMIT ', as.character(n_articles), '

')

maintenance_dataframe <- query_wikidata(maintenance_query)

return(maintenance_dataframe$qid)

}



#' Prepare URL for Wikidata search
#'
#' @param term A string to search on Wikidata
#' @param n_results The number of search results. Defaults to 200.
prepare_url_for_search <- function(term, n_results = 200) {
  term_for_url <- str_replace(term, " ", "_")
  term_for_url <- paste0("%22", term_for_url, "%22")

  url <- paste0(
    "https://www.wikidata.org/w/api.php?action=query&list=search&srsearch=",
    term_for_url,
    "&srlimit=", n_results, "&srprop=size&formatversion=2&format=json"
  )
  return(url)
}


#' Pull IDs returned in a Wikidata Search
#'
#' @param url A query URL that returns a JSON object
pull_related_ids <- function(url) {
  resp <- GET(url)
  obj <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  search <- obj[["query"]][["search"]]
  ids <- c()
  for (item in search)
  {
    ids <- c(ids, item$title)
  }
  return(ids)
}

#' make stub
#'
#' @param string A string
#' @param len The length of the stub to return
#' @param ending The end of the stub. Defaults to "..."
make_stub <- function(string, len, ending = "...") {
  if (nchar(string) < 50) {
    return(string)
  } else {
    paste0(substr(string, 1, len), ending)
  }
}

#' Get labels and descriptions of top 5
#'
#' @param ids A list of Wikidata ids
get_top_descriptions <- function(ids, article_ids) {
  counter <- 0
  items <- "{"
  for (i in ids[!(ids %in% article_ids)])
  {
    items <- paste0(items, " wd:", i)
    counter <- counter + 1

    if (counter == 5) {
      break
    }
  }
  items <- paste0(items, " }")


  descriptions_query <- paste0(" SELECT ?item ?itemLabel ?itemDescription ?typeLabel
            WHERE
            {
            VALUES ?item ", items, '.
            ?item wdt:P31 ?type .
            SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
            }
            ')

  descriptions_dataframe <- query_wikidata(descriptions_query)

  links <- c()
  for (u in descriptions_dataframe[["item"]])
  {
    qid <- str_replace(u, "http://www.wikidata.org/entity/", "")
    link <- paste0('<a target="_blank" href=', u, ">", qid, "</a>")
    links <- c(links, link)
  }
  descriptions_dataframe[["item"]] <- links

  descriptions <- c()
  for (u in descriptions_dataframe[["itemDescription"]])
  {
    description <- make_stub(u, len = 50)
    descriptions <- c(descriptions, description)
  }
  descriptions_dataframe[["itemDescription"]] <- descriptions
  return(descriptions_dataframe)
}




#' Select which QIDs on a list are instances of article
#'
#' @param ids A list of Wikidata ids
filter_for_instances_of_article <- function(ids) {
  items <- "{"
  for (i in ids)
  {
    items <- paste0(items, " wd:", i)
  }
  items <- paste0(items, " }")


  articles <- paste0(" SELECT ?item ?itemLabel
            WHERE
            {
            VALUES ?item ", items, '.
            ?item wdt:P31 wd:Q13442814.
            SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
            }
            ')

  articles_dataframe <- query_wikidata(articles)

  result <- c()
  for (u in articles_dataframe[["item"]])
  {
    qid <- str_replace(u, "http://www.wikidata.org/entity/", "")
    result <- c(result, qid)
  }
  return(result)
}

#' Prepare URL for Wikidata search
#'
#' @param term A string that was searched on Wikidata
#' @param term_id The QID relative to the term parameter
#' @param article_qids A vector of QIDs of articles that have the term as main subject
print_qs_to_prompt <- function(article_qids, term, term_id) {
  for (article in article_qids)
  {
    s <- article
    p <- "|P921|"
    o <- term_id
    r <- "|S887|"
    ro <- "Q69652283"
    print(paste0(s, p, o, r, ro))
  }
}


#' Prepare URL for Wikidata search
#'
#' @param term A string that was searched on Wikidata
#' @param term_id The QID relative to the term parameter
#' @param article_qids A vector of QIDs of articles that have the term as main subject
prepare_qs_to_render <- function(article_qids, term, term_id) {
  result <- c("\n")
  for (article in article_qids)
  {
    s <- article
    p <- "|P921|"
    o <- term_id
    r <- "|S887|"
    ro <- "Q69652283"
    statement <- paste0(s, p, o, r, ro)
    result <- c(result, paste0(statement, "\n"))
  }
  return(result)
}
