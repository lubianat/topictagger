library(stringr)
library(httr)
library(WikidataQueryServiceR)

#' Prepare URL for Wikidata search
#'
#' @param term A string to search on Wikidata
#' @param n_results The number of search results. Defaults to 200. 
prepare_url_for_search <- function(term, n_results=200)
{
  term_for_url = str_replace(term," ","%20")
  term_for_url = paste0("%22", term_for_url, "%22")
  
  url = paste0("https://www.wikidata.org/w/api.php?action=query&list=search&srsearch=",
               term_for_url,
               "&srlimit=", n_results, "&srprop=size&formatversion=2&format=json")
  return(url)
}


#' Pull IDs returned in a Wikidata Search
#'
#' @param url A query URL that returns a JSON object
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

#' Select which QIDs on a list are instances of article
#'
#' @param ids A list of Wikidata ids
filter_for_instances_of_article <- function(ids)
{    
  items = "{"
  for(i in ids)
  {
    items = paste0(items, " wd:", i)
  }
  items = paste0(items, " }")
  
  
  articles =paste0(' SELECT ?item ?itemLabel
            WHERE
            {
            VALUES ?item ', items, '.
            ?item wdt:P31 wd:Q13442814.
            SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
            }
            ')
  
  articles_dataframe = query_wikidata(articles)
  
  result = c()
  for (u in articles_dataframe[["item"]])
  {
    qid =str_replace(u, "http://www.wikidata.org/entity/", "")
    result = c(result, qid)
  }
  return(result)
}

#' Prepare URL for Wikidata search
#'
#' @param term A string that was searched on Wikidata
#' @param term_id The QID relative to the term parameter
#' @param article_qids A vector of QIDs of articles that have the term as main subject
print_qs_to_prompt <- function(article_qids, term, term_id)
{
  for (article in article_qids)
  {
    s = article
    p = "|P921|"
    o = term_id
    r = "|S887|"
    ro = "Q69652283"
    print(paste0(s, p, o, r, ro))
  }
}


#' Prepare URL for Wikidata search
#'
#' @param term A string that was searched on Wikidata
#' @param term_id The QID relative to the term parameter
#' @param article_qids A vector of QIDs of articles that have the term as main subject
prepare_qs_to_render <- function(article_qids, term, term_id)
{
  result=c("\n")
  for (article in article_qids)
  {
    s = article
    p = "|P921|"
    o = term_id
    r = "|S887|"
    ro = "Q69652283"
    statement = paste0(s, p, o, r, ro)
    result=c(result, paste0(statement,"\n" ))
  }
  return(result)
}





