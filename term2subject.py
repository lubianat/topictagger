import pandas as pd
from pandas import json_normalize 
import requests
from wikidata2df import wikidata2df



def main():
    term = input("Enter a name to look for titles: " )
    term_id = input("Enter Wikidata QID for: " )

    def run_all(term, term_id):
        url = prepare_url_for_search(term)
        ids =  pull_related_ids(url)
        articles_dataframe = filter_for_instances_of_article(ids)
        print_qs_to_file(articles_dataframe, term, term_id)

    def prepare_url_for_search(term):
        term_for_url = term.replace(" ","%20")
        term_for_url = "%22" +term_for_url + "%22"

        url = f"https://www.wikidata.org/w/api.php?action=query&list=search&srsearch={term_for_url}&srlimit=500&srprop=size&formatversion=2&format=json"
        return(url)

    def pull_related_ids(url):
        r = requests.get(url)
        df = json_normalize(r.json()["query"]["search"])

        ids = ["wd:"+a for a in df["title"]]
        return(ids)
        
    def filter_for_instances_of_article(ids):

        items = "{"
        for i in ids:
            items = items + " " + i
        items = items + " }"


        articles = """
        SELECT ?item ?itemLabel
        WHERE
        {
        VALUES ?item """ + items + """.
        ?item wdt:P31 wd:Q13442814.
        SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
        }
        """

        articles_dataframe = wikidata2df(articles)
        return(articles_dataframe)


    def print_qs_to_file(articles_dataframe, term, term_id):
        with open(term + ".qs", "w+") as f:
            for i, row in articles_dataframe.iterrows():
                s = row["item"]
                p = "|P921|"
                o = term_id
                r = "|S887|"
                ro = "Q69652283"
                f.write(s + p + o + r + ro + "\n")
    run_all(term, term_id)



if __name__ == "__main__":
    main()