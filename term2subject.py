import pandas as pd
from pandas import json_normalize 
from gooey import Gooey
import requests
from wikidata2df import wikidata2df
import argparse


# Simple GUI based on Gooey
#
# https://github.com/chriskiehl/Gooey 

@Gooey
def main():
    parser = argparse.ArgumentParser(description='Add main subject statements based on Wikidata searches.')
    parser.add_argument('-a','--term', required=False, nargs='+') 
    parser.add_argument('-m','--term_id', required=False, nargs='+')

    args = vars(parser.parse_args())


    def run_all_to_file(term, term_id):
        url = prepare_url_for_search(term)
        ids =  pull_related_ids(url)
        articles_dataframe = filter_for_instances_of_article(ids)
        print_qs_to_file(articles_dataframe, term, term_id)

    def run_all_to_prompt(term, term_id):
        url = prepare_url_for_search(term)
        ids =  pull_related_ids(url)
        articles_dataframe = filter_for_instances_of_article(ids)
        print_qs_to_prompt(articles_dataframe, term, term_id)

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

    def print_qs_to_prompt(articles_dataframe, term, term_id):
        with open(term + ".qs", "w+") as f:
            for i, row in articles_dataframe.iterrows():
                s = row["item"]
                p = "|P921|"
                o = term_id
                r = "|S887|"
                ro = "Q69652283"
                print(s + p + o + r + ro + "\n")


    if args["term"] and args["term_id"]:
        run_all_to_prompt(args["term"][0], args["term_id"][0])


if __name__ == "__main__":
    main()