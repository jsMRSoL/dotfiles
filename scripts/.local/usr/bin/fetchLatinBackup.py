#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import requests
import re
import sys
from bs4 import BeautifulSoup as bs


def make_soup(latin):
    BASE_URL = 'http://www.perseus.tufts.edu/hopper/text?doc=Perseus%3Atext%3A1999.04.0060%3Aentry%3D'
    page = requests.get(BASE_URL + latin)
    try:
        page.raise_for_status()
    except Exception as e:
        print('There was a problem: %s' % (e))
        quit()
    pageSoup = bs(page.text, 'html.parser')
    if len(pageSoup.findAll(text=re.compile("We're sorry"))) == 0:
        return pageSoup
    else:
        return -1

def fetch_entry(soup):
    elems = soup.select('#text_main .text')
    if len(elems) == 0:
        print('Unable to extract text from page.')
    else:
        # parts = elems[0].getText().split(" ", 4)
        # print(parts[0].strip() + parts[1].strip() + " " + parts[2].strip())
        output = elems[0].getText()
        regex = re.compile(r"(.*?)[v|\n|\.|I|\(].*")
        output = re.sub(regex, r'\1', output)
        output = re.sub(' ,', ',', output)
        output = re.sub(',$', '', output.strip())
        return output

def main(latin):
    soup = make_soup(latin)
    if soup == -1:
        print('No definition was found for ' + latin)
    else:
        return fetch_entry(soup)


if __name__ == '__main__':
    if len(sys.argv) == 1:
        print("No argument given.")
    else:
        for item in sys.argv[1:]:
            print(main(item))
