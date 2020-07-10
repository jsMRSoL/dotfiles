#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests, re, sys
from bs4 import BeautifulSoup as BS

def get_list_page(latin):
    BASE_URL = 'http://nodictionaries.com/novifex?text='
    page = requests.get(BASE_URL + latin)
    soup = BS(page.text, 'html.parser')
    return soup

def parse_list_page(soup):
    tables = soup.findAll('table', {'class':'lh0'})
    latin_entries = [ table.find('i')
            for table in tables ]
    latin_entries = [ entries.getText() for entries in latin_entries
            if entries != None ]
    eng_entries = [ table.find('span', {'class':'english'})
            for table in tables ]
    eng_entries = [ entries.getText() for entries in eng_entries
            if entries != None ]
    return zip(latin_entries, eng_entries)

def main(latin_string):
    latin_string = re.sub(' ', '+', latin_string)
    soup = get_list_page(latin_string)
    if soup == -1:
        return 'nodictionaries.com did not respond.'

    output = ""
    for latin, english in parse_list_page(soup):
        output += "- {} : {}\n".format(latin, english)
    return output

if __name__ == "__main__":
    if len(sys.argv) == 1:
        print('No argument given.')
    else:
        latin_string = '+'.join(sys.argv[1:])
        print(main(latin_string))
