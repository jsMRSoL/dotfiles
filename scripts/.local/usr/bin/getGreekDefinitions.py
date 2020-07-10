#!/usr/bin/env python3

import requests
import bs4
import sys

if len(sys.argv) < 1:
    print("No Greek word supplied. Exiting...")
    quit()

page = requests.get("http://www.perseus.tufts.edu/hopper/text?doc=%s"
                    % sys.argv[1])

try:
    page.raise_for_status()
except Exception as e:
    print("There was a problem: %s" % (e))

pageSoup = bs4.BeautifulSoup(page.content, "lxml")

lexes = pageSoup.find_all('div', {"class": "lex_sense"})

for lex in lexes:
    try:
        print(lex.i.getText(strip=True))
    except Exception as e:
        pass
