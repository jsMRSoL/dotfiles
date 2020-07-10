#!/usr/bin/env python3

import requests
import bs4
import sys

if len(sys.argv) < 1:
    print("No Greek word supplied. Exiting...")
    quit()

page = requests.get("http://www.perseus.tufts.edu/hopper/morph?l=%s&la=greek"
                    % sys.argv[1])

try:
    page.raise_for_status()
except Exception as e:
    print("There was a problem: %s" % (e))

pageSoup = bs4.BeautifulSoup(page.content, "lxml")

mydivs = pageSoup.find_all('div', {"class": "lemma"})
# print("No. of divs found: ", len(mydivs))

# Prefix ids with this for next search
# http://www.perseus.tufts.edu/hopper/text?doc=

if len(mydivs) == 0:
    print(sys.argv[1], ": [definition unavailable]")
else:
    for mydiv in mydivs:
        print(mydiv.h4.getText(strip=True), ":",
              mydiv.span.getText(strip=True))
        print("Link: ", mydiv.a.get('id')[:-5])
