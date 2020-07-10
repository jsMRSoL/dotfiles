#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import requests, bs4, sys

if len(sys.argv) < 1:
    quit()

page = requests.get("http://www.wordreference.com/iten/%s" % sys.argv[1])

try:
    page.raise_for_status()
except Exception as e:
    print('There was a problem: %s' % (e))

pageSoup = bs4.BeautifulSoup(page.text, "html.parser")

elems = pageSoup.select('#pronWR')

print(elems[0].getText())

