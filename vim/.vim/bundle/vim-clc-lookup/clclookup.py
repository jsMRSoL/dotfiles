#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re
import subprocess

HERMES = '/home/simon/.vim/bundle/vim-clc-lookup/hermes.sh'
DATABASE = '/home/simon/.vim/bundle/vim-clc-lookup/clc4-vocab'


def make_list(input_string):
    """Assemble list of headwords for lookup """
    pattern = re.compile(r'\W+')
    input_list = pattern.split(input_string)
    word_list = []
    for word in input_list:
        if len(to_headword(word)) > 0:
            word_list.append(to_headword(word))
        elif len(word) > 0:
            word_list.append(word)

    # flat_list = [word for list in word_list for word in list]
    flat_list = []
    for elmt in word_list:
        if type(elmt) == list:
            flat_list.extend(elmt)
        else:
            flat_list.append(elmt)

    return flat_list


def to_headword(inflected):
    """Runs inflected form through hermes.sh
    :returns: headword as string
    """
    if inflected in ["itaque", "itemque"]:
        return inflected

    result = subprocess.run([HERMES, inflected], stdout=subprocess.PIPE)
    result = result.stdout.decode('utf-8')
    return result.split()


def to_definition(headword):
    """Looks up headword in custom list
    :returns: string
    """
    entry = subprocess.run(['grep', '-m2', '^' + headword + '[,: ]', DATABASE],
                           stdout=subprocess.PIPE)
    entry = entry.stdout.decode('utf-8')
    return entry.strip()


if __name__ == "__main__":
    if sys.argv == 1:
        print('No argument was given')
    else:
        lookuplist = make_list(sys.argv[1])
        for item in lookuplist:
            answer = to_definition(item)
            """ because names will not be found
            and therefore return a blank line, check len
            """
            if len(answer):
                print('- ', answer.replace('\n', '\n-  '))
