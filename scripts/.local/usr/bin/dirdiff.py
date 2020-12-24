#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys, logging
from pathlib import Path

logging.basicConfig(level=logging.DEBUG)
logging.disable(logging.CRITICAL)

def filesInDir(dir):
    '''
    Returns a list of files.
    '''
    logging.debug(f'rawList: {[f.name for f in Path.iterdir(dir)]}')
    path = Path(dir)
    fileList = [f.name for f in path.iterdir() if f.is_file()]
    logging.debug(f'Files in {dir}:\n{fileList}')
    return fileList

def canonicalize_dir(dir):
    '''
    Returns a full path for the given directory.
    Type: string
    '''
    logging.debug(f'Path of {dir}:')
    cwd = Path.cwd()
    if dir == '.':
        dir = cwd
        logging.debug(f'{dir}')
        return dir
    else:
        dir = Path.joinpath(cwd, dir)
        return dir

def compare(list1, list2):
    '''
    Returns a sorted list of items in list1 not in list2.
    '''
    logging.debug(f'List 1: {list1}')
    logging.debug(f'List 2: {list2}')
    non_match = []
    for file in list1:
        if file not in list2:
            non_match.append(file)
    logging.debug(f'Non-matches: {non_match}')
    return sorted(non_match)


def main():
    if len(sys.argv) < 3:
        print('''
dirdiff.py compares two folders and outputs a list of files not present
in folder1 but not in folder2.

   dirdiff.py [folder1] [folder2] {optional}

The third argument is used when folder2 contains copies of folder1's
files with a suffix. The argument should be the suffix.

Example:
   dirdiff.py documents backup .bak
        ''')
        quit()

    logging.debug(f'Variable 1: {sys.argv[1]}')
    logging.debug(f'Variable 2: {sys.argv[2]}')
    # folder 1
    first_dir_files = filesInDir(canonicalize_dir(sys.argv[1]))
    # folder 2
    second_dir_files = filesInDir(canonicalize_dir(sys.argv[2]))

    if len(sys.argv) == 4:
        suffix = sys.argv[3]
        second_dir_files = [ f.replace(suffix, '') for f in second_dir_files ]
    # do comparison
    non_match = compare(first_dir_files, second_dir_files)
    if non_match:
        print('\n'.join(non_match))


if __name__ == '__main__':
    main()
