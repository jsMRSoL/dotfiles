#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
A script to create sub-directories in the current directory,
naming them according to the common basenames of the files
in the current directory, and move the files into the appropriate
directory.
E.g. st1.jpg, st2.jpg
--> ST/
    - st1.jpg
    - st2.jpg
'''
# import sys
import logging
from pathlib import Path

logging.basicConfig(level=logging.DEBUG)
# logging.disable(logging.CRITICAL)


def files_and_stems(folder):
    '''
    Returns a set of common basenames of files in the current directory.
    '''
    path = Path(folder)
    files = []
    stems = set()
    for f in path.iterdir():
        if f.is_file():
            files.append(f.name)
            stems.add(stemify(f.name))
    return sorted(files), sorted(stems)


def stemify(filename: str) -> str:
    new = ""
    for c in filename:
        if c.isalpha():
            new += c
        else:
            break
    return new or filename


def create_directories(dir_list):
    for dir in dir_list:
        new = Path(dir)
        new.mkdir()


def main():
    files, basenames = files_and_stems('.')
    create_directories(basenames)
    for filename in files:
        for dir_name in basenames:
            if dir_name in filename:
                source_file = Path(filename)
                dest_dir = Path(dir_name)
                dest_file = dest_dir / source_file.name
                source_file.rename(dest_file)
                break


if __name__ == "__main__":
    main()
