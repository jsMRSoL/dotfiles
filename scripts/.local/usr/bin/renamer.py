#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
A script to rename files with a stem and incrementing number.
E.g. image_001.jpg, image_002.jpg, image_003.jpg.
'''
import sys
import logging
from pathlib import Path

logging.basicConfig(level=logging.DEBUG)
# logging.disable(logging.CRITICAL)


def files_in_dir(folder):
    '''
    Returns a list of file objects.
    '''
    path = Path(folder)
    logging.debug('rawlist:\n%s', '\n'.join([f.name for f in path.iterdir()]))
    file_objects = [f for f in path.iterdir() if f.is_file()]
    logging.debug('Files in %s:\n%s', folder, file_objects)
    return sorted(file_objects)


def main():
    '''
    USAGE:
        renamer.py [name]

    Renames all the files in a folder with the name provided
    as an argument and an incrementing number.

    Example:
    >>>$ renamer.py image
    image_001.jpg
    image_002.jpg
    image_003.jpg
    '''
    logging.debug('sys.argv: %s', sys.argv)
    if len(sys.argv) < 2:
        print(f'{main.__doc__}')
        sys.exit(0)

    new_stem = str(sys.argv[1])
    num = 1

    file_objects = files_in_dir('.')
    logging.debug('File objects list: %s', file_objects)
    f_names_set = {f.name for f in file_objects}
    logging.debug('Filename set: %s', f_names_set)
    name_clashes = []
    for f_obj in file_objects:
        ext = f_obj.suffix
        new_name = f'{new_stem}_{num:03}{ext}'
        if new_name in f_names_set:
            new_name = f'TEMP_{new_name}'
            name_clashes.append(new_name)

        f_obj.rename(new_name)
        num += 1

    logging.debug('Name clashes list: %s', name_clashes)
    for name in name_clashes:
        f_obj = Path(name)
        new_name = name.replace('TEMP_', '')
        f_obj.rename(new_name)


if __name__ == '__main__':
    main()
