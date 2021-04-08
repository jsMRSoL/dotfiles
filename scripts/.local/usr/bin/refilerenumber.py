#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
A script to rename files with a stem and incrementing number.
E.g. image_001.jpg, image_002.jpg, image_003.jpg.
'''
import sys
import logging
from pathlib import Path

IMAGE_HOME = '/home/simon/vcol'
logging.basicConfig(level=logging.DEBUG)
# logging.disable(logging.CRITICAL)


def files_in_dir(fldr):
    '''
    Returns a list of file objects.
    '''
    logging.debug('rawlist:\n%s', '\n'.join([f.name for f in fldr.iterdir()]))
    file_objects = [f for f in fldr.iterdir() if f.is_file()]
    logging.debug('Files in %s:\n%s', fldr, file_objects)
    return sorted(file_objects)


def main():
    '''
    USAGE:
        renumber.py [dir]

    Renames all the files in the named folder with an incrementing number.

    Example:
    >>>$ renumber.py BL
    image_001.jpg
    image_002.jpg
    image_003.jpg
    '''
    logging.debug('sys.argv: %s', sys.argv)
    if len(sys.argv) < 2:
        print(f'{main.__doc__}')
        sys.exit(0)

    target_dir = Path(IMAGE_HOME, sys.argv[1])
    thumbs_dir = Path(target_dir, 'thumbs')
    num = 1

    # Get file list
    file_objects = files_in_dir(target_dir)
    logging.debug('File objects list: %s', file_objects)
    f_names_set = {f.name for f in file_objects}
    logging.debug('Filename set: %s', f_names_set)
    # Get thumb list
    thb_objects = files_in_dir(thumbs_dir)
    logging.debug('Thumb objects list: %s', thb_objects)
    thb_names_set = {f.name for f in thb_objects}
    logging.debug('Thumb name set: %s', thb_names_set)

    # Loop
    name_clashes = []
    thumb_clashes = []
    for f_obj in file_objects:
        thb = Path(f_obj.parent, 'thumbs', f'{f_obj.name}-thumb.jpg')
        ext = f_obj.suffix
        new_name = f'{target_dir.name}_{num:03}{ext}'
        logging.debug('File new name: %s', new_name)
        thb_new_name = f'{new_name}-thumb.jpg'
        logging.debug('Thumb new name: %s', thb_new_name)
        if new_name in f_names_set:
            new_name = f'TEMP_{new_name}'
            name_clashes.append(new_name)

        f_obj.rename(Path(target_dir, new_name))

        if thb_new_name in thb_names_set:
            thb_new_name = f'TEMP_{thb_new_name}'
            thumb_clashes.append(thb_new_name)

        thb.rename(Path(thumbs_dir, thb_new_name))
        num += 1

    # Handle clashes
    logging.debug('Name clashes list: %s', name_clashes)
    for name in name_clashes:
        f_obj = Path(target_dir, name)
        new_name = name.replace('TEMP_', '')
        f_obj.rename(Path(target_dir, new_name))

    logging.debug('Thumb clashes list: %s', thumb_clashes)
    for thumb in thumb_clashes:
        thb = Path(thumbs_dir, thumb)
        thb_new_name = thumb.replace('TEMP_', '')
        thb.rename(Path(thumbs_dir, thb_new_name))


if __name__ == '__main__':
    main()
