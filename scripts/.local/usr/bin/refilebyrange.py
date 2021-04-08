#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
A script to move image files and their thumbnails on the server.
'''
import sys
import logging
from pathlib import Path

logging.basicConfig(level=logging.DEBUG)
# logging.disable(logging.CRITICAL)

IMAGE_HOME = "/home/simon/vcol"

def get_num(folder):
    '''
    Returns the number for the next filename in the given folder.
    '''
    file_objects = [f for f in folder.iterdir() if f.is_file()]
    if len(file_objects) == 0:
        return 0
    last_file = sorted(file_objects)[-1].stem
    logging.debug('Last file: %s', last_file)
    try:
        num = last_file.split('_')[1]
        logging.debug('Number: %s', num)
        num = int(num)
    except ValueError as e:
        print(f'File parsing error: there was no _ in last file in {folder.name}.')
        print(f'Error message: {e}')
        sys.exit(0)

    return num


def is_in_range(file_obj, start, end):
    stem = file_obj.stem
    digits = stem.split('_')[1]
    num = int(digits)
    return start <= num <= end


def main():
    '''
    USAGE:
        refileonserver.py [target_dir] [src_dir] [start] [end]

    Move the files in [src_dir] whose names include numbers in the range from
    [start] to [end] and their thumbnails to the provided directory [dir] and
    rename them according to the numbering in that directory.

    Example:
    >>>$ refileonserver.py Pictures Screencaps image_001.jpg
    '''
    logging.debug('sys.argv: %s', sys.argv)
    if len(sys.argv) != 5:
        print(f'{main.__doc__}')
        sys.exit(0)

    target_name = sys.argv[1]
    target = Path(IMAGE_HOME, target_name)
    num = get_num(target)

    src_name = sys.argv[2]
    src_dir = Path(IMAGE_HOME, src_name)

    start = int(sys.argv[3])
    end = int(sys.argv[4])

    # Get candidate files
    files = [f for f in src_dir.iterdir()
                  if f.is_file() and is_in_range(f, start, end)]

    for src_obj in files:

        num += 1
        # Handle image
        logging.debug('Source file name: %s', src_obj)
        ext = src_obj.suffix
        new_name = f'{target_name}_{num:03}{ext}'
        logging.debug('New file name: %s', new_name)
        logging.debug('New file target: %s', target.joinpath(new_name))
        src_obj.rename(target.joinpath(new_name))

        # Handle thumbnail
        src_obj_thb = Path(IMAGE_HOME, src_dir, 'thumbs', f'{src_obj.name}-thumb.jpg')
        logging.debug('Source thumb name: %s', src_obj_thb)
        new_thb_name = f'{new_name}-thumb.jpg'
        logging.debug('New thumb name: %s', new_thb_name)
        logging.debug('New thumb target: %s', target.joinpath('thumbs', new_thb_name))
        src_obj_thb.rename(target.joinpath('thumbs', new_thb_name))


if __name__ == '__main__':
    main()
