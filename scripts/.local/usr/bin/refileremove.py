#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys
from pathlib import Path
import logging


logging.basicConfig(level=logging.DEBUG)
# logging.disable(logging.CRITICAL)

IMAGE_HOME = '/home/simon/vcol'


def remove(id, file):
    target = Path(IMAGE_HOME, id, file)
    logging.debug("Target file: %s", target)
    if target.exists():
        target.unlink()

    thb = Path(target.parent, 'thumbs', f'{target.name}-thumb.jpg')
    logging.debug("Target thumb: %s", thb)
    if thb.exists():
        thb.unlink()


def main():
    '''
    Usage:
       refileremover.py [dir] [filename]*

    Deletes all the named files in the named folder.

    Example:
    >>>$ renamer.py BL BL_001.jpg BL_002.jpg
    '''
    if len(sys.argv) < 2:
        print(f'{main.__doc__}')
        sys.exit(0)

    id = sys.argv[1]
    files = sys.argv[2:]
    for file in files:
        remove(id, file)


if __name__ == "__main__":
    main()
