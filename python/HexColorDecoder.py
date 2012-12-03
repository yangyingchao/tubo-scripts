#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import os
import re

r_match_color = re.compile("URxvt\.color([0-9]+): *#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$")

class Color( ):
    """
    """

    def __init__(self, index, r, g, b):
        """
        Arguments:
        - `r`:
        - `g`:
        - `b`:
        """
        self._index = index;
        self._r     = r
        self._g     = g
        self._b     = b

    def __str__(self):
        """
        """
        if self._index <= 7:
            fmt = "[Color%d]\nColor=%d,%d,%d\n\n"
            offset = 0;
        else:
            offset = 8
            fmt = "[Color%dIntense]\nColor=%d,%d,%d\n\n"

        return fmt%(self._index-offset, self._r, self._g, self._b)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print ("Usage: %s filename GRAB"%sys.argv[0])
        sys.exit(1)

    fn   = sys.argv[1]
    color_list = []
    if os.access(fn, os.F_OK):
        contents = open(fn).readlines()
        if len(contents) != 0:
            for item in contents:
                mm = r_match_color.match(item)
                if mm:
                    color_i = int(mm.group(1))
                    color_r = int(mm.group(2), 16)
                    color_g = int(mm.group(3), 16)
                    color_b = int(mm.group(4), 16)
                    c = Color(color_i, color_r, color_g, color_b)
                    color_list.append(c);
                    print c
