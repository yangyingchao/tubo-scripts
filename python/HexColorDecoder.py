#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import os
import re

r_match_color = re.compile(".*\.(.*): *#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print ("Usage: %s filename GRAB"%sys.argv[0])
        sys.exit(1)

    fn   = sys.argv[1]
    grab = sys.argv[2]
    if os.access(fn, os.F_OK):
        contents = open(fn).readlines()
        if len(contents) != 0:
            for item in contents:
                if item.find(grab) != -1:
                    mm = r_match_color.match(item)
                    if mm:
                        color_name = mm.group(1)
                        color_r    = int(mm.group(2), 16)
                        color_g    = int(mm.group(3), 16)
                        color_b    = int(mm.group(4), 16)

                        msg="[%s]\n%s=%d,%d,%d\nTransparency=false\n\n"%\
                            (color_name, color_name, color_r, color_g,
                            color_b)
                        print msg
