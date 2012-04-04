#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os

LINES = 100

def lst2str(lst):
    string = ""
    for item in lst:
        string += item.strip()+"\n"
    return string


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print "Usage: %s book"%sys.argv[0]
        sys.exit(1)

    fn = sys.argv[1]
    if not os.access(fn, os.F_OK):
        print "File %s can not be accessed!"%fn
        sys.exit(1)

    try:
        content = open(fn, "r").readlines()
    except:
        print "Failed to open or read orignal file: %s"%fn
        print sys.exc_info()
        sys.exit(1)

    total = len(content)
    num_sec = int(total/LINES)
    for idx in range(num_sec):
        fn = "%03d.txt"%(idx+1)
        try:
            fp = open(fn, "w")
            fp.write(lst2str(content[idx*LINES:(idx+1)*LINES]))
            fp.close()
        except:
            print "failed to write file: %s"%fn
            print sys.exc_info()

            # Write last section.

    fn = "%03d.txt"%(idx+2)
    try:
        fp = open(fn, "w")
        fp.write(lst2str(content[(idx+1)*LINES:]))
        fp.close()
    except:
        print "failed to write file: %s"%fn
