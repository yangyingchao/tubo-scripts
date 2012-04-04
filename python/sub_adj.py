#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os
import re

g_rTime = re.compile("(\d\d):(\d\d):(\d\d),(\d+) .*? (\d\d):(\d\d):(\d\d),(\d+)$")

def array2str(array):
    string = ""
    for item in array:
        string += item
        if not item.endswith("\n"):
            string += "\n"
    return string

def adjust_part(array, delay):
    new = int(array[0]) * 3600 + int(array[1]) * 60 + int(array[2]) + delay

    if new < 0:
        new = 0

    ret = "%02d:%02d:%02d,%s"%(new/3600, new%3600/60, new%3600%60,  array[3])
    return ret

def adjust_sentence(array, delay):
    str1 = adjust_part(array[0:4], delay)
    str2 = adjust_part(array[4:], delay)
    return str1 + " --> " + str2

def adjust_file(delay, fn):
    print("Applying %d seconds to file: %s"%(delay, fn))
    try:
        content = open(fn).readlines()
    except  :
        print("Failed to open or read file")
        print sys.exc_info()
        return

    length = len(content)
    for i in range(length):
        item = content[i]
        m = g_rTime.match(item)
        if m:
            time_array = m.groups()
            if len(time_array) == 8:
                content[i] = adjust_sentence(time_array, delay)
    backup_file = fn + ".bak"
    if os.access(backup_file, os.F_OK):
        os.unlink(backup_file)

    os.rename(fn, backup_file)
    fd = open(fn, "w")
    fd.write(array2str(content))
    fd.close()

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: %s +/- Num(seconds) file[s]\n"%sys.argv[0])
        sys.exit(1)


    for fn in sys.argv[2:]:
        print("Processing file: %s"%fn)
        try:
            delay = int(sys.argv[1])
        except  :
            print("Failed to calculate time: %s"%sys.argv[1])
            continue
        if not os.access(fn, os.F_OK):
            print("File %s does not exist, skip!"%fn)
            continue
        else:
            adjust_file(delay, fn)

    print("\nAll work done.")

