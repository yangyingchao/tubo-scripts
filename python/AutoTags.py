#!/usr/bin/python
import os
import sys
from glob import glob
from optparse import OptionParser

if __name__ == '__main__':
    usage = "usage: %prog [options] arg"
    parser = OptionParser(usage)
    parser.add_option("-f", "--file", dest="filename",
                      help="read data from FILENAME")
    parser.add_option("-v", "--verbose",
                      action="store_true", dest="verbose")
    parser.add_option("-q", "--quiet",
                      action="store_false", dest="verbose")
    parser.add_option("-u", "--update",
                      action="store_true", dest="update")
    (options, args) = parser.parse_args()

    dest = os.getcwd()
    if len(args) > 1:
        if sys.argv[1].startswith("/"):
            dest = sys.argv[1]
        else:
            dest = os.path.join(dest, args[0])

    if not os.path.isdir(dest):
        print "Error, destination should be a directory!"
        sys.exit(1)

    os.chdir(dest)
    if options.update is None:
        cmd = "/usr/local/bin/gtags -v"
    else:
        cmd = "/usr/local/bin/global -uv"

    for item in os.listdir("."):
        if not os.path.isdir(item):
            print "\nSkipping file: %s"%item
            continue
        print "\nProcessing: %s"%item
        for subitem in os.listdir(item):
            path = os.path.join(item, subitem)
            if os.path.isdir(path):
                print "\t Processing: %s"%path
                os.chdir(path)
                os.system(cmd)
                os.chdir(dest)
            else:
                print "\t%s not a directory!"%path
