#!/usr/bin/env python
# -*- coding: utf-8 -*-
# ***************************************************************************
# Copyright (C) 2010 yangyingchao@gmail.com

# Author: yangyingchao <yangyingchao@gmail.com>

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.

# You should have received a copy of the GNU General Public License along with
# GNU Emacs; see the file COPYING. If not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
# ****************************************************************************

from sgmllib import SGMLParser
from copy import deepcopy
from xml.dom.minidom import *
import os
import  sys
import urllib2
import re
import sys


class MyParser(SGMLParser):
    def __init__(self):
        self.link = []
        SGMLParser.__init__(self, 0)

    def handle_data(self, data):
        pass

    def flush(self):
        pass

    def handle_comment(self, data):
        pass

    def start_a(self, attrs):
        for (key, val) in attrs:
            if key == "href":
                self.link.append(val)

    def end_a(self):
        pass

    def unknown_starttag(self, tag, attrs):
        pass

    def unknown_endtag(self, tag):
        pass


    def unknown_entityref(self, ref):
        pass

    def unknown_charref(self, ref):
        pass

    def unknown_decl(self, data):
        pass

    def close(self):
        SGMLParser.close(self)
        self.flush()


def downURL(url, filename):
    print "Download %s, save as %s"%(url, filename)
    try:
        fp = urllib2.urlopen(url)
    except:
        print "download exception"
        return 0
    op = open(filename, "wb")
    while 1:
        s = fp.read()
        if not s:
            break
        op.write(s)
    fp.close( )
    op.close( )
    return 1

def getURL(url):
    print "Parsing %s"%url
    try:
        fp = urllib2.urlopen(url)
        contents = fp.readlines()
    except:
        print "exception"
        return []

    item_list = []
    for s in contents:
        urls = pattern.findall(s)
        if urls:
            item_list.extend(urls)
        fp.close( )
    return item_list

def reptile(base_url):
    """
    Download all articles from base_url.
    Arguments:
    - `base_url`: Url of website.
    """
    page_list = [base_url]

    try:
        fp = urllib2.urlopen(base_url)
        contents = fp.read()
    except:
        print "exception"
        print sys.exc_info()

    else:
        parser = MyParser()
        for item in contents:
            parser.feed(item)
        for link in parser.link:
            page_list.append(base_url+"/"+link)
        parser.close()

    for item in page_list:
        print item
        local_file = item.strip("/").split("/")[-1]
        ret = downURL(item,local_file)
    pass

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print "Usage: %s url"%sys.argv[0]
        sys.exit(1)
    for base_url in sys.argv[1:]:
        reptile (base_url)


