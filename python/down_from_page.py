#!/usr/bin/env python
# -*- coding: utf-8 -*-
#!/usr/bin/env python
# -*- coding: utf-8 -*-
# ****************************************************************************
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
# GNU Emacs; see the file COPYING.  If not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
# ****************************************************************************

from copy import deepcopy
from sgmllib import SGMLParser
from xml.dom.minidom import *
import os
import re
import sys
import urllib2

title = "Untitled"

class MyParser(SGMLParser):

    def __init__(self):
        self.data = ""
        self.links = []
        self.TAG_BEG = False
        self.TAG_END = False
        SGMLParser.__init__(self, 0)

    def handle_data(self, data):
        if (self.TAG_BEG is True) and (self.TAG_END is False):
            self.data += data
        pass

    def start_title(self, attrs):
        self.link = ""
        self.data=""

        self.TAG_BEG = True
        self.TAG_END = False
        for (key, val) in attrs:
            if key == "href":
                self.link = val

    def end_title(self):
        self.TAG_BEG = False
        self.TAG_END = True

        self.title = self.data.strip()


    def flush(self):
        pass

    def handle_comment(self, data):
        pass

    def start_a(self, attrs):
        self.data=""

        self.TAG_BEG = True
        self.TAG_END = False
        for (key, val) in attrs:
            if key == "href":
                self.link = val

    def end_a(self):
        self.TAG_BEG = False
        self.TAG_END = True
        tmp = {}
        tmp["name"] = self.data
        tmp["link"] = self.link
        self.links.append(deepcopy(tmp))


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

def lst2str(lst):
    string = ""
    for item in lst:
        string += item.strip()+ "\n"
    return string

def downURL(url, filename):
    print "Download %s, save as %s"%(url, filename)
    try:
        fp = urllib2.urlopen(url)
    except:
        print "download exception"
        print sys.exc_info()
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


def reptile(base_url):
    """
    Download all articles from base_url.
    Arguments:
    - `base_url`: Url of website.
    """
    page_list = []
    if not len(base_url):
        print "No page to reptile!"
        sys.exit(1)

    parser = MyParser()

    if base_url.startswith("http"):
        myopen = urllib2.urlopen
    else:
        myopen = open

    try:
        content = myopen(base_url).read()
    except:
        print "Failed to read from %s."%base_url
        print sys.exc_info()

    for item in content:
        parser.feed(item)

    for tmp in parser.links:
        page_list.append(tmp.get("link"))

    global title
    title = parser.title
    parser.close()

    item_list = list(set(page_list))

    for item in item_list:
        # Strip '#' from url.
        pos = item.find('#')
        if pos != -1:
            item = item[:pos]

        # Added base_url to item if necessary
        if not item.startswith("http"):
            item = base_url.rstrip("/")+"/"+item
            pass

        local_file = item.split("/")[-1]
        print item, local_file
        if not local_file:
            print "Empty local file! Continue from next one!"
            continue

        if os.access(local_file, os.F_OK):
            print "File: %s existed, skip ..."%local_file
        else:
            ret = downURL(item, local_file)

    # Remember to download the index file!
    downURL(base_url, "index.html")
    print "Total: %d articles."%(len(item_list))
    pass


def walk_dir(lst, dirname, filenames):
    for filename in filenames:
        fn = os.path.join(dirname, filename)
        if os.path.isdir(fn) or \
                not filename.endswith("html"):
            continue
        print "Processing: %s"%fn
        tmp = {}
        parser = MyParser()
        content = open(fn).read()
        for item in content:
            parser.feed(item)
        tmp["file"] = filename
        tmp["title"] = parser.title
        parser.close()
        lst.append(deepcopy(tmp))
    pass

def gen_index():
    """
    Generate index of all htmls in this directory.
    """
    file_lists = []
    os.path.walk(".", walk_dir, file_lists)

    fp = open("%s.devhelp2"%os.path.basename(os.getcwd()), "w")
    string = '<?xml version="1.0" encoding="utf-8"?>\n<book author=""' +\
        ' language="c" link="index.html" name="" title="%s"'%title+\
        ' version="2" xmlns="http://www.devhelp.net/book">\n  <chapters>'
    for item in file_lists:
        link = item.get("file")
        try:
            name =item.get("title").decode('gbk').encode('utf-8')
        except:
            name = item.get("title")
        finally:
            string += '<sub link="%s" name="%s"/>\n'%(link, name)

    string +=   '\n</chapters>\n   </book>\n'
    fp.write(string)
    fp.close()

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print "Usage: %s url of baidu space"%sys.argv[0]
        print "Such as: %s http://hi.baidu.com/Username"
        gen_index()
        sys.exit(1)
    base_url = sys.argv[1]
    reptile (base_url)
    gen_index()
