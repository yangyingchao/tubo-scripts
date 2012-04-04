#!/usr/bin/env python
# -*- coding: utf-8 -*-
# *********************************************************************************
# Copyright (C) 2010 yangyingchao@gmail.com

# Author: yangyingchao <yangyingchao@gmail.com>

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# GNU Emacs; see the file COPYING.  If not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
# *********************************************************************************

from sgmllib import SGMLParser
from copy import deepcopy
from xml.dom.minidom import *
import os
import  sys

charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
keys = ["name", "link", "type"]

class MyParser(SGMLParser):
    def __init__(self):
        self.result = []
        self.data = ""
        self.link = ""
        self.TAG_BEG = False
        self.TAG_END = False
        SGMLParser.__init__(self, 0)

    def handle_data(self, data):
        if (self.TAG_BEG is True) and (self.TAG_END is False):
            self.data += data
        pass

    def flush(self):
        pass

    def handle_comment(self, data):
        pass

    def start_a(self, attrs):
        self.link = ""
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
        tmp["link"] = self.link
        tmp["type"] = "function"
        tmp["name"] = self.data.strip()

        self.result.append(deepcopy(tmp))

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


def get_links(to_be_parsed=[]):
    """
    Parse inputed HTML file and return a list of links.
    """
    result = []
    type = 0 #A sign to indicate type of reference htmls.
    fn_prefix = ""
    if len(to_be_parsed) == 0:
        if os.access("A.html", os.F_OK) or os.access("a.html", os.F_OK):
            type = 1
        elif os.access("index_A.html", os.F_OK) or \
                 os.access("index_a.html", os.F_OK):
            type = 2
            fn_prefix = "index_"

        else:
            print "Unrecoganized name of reference file.",\
                  "Modify this script according to corresponding file name."
            return []
            sys.exit()

        for char in charset:
            fn = fn_prefix + char + ".html"
            to_be_parsed.append(fn)

    for fn in to_be_parsed:
        if os.access(fn, os.F_OK):
            print "Parse: %s"%fn
            parser = MyParser()
            content = open(fn, "r").read()
            for item in content:
                parser.feed(item)
            result.extend(deepcopy(parser.result))
            parser.close()
    return result

def merge_dic(input_list):
    """
    Merge refernces. The ones in devhelp2 file will be keeped if multiple
    reference found.
    """
    name_list = []
    final = []
    while len(input_list):
        tmp = input_list.pop()
        if tmp["name"] not in name_list:
            name_list.append(tmp["name"])
            final.append(tmp)
    return final


if __name__ == '__main__':

    to_be_parsed = []

    if len(sys.argv)  < 2:
        print "A develop2 file should be provided!"
        sys.exit()
    if len(sys.argv) > 2:
        to_be_parsed.extend(sys.argv[2:])

    devhelp_file = sys.argv[1]

    refs =  get_links(to_be_parsed)

    doc = parse(devhelp_file)
    node_book = doc.getElementsByTagName("book")[0]

    func_list = doc.getElementsByTagName("functions")

    if func_list:
        print "Old functions existed, they will be merged."
        for node_func in func_list:
            for  nd in node_func.getElementsByTagName("keyword"):
                tmp = {}
                for key in keys:
                    tmp[key] = nd.getAttribute(key)
                refs.append(deepcopy(tmp))

            node_book.removeChild(node_func)

    node_func = doc.createElement("functions")
    node_book.appendChild(node_func)

    for dic in merge_dic(refs):
        nd = doc.createElement("keyword")
        for key in keys:
            nd.setAttribute(key, dic[key])
        node_func.appendChild(nd)

    try:
        os.rename(devhelp_file, "%s_backup"%devhelp_file)
    except:
        print "Failed to backup orignal file: %s.\n"%devhelp_file, \
              " Nothing to do."
        sys.exit()

    fp = open(devhelp_file, "w")
    doc.writexml(fp)
    fp.close()
