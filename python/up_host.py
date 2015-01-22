#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import httplib
from HTMLParser import HTMLParser
import re
import tempfile

class MyHTMLParser(HTMLParser):
    """
    """

    def __init__(self):
        """
        """
        HTMLParser.__init__(self)
        self.in_span = False
        self.hosts = ["127.0.0.1    localhost"]
        self.r = re.compile("([0-9.]+)[\s\n]+([a-zA-Z0-9._]+)")
        self.last_exp = ""
        pass
    def handle_data(self, data):
        """
        """
        if self.in_span:
            self.last_exp += data

    def handle_starttag(self, tag, attrs):
        """

        Arguments:

        - `tag`:
        - `attrs`:
        """
        if tag == 'span':
            self.in_span = True

    def handle_endtag(self, tag):
        """
        """
        if tag == 'span':
            self.in_span = False
            m = self.r.match(self.last_exp)
            if m:
                entry = m.group(1) + "\t\t" + m.group(2)
                self.hosts.append(entry)
                print("Got entries: %d\n"%(len(self.hosts)))
            self.last_exp = ""

if __name__ == '__main__':
    conn = httplib.HTTPConnection('www.360kb.com')
    conn.request('GET', '/kb/2_122.html')
    r = conn.getresponse()
    if r.status != 200:
        print("Failed to get response, status: %d -- %s\n"%(
            r.status, r.reason))
        sys.exit(1)

    data = r.read()

    p = MyHTMLParser()
    p.feed(data)

    if len(p.hosts) > 1:
        (fd, fn) = tempfile.mkstemp(prefix='hosts_')
        os.write(fd, "\n".join(p.hosts))
        os.close(fd)
        print("Please copy %s to /etc/hosts, like:\n"
              "sudo mv %s /etc/hosts"%(fn, fn))
    else:
        print("No valid entries found...\n")
