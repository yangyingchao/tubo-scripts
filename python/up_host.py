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
        HTMLParser.__init__(self)
        self.data=""
        pass

    def handle_data(self, data):
        self.data += data

    def handle_starttag(self, tag, attrs):
        pass
    def handle_endtag(self, tag):
        pass

    def save_to_fd(self, fd):
        print self.hosts
        pass
        if self.parsed_entries != 0:
            os.write(fd, '\n'.join(self.hosts) + '\n')
            return True
        else:
            return False

LOCAL_HOSTS = '/etc/hosts.local'
if __name__ == '__main__':
    hosts = []
    if os.access(LOCAL_HOSTS, os.F_OK):
        hosts = open(LOCAL_HOSTS).readlines();
    else:
        print("You need to put local hosts into %s if needed.\n"%(LOCAL_HOSTS))

    if not hosts:
        hosts.append('127.0.0.1    localhost')

    conn = httplib.HTTPConnection('www.360kb.com')
    conn.request('GET', '/kb/2_122.html')
    r = conn.getresponse()
    if r.status != 200:
        print("Failed to get response, status: %d -- %s\n"%(
            r.status, r.reason))
        sys.exit(1)


    p = MyHTMLParser()
    p.feed(r.read())

    r = re.compile("([0-9.]+)[\s\n]+([a-zA-Z0-9._\-]+\.[a-zA-Z0-9._\-]+)")
    entries = 0
    pos = 0
    while True:
        s = r.search(p.data, pos)
        if s:
            hosts.append('%s\t%s'%(s.group(1), s.group(2)))
            pos = s.end()
            entries += 1
            print("Found %d entries\n"%(entries))
        else:
            break

    if entries > 1:
        fn = None
        try:
            fd = os.open('/etc/hosts', os.O_RDWR|os.O_CREAT, 644)
        except OSError as e:
            (fd, fn) = tempfile.mkstemp(prefix='hosts_')

        os.write(fd, '\n'.join(hosts) + '\n')
        if fn:
            print("Please copy %s to /etc/hosts, like:\n"
                  "sudo cat %s > /etc/hosts && rm -rf %s"%(fn, fn, fn))
        else:
            print("Total %d entries written.\n"%(len(hosts)))
    else:
        print("Did not find usable hosts...\n")
