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

    def __init__(self, hosts=[]):
        HTMLParser.__init__(self)
        self.in_span = False
        self.hosts = hosts
        self.r = re.compile("([0-9.]+)[\s\n]+([a-zA-Z0-9._\-]+)")
        self.last_exp = ""
        self.parsed_entries = 0
        pass

    def handle_data(self, data):
        if self.in_span:
            self.last_exp += data

    def handle_starttag(self, tag, attrs):
        if tag == 'span':
            self.in_span = True

    def handle_endtag(self, tag):
        if tag == 'span':
            self.in_span = False
            m = self.r.match(self.last_exp)
            if m:
                entry = m.group(1) + "\t\t" + m.group(2)
                self.hosts.append(entry)
                self.parsed_entries += 1
                print("Got entries: %d"%(self.parsed_entries))
            self.last_exp = ""

    def save_to_fd(self, fd):
        if self.parsed_entries != 0:
            os.write(fd, '\n'.join(self.hosts) + '\n')
            return True
        else:
            return False

LOCAL_HOSTS = '/etc/hosts.local'
if __name__ == '__main__':
    local_hosts = []
    if os.access(LOCAL_HOSTS, os.F_OK):
        local_hosts = open(LOCAL_HOSTS).readlines();
    else:
        print("You need to put local hosts into %s if needed.\n"%(LOCAL_HOSTS))

    if not local_hosts:
        local_hosts.append('127.0.0.1    localhost')

    conn = httplib.HTTPConnection('www.360kb.com')
    conn.request('GET', '/kb/2_122.html')
    r = conn.getresponse()
    if r.status != 200:
        print("Failed to get response, status: %d -- %s\n"%(
            r.status, r.reason))
        sys.exit(1)

    data = r.read()

    p = MyHTMLParser(local_hosts)
    p.feed(data)

    if len(p.hosts) > 1:
        fn = None
        try:
            fd = os.open('/etc/hosts', os.O_RDWR, 644)
        except OSError as e:
            (fd, fn) = tempfile.mkstemp(prefix='hosts_')

        if (p.save_to_fd(fd)):
            if fn:
                print("Please copy %s to /etc/hosts, like:\n"
                      "sudo cat %s > /etc/hosts && rm -rf %s"%(fn, fn, fn))
            else:
                print("Total %d entries written.\n"%(len(p.hosts)))
        else:
            print("Did not find usable hosts...\n")
    else:
        print("No valid entries found...\n")
