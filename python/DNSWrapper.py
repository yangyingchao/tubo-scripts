#!/usr/bin/env python
# -*- coding: utf-8 -*-

import dns
import dns.resolver

class DNSServer(object):
    """Representation of a DNSServer
    """

    def __init__(self, servers):
        """Initialize a DNSServer

        Arguments:
        - `primary`:
        - `secondary`:
        """
        self._servers = servers

    def GetAddress(self, host):
        """Get address of specified host.
        """
        outs = []
        r = dns.resolver.Resolver()
        r.nameservers = self._servers
        ans  = dns.resolver.query('google.com', 'A').response.answer[0]
        for item in ans.items:
            outs.append("%s"%item)
        return outs



def find_hosts():
    s = DNSServer("209.244.0.3", "209.244.0.4")
    s.GetAddress("www.google.com")

# "http://pcsupport.about.com/od/tipstricks/a/free-public-dns-servers.htm"
if __name__ == '__main__':
    find_hosts()
