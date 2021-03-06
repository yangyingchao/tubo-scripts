#!/usr/bin/env python
# -*- coding: utf-8 -*-

from DNSWrapper import DNSServer

DB=[["209.244.0.3", "209.244.0.4"],
    ["8.8.8.8", "8.8.4.4"],
    ["8.26.56.26", "8.20.247.20"],
    ["208.67.222.222", "208.67.220.220"],
    ["156.154.70.1", "156.154.71.1"],
    ["199.85.126.10", "199.85.127.10"],
    ["81.218.119.11", "209.88.198.133"],
    ["195.46.39.39", "195.46.39.40"],
    ["216.87.84.211", "208.115.243.35"],
    ["199.5.157.131", "208.71.35.137"],
    ["208.76.50.50", "208.76.51.51"],
    ["216.146.35.35", "216.146.36.36"],
    ["37.235.1.174", "37.235.1.177"],
    ["89.233.43.71", "91.239.100.100"],
    ["84.200.69.80", "84.200.70.40"],
    ["74.82.42.42"], ["109.69.8.51"]]

def find_googles():
    outs = []
    for item in DB:
        server = DNSServer(item)
        outs.extend(server.GetAddress("google.cn"))
    return outs

if __name__ == '__main__':
    print find_googles()
