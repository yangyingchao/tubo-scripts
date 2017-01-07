#!/usr/bin/env python
# -*- coding: utf-8 -*-

import random
import os

def main():

    print("开始。。。。。。\n")
    while True:
        a = int(random.random() * 1000 % 5)+1
        b = int(random.random() * 1000 % 5)+1

        s = a + b

        while True:
            try:
                c = int(input("（数学王国） 输入结果： %d + %d = " % (a, b)))
            except Exception as e:
                print e
                continue

            if s == cx:
                print("算对了！杨英超真棒！！\n")
                break
            else:
                print("哎呀，没算对，再试一次吧:")


if __name__ == '__main__':
    main()
