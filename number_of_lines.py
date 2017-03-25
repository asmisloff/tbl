#-*-    coding: utf-8   -*-
from __future__ import print_function
import os
import sys

files = [file for file in os.listdir("e:/Project/tbl") if file.endswith(".lsp")]

CNT = 0
for name in files:
    file = open(name)
    cnt = 0
    for line in file:
        if line != "\n":
            cnt += 1
    file.close()
    print("in {0} {1} rows".format(name, cnt))
    CNT += cnt

print("Total:", CNT, "rows")