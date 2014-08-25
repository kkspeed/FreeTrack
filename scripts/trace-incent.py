#!/usr/bin/python2
import os
import time
from   datetime import datetime

targets = ["16264167", "12712951", "12701167", "23546700", "13982166",
           "5184727", "12311975", "30462354", "32986732", "2939755",
           "12167736", "5127543", "32601287", "9419801", "30094136",
           "32323273", "23036166", "28853365", "27509040", "31738410",
           "32908492", "6533112", "17262639", "31295215", "24101908",
           "10807775", "18218123", "22674915", "4542015", "33539421",
           "26473864", "31787169", "31254603", "31674955", "2768080",
           "30062602", "32548386", "27633814", "8800508", "13608650"]

def run_task (targets):
    for t in targets:
       print "[" + str(datetime.now()) + "] " + t
       time.sleep(1)
       os.system("curl --data 'app=%s&user=%s' " % ("momo", t) +
                 "http://tdt-server:8080/submit-trace")

while(True):
    run_task(targets)
    time.sleep(2400)