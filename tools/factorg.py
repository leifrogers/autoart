import datetime
import string
import os

filename = "factoids.clp"
preload = open(filename, "r")
lines = [line.rstrip('\n') for line in preload]
preload.close

lines.sort()

file = open("factoidsOrg.clp", "w")

for line in lines:
    print(line)
    file.write(line + "\n")
    
file.close()