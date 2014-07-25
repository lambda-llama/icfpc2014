#!/usr/bin/env python
import re
import sys

def main():
    f = file(sys.argv[1])
    current_line = 0
    current_file = []
    current_labels = {}
    for line in f.readlines():
        if not line.strip():
            continue
        if line.startswith("  "):
            current_file.append(line.strip().split(";")[0].strip())
            current_line += 1
        else:
            current_labels[line.strip("\r\n: ")] = current_line
    for i in range(len(current_file)):
        s = current_file[i]
        for label, idx in current_labels.items():
            if s.find(label) >= 0:
                s = s.replace(label, str(idx))
        current_file[i] = s
    print "\n".join(current_file) + "\n"

if __name__ == "__main__":
    main()
