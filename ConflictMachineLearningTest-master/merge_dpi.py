#!/usr/bin/env python2

# Author:   Konrad Talik <konrad.talik@slimak.matinf.uj.edu.pl>
# License:  CC0

import csv

with open('DATA/dpi.csv', 'w') as dpi:
    writer = csv.writer(dpi)
    header_missing = True

    for part in [
        'DATA/dpi.1.csv',
        'DATA/dpi.2.csv'
        ]:

        with open(part, 'rU') as dpi_part:
            reader = csv.reader(dpi_part)
            first = True

            for row in reader:

                if first and header_missing:
                    header_missing = False
                    first = False
                elif first:
                    first = False
                    continue

                writer.writerow(row)
