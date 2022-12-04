#!/usr/bin/env bash
echo "/us/bin/ls *.ad{s,b} *.{c,h} | sed -e /^/s//\"/ | sed -e /$/s//\"\,\ / >> units.txt" > units2.txt
echo " " >> units.txt

/usr/bin/ls *.ad{s,b} *.{c,h} | sed -e /^/s//\"/ | sed -e /$/s//\"\,\ / >> units2.txt

#usage: cd dir; ./create_units
