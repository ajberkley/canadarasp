#!/bin/bash
SPACEFREE=`df $1 | tail -1 | awk '{print $2}'`
if [ $SPACEFREE -gt 16000000 ]; then exit 0; else exit 1; fi
