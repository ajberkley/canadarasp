#!/bin/bash

if [ "$DOMAIN" == "west" ]
then
    echo Using domain west
    XVALS=($(seq -134 2 -112))
    YVALS=($(seq 45 2 55))
else
    echo Using domain continental
    XVALS=($(seq -152 2 -42))
    YVALS=($(seq 37 2 70))
fi
