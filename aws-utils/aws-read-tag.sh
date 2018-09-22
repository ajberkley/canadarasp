#!/bin/bash
INSTANCE=$1
NAME=$2
aws ec2 describe-tags --filters "Name=resource-id,Values=$INSTANCE" "Name=key,Values=$NAME" | awk '{print $5}'
