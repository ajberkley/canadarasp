#!/bin/bash
INSTANCE=$1
NAME=$2
aws ec2 describe-tags --output json --filters "Name=resource-id,Values=$INSTANCE" "Name=key,Values=$NAME" | jq .Tags[0].Value | tr -d \"
