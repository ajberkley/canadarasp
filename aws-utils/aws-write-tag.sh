#!/bin/bash
# Usage aws-tag-instance instance-ic key value
INSTANCE=$1
KEY=$2
VALUE=$3
aws ec2 create-tags --tags Key=$KEY,Value=$VALUE --resources $INSTANCE
