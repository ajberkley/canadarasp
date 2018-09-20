#!/bin/bash
INSTANCE_NAME="HRDPS DEV 4.1"
aws ec2 describe-instances --filters Name=tag:Name,Values="$INSTANCE_NAME" | grep INSTANCES | awk '{ print $8 }'
