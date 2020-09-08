#!/bin/bash
INSTANCE_NAME=${1:-"HRDPS PROD V7"}
aws ec2 describe-instances --filters Name=tag:Name,Values="$INSTANCE_NAME" | grep INSTANCES | awk '{ print $8 }'
