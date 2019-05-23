#!/bin/bash
INSTANCE_NAME="HRDPS PROD V6"
aws ec2 describe-instances --filters Name=tag:Name,Values="$INSTANCE_NAME" | grep INSTANCES | awk '{ print $8 }'
