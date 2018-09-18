#!/bin/bash
INSTANCE_NAME=${1:-"HRDPS DEV 4.1"}
INSTANCE_ID=`aws ec2 describe-instances --filters Name=tag:Name,Values="$INSTANCE_NAME" | grep INSTANCES | awk '{ print $8 }'`
if [ -z $INSTANCE_ID ]; then
    echo Could not find $INSTANCE_NAME
else
    aws ec2 stop-instances --instance-ids $INSTANCE_ID
fi
