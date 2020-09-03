#!/bin/bash
INSTANCE_NAME=${1:-"HRDPS PROD V6"}
INSTANCE_ID=`aws ec2 describe-instances --filters Name=tag:Name,Values="$INSTANCE_NAME" | grep INSTANCES | awk '{ print $8 }'`
if [ -z $INSTANCE_ID ]; then
    echo Could not find $INSTANCE_NAME
else
    echo Starting $INSTANCE_NAME which has ID $INSTANCE_ID
    aws ec2 start-instances --instance-ids $INSTANCE_ID
fi
