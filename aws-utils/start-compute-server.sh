#!/bin/bash
INSTANCE_NAME=${1:-"HRDPS PROD V9"}
INSTANCE_ID=`aws --output json ec2 describe-instances --filters Name=tag:Name,Values="HRDPS PROD V9" | jq .Reservations[0].Instances[0].InstanceId | tr -d \"`
if [ -z $INSTANCE_ID ]; then
    echo Could not find $INSTANCE_NAME
else
    echo Starting $INSTANCE_NAME which has ID $INSTANCE_ID
    aws ec2 start-instances --instance-ids $INSTANCE_ID
fi
