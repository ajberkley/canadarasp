#!/bin/bash
INSTANCE_NAME=${1:-"HRDPS PROD V9"}
INSTANCE_ID=`aws --output json ec2 describe-instances --filters Name=tag:Name,Values="$INSTANCE_NAME" | jq .Reservations[0].Instance[0].InstanceId`
if [ $INSTANCE_ID == null ]; then
    echo Could not find $INSTANCE_NAME to stop it
else
    aws ec2 stop-instances --instance-ids $INSTANCE_ID
fi
