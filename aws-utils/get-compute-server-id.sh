#!/bin/bash
INSTANCE_NAME=${1:-"HRDPS PROD V9"}
aws --output json ec2 describe-instances --filters Name=tag:Name,Values="$INSTANCE_NAME" | jq .Reservations[0].Instances[0].InstanceId | tr -d '"'

