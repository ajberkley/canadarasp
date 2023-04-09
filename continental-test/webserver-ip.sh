#!/bin/sh
# webserver.sh "WEBSERVERNAME"
WEBSERVER=${1:-"WEB PROD V9"}
aws ec2 describe-instances --filter "Name=tag:Name,Values=$WEBSERVER" --query "Reservations[*].Instances[*].PrivateIpAddress" --output text
