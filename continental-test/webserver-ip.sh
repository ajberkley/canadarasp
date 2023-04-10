#!/bin/sh
# webserver.sh "WEBSERVERNAME"
WEBSERVERNAME=${1:-"WEB PROD V9"}
aws ec2 describe-instances --filter "Name=tag:Name,Values=$WEBSERVERNAME" --query "Reservations[*].Instances[*].PrivateIpAddress" --output text
