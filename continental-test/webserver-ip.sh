#!/bin/sh
aws ec2 describe-instances --filter "Name=tag:Name,Values=Webserver Dev" --query "Reservations[*].Instances[*].PrivateIpAddress" --output text
