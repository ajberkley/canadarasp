#!/bin/sh
aws ec2 describe-instances --filter "Name=tag:Name,Values=WEB PROD V6" --query "Reservations[*].Instances[*].PrivateIpAddress" --output text
