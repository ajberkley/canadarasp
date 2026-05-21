#!/bin/sh
# Local-docker override. Production resolves an EC2 Name tag via `aws ec2`.
# In compose, the web service is reachable by its service name.
echo "web"
