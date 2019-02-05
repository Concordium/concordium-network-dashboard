#!/bin/bash
# Create the image
docker build -f k8s.Dockerfile -t concordium/dashboard .

docker tag concordium/dashboard:latest 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/dashboard:latest

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/dashboard:latest
