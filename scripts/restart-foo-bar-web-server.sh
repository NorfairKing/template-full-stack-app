#!/usr/bin/env bash


killall foo-bar-api-server || true
killall foo-bar-web-server || true

api_port=8001
foo-bar-api-server --port "$api_port" &
foo-bar-web-server --api-url "http://localhost:$api_port" &
