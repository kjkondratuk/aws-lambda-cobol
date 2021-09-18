#! /bin/zsh

build:
	docker build -t gnucobol:1.0 -t gnucobol:latest --build-arg COBC_VERSION=3.1.2 .
