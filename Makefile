#! /bin/zsh

build:
	docker build -t gnucobol:1.0 -t gnucobol:latest --build-arg COBC_VERSION=3.1.2 .

run:
	docker run -it -v gnucobol:/root:rw gnucobol:1.0

volumes:
	docker volume create --driver=local --opt type=none --opt o=bind --opt device="$(shell pwd)" gnucobol

clean:
	docker volume rm gnucobol
	docker image prune