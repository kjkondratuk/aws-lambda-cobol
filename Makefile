create-layer:
	-rm ../cobol-layer.zip
	-cp out/lib* layer/
	-cp -r out/gnucobol layer/
	cd layer && zip -r ../cobol-layer.zip *

create-lambda:
	-rm ../cobol-lambda.zip
	cd out && zip -r ../cobol-lambda.zip application

volumes:
	-mkdir out
	-docker volume rm gnucobol
	docker volume create --driver=local --opt type=none --opt o=bind --opt device="$(shell pwd)/out" gnucobol

build:
	docker build -t gnucobol:latest .

run: build volumes
	docker run -it -v gnucobol:/opt/bin gnucobol:latest

browse: build volumes
	docker run -it --entrypoint /bin/sh -v gnucobol:/opt/bin gnucobol:latest

all: run create-lambda create-layer

compile:
	cobc -xo $(ARGS) -lcob -lcjson

run-compiled:
	LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib" $(ARGS)

clean:
	-rm out/lib*
	-rm -rf out/gnucobol
	-rm -rf layer/gnucobol
	-rm layer/lib*
	-rm cobol-lambda.zip
	-rm cobol-layer.zip
	docker container prune
	docker image prune
	docker volume prune