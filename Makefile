THIS_FILE := $(lastword $(MAKEFILE_LIST))

# Command aliases
DOCKER=docker
ZIP=zip
ZIP_RECURSIVE=$(ZIP) -r

# Build image aliases
IMAGE_OUT_DIR=/opt/bin
SOURCES_DIR=/opt/sources
BINARY_NAMES=application json
IMAGE_LIB_DIRS=/usr/local/lib

# Docker build aliases
IMAGE_NAME=gnucobol
IMAGE_TAG=latest
IMAGE_REF=$(IMAGE_NAME):$(IMAGE_TAG)
OUTPUT_VOLUME_NAME=gnucobol
VOLUME_CREATE_OPTS=--driver=local --opt type=none --opt o=bind --opt device="$(shell pwd)/out"
RUN_IMAGE=$(DOCKER) run -it -v $(OUTPUT_VOLUME_NAME):$(IMAGE_OUT_DIR):rw

# Host directory and file aliases
HOST_OUT_DIR=out
LAYER_DIR=layer
LAYER_ZIP=cobol-layer.zip
LAMBDA_ZIP=cobol-lambda.zip

# Compliation aliases
LIBRARY_INCLUDES=-lcob -lcjson
COMPILE_EXECUTABLE=cobc -x $(LIBRARY_INCLUDES) -o $(IMAGE_OUT_DIR)/$(SOURCE)
COMPILE_EXT=cob

# Special aliases
BROWSE_ENTRYPOINT_OPTS=--entrypoint /bin/sh

create-layer:
	-rm ../$(LAYER_ZIP)
	-cp $(HOST_OUT_DIR)/lib* $(LAYER_DIR)
	-cp -r $(HOST_OUT_DIR)/gnucobol $(LAYER_DIR)
	cd $(LAYER_DIR) && $(ZIP_RECURSIVE) ../$(LAYER_ZIP) *

publish-layer:
	aws lambda publish-layer-version --layer-name gnucobol --compatible-runtimes provided.al2 --zip-file fileb://cobol-layer.zip

create-lambda:
	-rm ../$(LAMBDA_ZIP)
	cd $(HOST_OUT_DIR) && $(ZIP_RECURSIVE) ../$(LAMBDA_ZIP) $(BINARY_NAMES)

publish-lambda:
	aws lambda update-function-code --function-name dev-cobol-function --zip-file fileb://cobol-lambda.zip

volumes:
	-mkdir $(HOST_OUT_DIR)
	-$(DOCKER) volume rm $(OUTPUT_VOLUME_NAME)
	$(DOCKER) volume create $(VOLUME_CREATE_OPTS) $(OUTPUT_VOLUME_NAME)

build:
	$(DOCKER) build -t $(IMAGE_REF) .

run: build volumes
	$(RUN_IMAGE) $(IMAGE_REF)

browse: build volumes
	$(RUN_IMAGE) $(BROWSE_ENTRYPOINT_OPTS) $(IMAGE_REF)

all:
	@$(MAKE) -f $(THIS_FILE) copy-libs
	@$(MAKE) -f $(THIS_FILE) compile-target SOURCE=application
	@$(MAKE) -f $(THIS_FILE) compile-target SOURCE=json
	@$(MAKE) -f $(THIS_FILE) create-lambda
	@$(MAKE) -f $(THIS_FILE) create-layer

copy-libs:
	$(RUN_IMAGE) $(IMAGE_REF) sh -c "cp -r /usr/lib64/libcjson* $(IMAGE_OUT_DIR)/ && cp -r /usr/local/lib/* $(IMAGE_OUT_DIR)/"
	cp $(HOST_OUT_DIR)/lib* $(LAYER_DIR)
	cp -r $(HOST_OUT_DIR)/gnucobol $(LAYER_DIR)

compile-target:
	$(RUN_IMAGE) $(IMAGE_REF) $(COMPILE_EXECUTABLE) $(SOURCES_DIR)/$(SOURCE).$(COMPILE_EXT)

run-target: compile-target
	$(RUN_IMAGE) -e LD_LIBRARY_PATH="/usr/local/lib" --entrypoint $(IMAGE_OUT_DIR)/$(SOURCE) $(IMAGE_REF) $(ARGS)

clean:
	-rm $(HOST_OUT_DIR)/lib*
	-rm -rf $(HOST_OUT_DIR)/gnucobol
	-rm -rf $(LAYER_DIR)/gnucobol
	-rm $(LAYER_DIR)/lib*
	-rm $(LAMBDA_ZIP)
	-rm $(LAYER_ZIP)
	-rm $(HOST_OUT_DIR)/application
	-rm $(HOST_OUT_DIR)/json
	$(DOCKER) container prune
	$(DOCKER) image prune
	$(DOCKER) volume prune