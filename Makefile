####################################################################################################
# A containerized build environment for GNU COBOL programs deploying to AWS Lambda
# This environment includes:
#    - GNU Cobol Installation
#    - libxml2 - for XML processing
#    - libcjson - for JSON processing
#    - libcurl - for network interactions
####################################################################################################

#region Variables
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
OUTPUT_VOLUME_NAME=gnucobol-out
INPUT_VOLUME_NAME=gnucobol-src
VOLUME_CREATE_OPTS=--driver=local --opt type=none --opt o=bind --opt device="$(shell pwd)"
RUN_IMAGE=$(DOCKER) run -it -v $(OUTPUT_VOLUME_NAME):$(IMAGE_OUT_DIR):rw -v $(INPUT_VOLUME_NAME):$(SOURCES_DIR):ro
RUN_IMAGE_NO_IT=$(DOCKER) run -v $(OUTPUT_VOLUME_NAME):$(IMAGE_OUT_DIR):rw -v $(INPUT_VOLUME_NAME):$(SOURCES_DIR):ro

# Host directory and file aliases
HOST_OUT_DIR=out
LAYER_DIR=layer
LAYER_ZIP=cobol-layer.zip
LAMBDA_ZIP=cobol-lambda.zip

# Compliation aliases
LIBRARY_INCLUDES=-lcob -lcjson
COMPILE_EXECUTABLE=cobc -x $(LIBRARY_INCLUDES) -fword-continuation=ok -o $(IMAGE_OUT_DIR)/$(SOURCE)
COMPILE_EXT=cob

# Special aliases
BROWSE_ENTRYPOINT_OPTS=--entrypoint /bin/sh

#endregion

####################################################################################################
# Create ALL deployment artifacts
####################################################################################################
all:
	@$(MAKE) -f $(THIS_FILE) create-lambda
	@$(MAKE) -f $(THIS_FILE) create-layer

####################################################################################################
# Create docker execution environment
####################################################################################################
volumes:
	-mkdir $(HOST_OUT_DIR)
	-$(DOCKER) volume rm $(OUTPUT_VOLUME_NAME)
	$(DOCKER) volume create $(VOLUME_CREATE_OPTS)/out $(OUTPUT_VOLUME_NAME)
	$(DOCKER) volume create $(VOLUME_CREATE_OPTS)/src $(INPUT_VOLUME_NAME)

build:
	$(DOCKER) build -t $(IMAGE_REF) .

####################################################################################################
# Execute commands in the build environment
####################################################################################################

# Start a shell in the build container
browse: build volumes
	$(RUN_IMAGE) $(BROWSE_ENTRYPOINT_OPTS) $(IMAGE_REF)

# Compile and execute the target program in the build container
run-target: compile-target
	$(RUN_IMAGE_NO_IT) --entrypoint $(IMAGE_OUT_DIR)/$(SOURCE) < input.json $(IMAGE_REF) $(ARGS)

####################################################################################################
# Create deployable artifacts
####################################################################################################
compile-target:
	$(RUN_IMAGE) $(IMAGE_REF) $(COMPILE_EXECUTABLE) $(SOURCES_DIR)/$(SOURCE).$(COMPILE_EXT)

run-local:
	cobc -x $(LIBRARY_INCLUDES) -fword-continuation=ok src/json.cob
	LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/lib/:/usr/local/lib" ./json < input.json

copy-libs: build volumes
	$(RUN_IMAGE) $(IMAGE_REF) sh -c "cp -r /usr/lib64/libcjson* $(IMAGE_OUT_DIR)/ && cp -r /usr/local/lib/* $(IMAGE_OUT_DIR)/"
	cp $(HOST_OUT_DIR)/lib* $(LAYER_DIR)
	cp -r $(HOST_OUT_DIR)/gnucobol $(LAYER_DIR)

create-layer: copy-libs
	-rm ../$(LAYER_ZIP)
	-cp $(HOST_OUT_DIR)/lib* $(LAYER_DIR)
	-cp -r $(HOST_OUT_DIR)/gnucobol $(LAYER_DIR)
	cd $(LAYER_DIR) && $(ZIP_RECURSIVE) ../$(LAYER_ZIP) *

create-lambda: copy-libs
	@$(MAKE) -f $(THIS_FILE) compile-target SOURCE=application
	@$(MAKE) -f $(THIS_FILE) compile-target SOURCE=json
	-rm ../$(LAMBDA_ZIP)
	cd $(HOST_OUT_DIR) && $(ZIP_RECURSIVE) ../$(LAMBDA_ZIP) $(BINARY_NAMES)

####################################################################################################
# Publish artifacts to AWS for execution with the AWS CLI
####################################################################################################
publish-layer: create-layer
	aws lambda publish-layer-version --layer-name gnucobol --compatible-runtimes provided.al2 --zip-file fileb://cobol-layer.zip

publish-lambda: create-lambda
	aws lambda update-function-code --function-name dev-cobol-function --zip-file fileb://cobol-lambda.zip

####################################################################################################
# Clean up after ourselves - WARNING - this runs docker prune commands
####################################################################################################
clean:
	$(DOCKER) container prune
	$(DOCKER) image prune
	$(DOCKER) volume prune
	$(DOCKER) rmi $(IMAGE_REF)
	-rm -rf $(HOST_OUT_DIR)/lib*
	-sudo rm -rf $(HOST_OUT_DIR)/gnucobol
	-rm -rf $(LAYER_DIR)/gnucobol
	-rm -rf $(LAYER_DIR)/lib*
	-rm -rf $(LAMBDA_ZIP)
	-rm -rf $(LAYER_ZIP)
	-rm -rf $(HOST_OUT_DIR)/application
	-rm -rf $(HOST_OUT_DIR)/json