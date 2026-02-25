# Docker for Uroboros

## Docker build

Use `docker build` to build the docker image from Dockerfile.

```shell
# build using Ubuntu 22.04 (default)
docker build -f ./docker/Dockerfile.intel -t uroboros_intel .
# build using Ubuntu 20.04
docker build --build-arg UBUNTU_VERSION=20.04 -f ./docker/Dockerfile.intel -t uroboros_intel .
# build using Ubuntu 24.04
docker build --build-arg UBUNTU_VERSION=24.04 -f ./docker/Dockerfile.intel -t uroboros_intel .
```

## Docker usage

run the Docker container with `docker run`.

```shell
docker run -it -v <path to uroboros project directory>:/usr/src uroboros_intel bash
```
