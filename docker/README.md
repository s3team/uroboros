# Docker for Uroboros

## Docker build

Use `docker build` to build the docker image from Dockerfile.

```shell
# build using Ubuntu 20.04 (default)
docker build -f ./docker/Dockerfile -t uroboros .
# build using Ubuntu 22.04
docker build --build-arg UBUNTU_VERSION=22.04 -f ./docker/Dockerfile -t uroboros .
# build using Ubuntu 24.04
docker build --build-arg UBUNTU_VERSION=24.04 -f ./docker/Dockerfile -t uroboros .
```

## Docker usage

run the Docker container with `docker run`.

```shell
docker run -it -v <path to uroboros parent>/uroboros:/usr/src uroboros bash
```
