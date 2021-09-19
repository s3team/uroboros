# Docker for uroboros v 0.12

## Docker build


Then use `docker build` to build the docker image from Dockerfile.

```shell
docker build -f Dockerfile -t ail:0.12 .
```

## Docker usage

run the Docker container with `docker run --privileged` in order to allow the container to create its own namespaces.

```shell
docker run -it ail:0.12
```
