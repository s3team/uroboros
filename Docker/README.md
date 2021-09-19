# Docker for AIL

Before you start, you need to check out all necessary git submodules.

```shell
git submodule update --init --recursive
```

## Docker build


First copy or link your SPEC CPU 2006 iso file to `AIL/data/`.

```shell
ln -s ${your_spec_iso} ./data
```

Then use `docker build` to build the docker image from Dockerfile.

```shell
docker build -f ./Docker/Dockerfile -t ail:0.1 .
```

Some options can also be customized in `Dockerfile`.

```shell
# build spec cpu 2006
ENV SPECIMG=${your_spec_img_path}
ENV SRCDIR=${your_spec_src_extracted_path}
ENV BINDIR=${your_spec_bin_output_path}
ENV REBUILD_SPEC=${rebuild_spec_tool_flag}("" or "yes")
```

## Docker usage

run the Docker container with `docker run --privileged` in order to allow the container to create its own namespaces.

```shell
docker run --privileged -it -v$(pwd)/data:/data ail:0.1
```

Note: for each time you start the container, it will rebuild the SPEC tools.
