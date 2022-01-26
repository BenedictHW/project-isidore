FROM alpine:3.15.0
COPY . /project-isidore
ARG BUILDPACK_DIR=/project-isidore/bin
ARG BUILD_DIR=/project-isidore
ARG CACHE_DIR=/pi-build-cache
ARG QL_DIST_VER=2021-12-30
RUN apk add --no-cache sbcl && \
    mkdir -p /project-isidore/bin/lib && \
    wget https://beta.quicklisp.org/quicklisp.lisp \
    -O $BUILDPACK_DIR/lib/quicklisp.lisp && \
    sbcl --dynamic-space-size 2048 --load $BUILDPACK_DIR/make.lisp && \
    mv -v $BUILD_DIR/src/ProjectIsidore $BUILD_DIR/ProjectIsidore && \
    chmod a+x $BUILD_DIR/ProjectIsidore && \
    rm -rf /pi-build-cache && \
    apk del sbcl
CMD ["/project-isidore/ProjectIsidore"]
