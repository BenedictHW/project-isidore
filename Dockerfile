FROM clfoundation/sbcl:2.2.0-alpine3.15
COPY . /project-isidore
ARG BUILDPACK_DIR=/project-isidore/bin
ARG BUILD_DIR=/project-isidore
ARG CACHE_DIR=/pi-build-cache
ARG QL_DIST_VER=2021-12-30
RUN mkdir -p /project-isidore/bin/lib && \
    wget https://beta.quicklisp.org/quicklisp.lisp \
    -O $BUILDPACK_DIR/lib/quicklisp.lisp
RUN sbcl --dynamic-space-size 512 --load $BUILDPACK_DIR/make.lisp && \
    mv -v $BUILD_DIR/src/ProjectIsidore $BUILD_DIR/ProjectIsidore && \
    chmod a+x $BUILD_DIR/ProjectIsidore && \
    rm -rf /pi-build-cache
CMD ["/project-isidore/ProjectIsidore"]
