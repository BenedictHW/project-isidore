FROM clfoundation/sbcl:2.2.4-slim as build
COPY . /project-isidore/
ARG BUILD_DIR=/project-isidore
ARG CACHE_DIR=/pi-build-cache
ARG QL_DIST_VER=2022-04-01
RUN apt-get update && apt-get install -y curl \
    && curl https://beta.quicklisp.org/quicklisp.lisp -o $BUILD_DIR/quicklisp.lisp \
    && sbcl --dynamic-space-size 512 --load $BUILD_DIR/src/make.lisp \
    && chmod a+x $BUILD_DIR/bin/ProjectIsidore
FROM gcr.io/distroless/base-debian11
COPY --from=build /lib/x86_64-linux-gnu/libz.so.1 /lib/x86_64-linux-gnu/libz.so.1
COPY --from=build /project-isidore/bin/ /project-isidore/bin/
COPY --from=build /project-isidore/assets/ /project-isidore/assets/
COPY --from=build /project-isidore/data/ /project-isidore/data/
CMD ["/project-isidore/bin/ProjectIsidore"]
