# Single image used by both `web` and `compute` services. Differs only in
# entrypoint. Targets linux/amd64 because NCL is x86_64-only — on Apple Silicon
# this runs under emulation.

# NCL 6.6.2 with all its runtime libs bundled in /opt/conda. We multi-stage
# copy that prefix rather than install via conda ourselves — conda's solver
# deadlocks on NCL's dep graph under amd64 emulation, but `docker pull` of an
# already-resolved image is fine.
#
# Trim ~2.9 GB of conda artifacts NCL doesn't need at runtime before the
# final COPY: the package cache, C/C++ headers, Python (NCL is its own
# scripting language and doesn't shell out to python), conda metadata, and
# man/doc.
FROM --platform=linux/amd64 qcmiao1998/ncl:latest AS ncl
RUN rm -rf /opt/conda/pkgs \
           /opt/conda/include \
           /opt/conda/lib/python3.* \
           /opt/conda/conda-meta \
           /opt/conda/share/doc \
           /opt/conda/share/man \
           /opt/conda/man \
           /opt/conda/info \
           /opt/conda/doc

FROM --platform=linux/amd64 ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=America/Vancouver \
    LC_ALL=C.UTF-8 \
    LANG=C.UTF-8

# -- System packages --------------------------------------------------------
# noble ships GDAL 3.8.x (satisfies the ≥3.6.1 requirement) and wgrib2.
# parallel is used heavily by the pipeline. apache2 + default-jre are only
# needed by the `web` service but the same image serves both roles.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl wget git jq unzip bzip2 sudo tzdata \
        parallel \
        sbcl \
        libgdal-dev gdal-bin \
        libeccodes-dev libeccodes-data \
        imagemagick ghostscript \
        apache2 default-jre-headless \
        build-essential libssl-dev libffi-dev \
        netbase \
        libgd-dev fonts-liberation \
    && rm -rf /var/lib/apt/lists/* \
    # cl-gd renders the map legend/title text via libgd's FreeType API and looks
    # up fonts by name ("arial","trebuc","trebucbd") under the hard-coded
    # GDFONTPATH below. The image ships no msttcorefonts, so point those names at
    # the metric-compatible Liberation faces. Without this the legend/title images
    # fail to render (the calls are swallowed by ignore-errors) for every model.
    && mkdir -p /usr/share/fonts/truetype/msttcorefonts \
    && ln -sf /usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf /usr/share/fonts/truetype/msttcorefonts/arial.ttf \
    && ln -sf /usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf /usr/share/fonts/truetype/msttcorefonts/trebuc.ttf \
    && ln -sf /usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf    /usr/share/fonts/truetype/msttcorefonts/trebucbd.ttf

# noble's base image already has a default user `ubuntu` (UID 1000). Give it
# passwordless sudo so production scripts that use `sudo` for swapfile/mount
# don't die — the overrides skip those steps anyway, but a few scripts still
# call `sudo chown`. Re-number the user to match the host (build-arg, default
# 1000) so files written back through bind mounts stay owned by the host user.
ARG HOST_UID=1000
ARG HOST_GID=1000
RUN if [ "$HOST_UID" != "1000" ] || [ "$HOST_GID" != "1000" ]; then \
        groupmod -g "$HOST_GID" ubuntu && \
        usermod -u "$HOST_UID" -g "$HOST_GID" ubuntu && \
        chown -R "$HOST_UID":"$HOST_GID" /home/ubuntu ; \
    fi \
    && usermod -aG sudo ubuntu \
    && echo 'ubuntu ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers \
    && mkdir -p /mnt && chown ubuntu:ubuntu /mnt \
    && chmod o+x /home/ubuntu

# Production scripts hard-code these paths.
RUN ln -sf /usr/bin/sbcl /usr/local/bin/sbcl \
    && ln -sf /usr/bin/gdal_translate /usr/local/bin/gdal_translate \
    && ln -sf /usr/bin/gdalwarp /usr/local/bin/gdalwarp

USER ubuntu
WORKDIR /home/ubuntu

# -- Quicklisp + Lisp libraries (mirrors setup-lisp.sh) --------------------
# Two non-obvious things going on:
#  - Quicklisp's bootstrap uses its own small HTTP client which is fragile
#    under amd64 emulation on Apple Silicon — fetches of the 839-byte
#    `quicklisp.sexp` intermittently fail with END-OF-DATA. Retry.
#  - `ql:add-to-init-file` calls (press-enter-to-continue) which hits EOF in
#    `--non-interactive` mode. We skip it and write ~/.sbclrc ourselves.
RUN curl -sSL https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp \
    && for i in 1 2 3 4 5; do \
         rm -rf /home/ubuntu/quicklisp; \
         sbcl --non-interactive --load /tmp/quicklisp.lisp \
              --eval '(quicklisp-quickstart:install)' \
           && break; \
         echo "[build] quicklisp install attempt $i failed, retrying after 5s"; \
         sleep 5; \
       done \
    && test -f /home/ubuntu/quicklisp/setup.lisp \
    && printf '%s\n' \
         '#-quicklisp' \
         '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"' \
         '                                       (user-homedir-pathname))))' \
         '  (when (probe-file quicklisp-init)' \
         '    (load quicklisp-init)))' \
         > /home/ubuntu/.sbclrc \
    && rm /tmp/quicklisp.lisp

# Three companion repos referenced via ASDF source-registry, exactly as
# setup-lisp.sh wires them on production.
RUN mkdir -p /home/ubuntu/.config/common-lisp/source-registry.conf.d \
    && git clone --depth=1 https://github.com/ajberkley/cl-png.git           /home/ubuntu/cl-png \
    && git clone --depth=1 https://github.com/ajberkley/cl-gdal.git          /home/ubuntu/cl-gdal \
    && git clone --depth=1 https://github.com/ajberkley/mmap-shared-cache.git /home/ubuntu/mmap-shared-cache \
    && printf '(:tree "/home/ubuntu/cl-png")\n'             > /home/ubuntu/.config/common-lisp/source-registry.conf.d/10-cl-png.conf \
    && printf '(:tree "/home/ubuntu/cl-gdal")\n'            > /home/ubuntu/.config/common-lisp/source-registry.conf.d/11-cl-gdal.conf \
    && printf '(:tree "/home/ubuntu/mmap-shared-cache")\n'  > /home/ubuntu/.config/common-lisp/source-registry.conf.d/12-mmap-shared-cache.conf

# Pre-load quicklisp packages used across the codebase so first run is fast
# and image-build catches any version-skew issues early.
RUN sbcl --non-interactive \
         --eval '(ql:quickload "png")' \
         --eval '(ql:quickload "osicat")' \
         --eval '(ql:quickload "alexandria")' \
         --eval '(ql:quickload "cl-gd")' \
         --eval '(ql:quickload "cl-ppcre")' \
         --eval '(ql:quickload "iterate")' \
         --eval '(ql:quickload "local-time")' \
         --eval '(ql:quickload "parse-number")' \
         --eval '(ql:quickload "parse-float")' \
         --eval '(ql:quickload "cl-fad")' \
         --eval '(ql:quickload "trivial-garbage")' \
         --eval '(ql:quickload "hunchentoot")'

# cl-gd ships a C glue (cl-gd-glue.so) it does NOT build automatically; quickload
# above only fetches/compiles its Lisp. Build the glue now that libgd-dev is
# present so draw-freetype-string resolves at runtime (otherwise the map
# legend/title text silently fails to render). Mirrors cl-gd's own Makefile.
RUN CLGD_DIR="$(dirname "$(find /home/ubuntu/quicklisp -name cl-gd-glue.c | head -1)")" \
    && make -C "$CLGD_DIR" \
    && test -f "$CLGD_DIR/cl-gd-glue.so"

# -- wgrib2 via micromamba --------------------------------------------------
# wgrib2 isn't packaged for Ubuntu. micromamba (single static binary, no
# Python) installs it from conda-forge in ~30s with a small dep graph.
USER root
RUN curl -L https://micro.mamba.pm/api/micromamba/linux-64/latest \
        | tar -C /usr/local -xj bin/micromamba \
    && chmod +x /usr/local/bin/micromamba \
    && /usr/local/bin/micromamba --version
USER ubuntu

ENV MAMBA_ROOT_PREFIX=/home/ubuntu/micromamba \
    MAMBA_NO_BANNER=1

# do-generate-new-variables.sh runs `parallel --env _` which requires
# `parallel --record-env` to have run once before; otherwise it warns on
# every invocation. Also suppress GNU parallel's citation nag (`will cite`).
RUN mkdir -p /home/ubuntu/.parallel \
    && touch /home/ubuntu/.parallel/will-cite \
    && parallel --record-env

RUN micromamba create -y -p /home/ubuntu/micromamba -c conda-forge \
        --strict-channel-priority --override-channels \
        wgrib2 \
    && micromamba clean -afy

# -- NCL 6.6.2 (copied from dtcenter-style image, see top of Dockerfile) ---
# NCL ships all its runtime libs in /opt/conda/lib so it doesn't conflict
# with the system libgdal/libnetcdf in this image. Scripts that hard-code
# NCARG_ROOT=/home/ubuntu/NCARG/ work via the symlink.
USER root
COPY --from=ncl /opt/conda /opt/conda
RUN ln -sfn /opt/conda /home/ubuntu/NCARG
USER ubuntu

ENV NCARG_ROOT=/opt/conda \
    PATH=/home/ubuntu/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/ubuntu/micromamba/bin:/opt/conda/bin

# Production webserver-install.md creates `~/continental-test` as a symlink
# into the repo (do-tile-generation.sh references /home/ubuntu/continental-test/
# for grib2tables). Bake that in.
RUN ln -s /home/ubuntu/canadarasp/continental-test /home/ubuntu/continental-test

# Apache wants to log to /var/log/apache2 and run as www-data. We override
# DocumentRoot at run time via a local vhost file copied in by the web entry
# script (the production vhost expects /home/ubuntu/html which is itself a
# symlink to canadarasp/web-server).
USER root
RUN rm -f /etc/apache2/sites-enabled/* \
    && a2enmod proxy proxy_http headers substitute filter >/dev/null
USER ubuntu

WORKDIR /home/ubuntu/canadarasp

# Each service supplies its own entrypoint via compose `command:`. No default
# CMD here — `docker compose run compute MODEL` should fail loudly if MODEL is
# missing rather than starting some surprising default service.
