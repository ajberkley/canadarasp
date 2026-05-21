#!/bin/bash
# Web service entrypoint. Starts apache2 (static + reverse proxy), the
# Hunchentoot dynamic-windgram service on :8090, and the SBCL/Java timezone
# service on :8082. Each background service runs in a `while true; do ...; sleep
# 1; done` loop so a crash restarts it (mirrors production's start-webserver.sh
# and run-timezone-server.sh).
set -e

REPO=/home/ubuntu/canadarasp

# Install the vhost. DocumentRoot points directly at the bind-mounted source
# and Alias maps /map-pngs etc. into /mnt — see docker/web-vhost.conf.
# Templates $GMAPS_API_KEY in if set; otherwise leaves the substitution as
# a no-op (the original canadarasp.com-restricted key stays in the HTML).
if [ -n "$GMAPS_API_KEY" ]; then
    sudo sed "s|__GMAPS_API_KEY__|$GMAPS_API_KEY|" \
        "$REPO/docker/web-vhost.conf" \
        | sudo tee /etc/apache2/sites-enabled/000-default.conf >/dev/null
    echo "[web] Google Maps key wired into Apache mod_substitute"
else
    sudo cp "$REPO/docker/web-vhost.conf" /etc/apache2/sites-enabled/000-default.conf
    echo "[web] note: \$GMAPS_API_KEY unset — base map will fail to render in browser"
fi

# Apache in the foreground would be ideal but we have two extra long-lived
# Lisp services to babysit. Run apache via apachectl and tail logs.
sudo mkdir -p /var/log/apache2
sudo apache2ctl start

# Dynamic windgram service (Hunchentoot, port 8090). Loops to auto-restart.
(
    cd "$REPO/continental-test/plot-generation"
    while true; do
        ./generate-windgram-on-demand.lisp || true
        echo "[web] windgram service exited, restarting in 1s"
        sleep 1
    done
) &

# Timezone service (Hunchentoot wrapping a Java helper, port 8082).
(
    while true; do
        sbcl --load /home/ubuntu/quicklisp/setup.lisp \
             --script "$REPO/web-server/timezone/timezone.lisp" || true
        echo "[web] timezone service exited, restarting in 1s"
        sleep 1
    done
) &

# Tail apache logs in the foreground so `docker compose logs web` is useful
# and the container stays alive even if the lisp services die between
# restarts. `exec` so signals reach tail (and apache via its parent).
sudo touch /var/log/apache2/access.log /var/log/apache2/error.log
exec sudo tail -F /var/log/apache2/access.log /var/log/apache2/error.log
