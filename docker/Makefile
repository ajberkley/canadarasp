# Convenience wrappers around docker compose for the local canadarasp stack.
# Run from this directory: `cd docker && make <target>`.

MODEL ?= gdps

.PHONY: help build up down logs ps web compute download smoke clean nuke status

help:  ## Show available targets
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "  %-12s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build:  ## Build the canadarasp:local image
	docker compose build

up:  ## Start the web service in the background (alias: web)
	docker compose up -d web

web: up

down:  ## Stop and remove the web container (keeps the mnt volume)
	docker compose down

ps:  ## Show running containers
	docker compose ps

status:  ## Show what data the shared volume contains
	@docker compose run --rm --no-deps --entrypoint bash compute -c ' \
		echo "=== /mnt usage ===" ; \
		du -sh /mnt/* 2>/dev/null | sort -hr ; \
		echo ; \
		echo "=== download cycles ===" ; \
		for m in gdps hrdps hrdps_rot ; do \
			[ -d /mnt/download-box/$$m ] && echo "  $$m: $$(ls /mnt/download-box/$$m 2>/dev/null | grep -v latest | wc -l) cycles, latest=$$(readlink /mnt/download-box/$$m/latest 2>/dev/null | xargs -n1 basename 2>/dev/null || echo none)" ; \
		done ; \
		echo ; \
		echo "=== windgram PNG counts ===" ; \
		find /mnt/windgrams-data -name "*.png" 2>/dev/null | awk -F/ "{print \$$5}" | sort | uniq -c | head ; \
		echo ; \
		echo "=== map PNG counts ===" ; \
		for m in gdps hrdps ; do \
			[ -d /mnt/map-pngs/$$m ] && echo "  $$m: $$(find /mnt/map-pngs/$$m -name '*.png' 2>/dev/null | wc -l) PNGs across $$(ls /mnt/map-pngs/$$m | wc -l) cycles" ; \
		done'

logs:  ## Tail web service logs
	docker compose logs -f web

download:  ## Download a model cycle from ECCC. Usage: make download MODEL=gdps
	docker compose run --rm download $(MODEL)

compute:  ## Run the pipeline for a model. Usage: make compute MODEL=gdps [LIMIT=N]
	docker compose run --rm $(if $(LIMIT),-e WINDGRAM_LIMIT=$(LIMIT)) compute $(MODEL)

smoke:  ## Run compute in smoke mode (only generates new variables). Usage: make smoke MODEL=gdps
	docker compose run --rm compute $(MODEL) smoke

verify:  ## Fast end-to-end: download + cap windgrams to 3 + compute. Usage: make verify MODEL=gdps
	docker compose run --rm download $(MODEL)
	docker compose run --rm -e WINDGRAM_LIMIT=3 compute $(MODEL)

shell:  ## Open a shell in a one-shot compute container
	docker compose run --rm --no-deps --entrypoint bash compute

clean:  ## Remove the mnt volume (resets all generated data)
	docker compose down -v

nuke:  ## clean + remove the image (next `make build` is from scratch)
	docker compose down -v --rmi local
