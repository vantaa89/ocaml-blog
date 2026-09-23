#!/usr/bin/env bash
set -Eeuo pipefail

APP_DIR="/home/opc/ocaml-blog"
SERVICE="blog"
PROFILE="release"

log() {
  printf '[deploy] %s\n' "$1"
}

cd "$APP_DIR"

if [[ ! -f owner_profile.sexp ]]; then
  log "owner_profile.sexp is missing; the build embeds it, so create it first"
  exit 1
fi

log "install dependencies"
opam install --deps-only --yes .

log "build (profile: ${PROFILE})"
opam exec -- dune build --profile "$PROFILE"

log "create any missing tables"
"$APP_DIR/_build/default/bin/main.exe" create-tables

log "restart service: ${SERVICE}"
sudo systemctl restart "$SERVICE"
sudo systemctl is-active --quiet "$SERVICE"

log "deployment completed"
