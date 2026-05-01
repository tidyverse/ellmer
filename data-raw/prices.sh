#!/usr/bin/env bash
# Update data-raw/prices.json from litellm's upstream pricing data.
#
# Usage: bash data-raw/prices.sh
#
# Requires: curl, jq, uv (https://docs.astral.sh/uv/)

set -euo pipefail

LITELLM_URL="https://raw.githubusercontent.com/BerriAI/litellm/refs/heads/main/model_prices_and_context_window.json"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT="$SCRIPT_DIR/prices.json"
SCHEMA="$SCRIPT_DIR/prices.schema.json"
FILTER="$SCRIPT_DIR/prices.jq"

# --- dependencies -----------------------------------------------------------

for cmd in curl jq uv; do
  if ! command -v "$cmd" &>/dev/null; then
    echo "error: '$cmd' not found on PATH" >&2
    [[ "$cmd" == "uv" ]] && echo "  install: https://docs.astral.sh/uv/" >&2
    exit 1
  fi
done

# --- fetch & transform -------------------------------------------------------

echo "Fetching litellm prices..."
raw=$(curl -fsSL "$LITELLM_URL")

echo "Transforming..."
transformed=$(echo "$raw" | jq -f "$FILTER")

# --- sanity checks -----------------------------------------------------------

row_count=$(echo "$transformed" | jq 'length')
if [[ "$row_count" -lt 500 ]]; then
  echo "error: only $row_count rows after transform (expected >= 500); aborting" >&2
  exit 1
fi
echo "  rows: $row_count"

provider_count=$(echo "$transformed" | jq '[.[].provider] | unique | length')
if [[ "$provider_count" -lt 8 ]]; then
  echo "error: only $provider_count providers (expected 8); aborting" >&2
  exit 1
fi
echo "  providers: $provider_count"

# --- schema validation -------------------------------------------------------

echo "Validating schema..."
tmp=$(mktemp /tmp/prices_XXXXXX.json)
trap 'rm -f "$tmp"' EXIT
echo "$transformed" > "$tmp"

if ! uvx check-jsonschema --schemafile "$SCHEMA" "$tmp"; then
  echo "error: schema validation failed" >&2
  exit 1
fi

# --- write -------------------------------------------------------------------

echo "$transformed" > "$OUT"
echo "Prices updated: $OUT ($row_count rows)"
