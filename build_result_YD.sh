#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$ROOT_DIR/Result_SYT"
DST_DIR="$ROOT_DIR/result_YD"

mkdir -p "$DST_DIR"

# Start from a clean grouped-output directory on each run.
find "$DST_DIR" -maxdepth 1 -type f -name '*.csv' -delete

csv_escape() {
  local value="${1//\"/\"\"}"
  printf '%s' "$value"
}

append_grouped_row() {
  local yd="$1"
  local tableau="$2"
  local parent_tableau="$3"
  local bethe_roots="$4"
  local source_file="$5"
  local succeeded_q="$6"
  local failure_reason="$7"
  local timing_seconds="$8"
  local yd_filename out_file

  yd_filename="${yd// /}"
  out_file="$DST_DIR/$yd_filename.csv"

  if [[ ! -f "$out_file" ]]; then
    printf '"Tableau","YoungDiagram","ParentTableau","SucceededQ","FailureReason","TimingSeconds","BetheRoots","SourceFile"\n' > "$out_file"
  fi

  printf '"%s","%s","%s","%s","%s","%s","%s","%s"\n' \
    "$(csv_escape "$tableau")" \
    "$(csv_escape "$yd")" \
    "$(csv_escape "$parent_tableau")" \
    "$(csv_escape "$succeeded_q")" \
    "$(csv_escape "$failure_reason")" \
    "$(csv_escape "$timing_seconds")" \
    "$(csv_escape "$bethe_roots")" \
    "$(csv_escape "$(basename "$source_file")")" >> "$out_file"
}

parse_final_row() {
  local row="$1"
  printf '%s\n' "$row" | sed -n 's/^"[^"]*","\([^"]*\)","\([^"]*\)",\([^,]*\),"\([^"]*\)",\([^,]*\),"\([^"]*\)"$/\1\n\2\n\3\n\4\n\5\n\6/p'
}

parse_subsyt_row() {
  local row="$1"
  printf '%s\n' "$row" | sed -n 's/^"\([^"]*\)","\([^"]*\)","\([^"]*\)","\([^"]*\)"$/\1\n\2\n\3\n\4/p'
}

declare -A FINAL_SUCCEEDED FINAL_FAILURE FINAL_TIMING

while IFS= read -r -d '' file; do
  header="$(sed -n '1p' "$file")"
  row="$(sed -n '2p' "$file")"

  [[ -n "$row" ]] || continue

  if [[ "$header" == '"RunID","Tableau","YoungDiagram","SucceededQ","FailureReason","TimingSeconds","BetheRoots"' ]]; then
    mapfile -t parsed < <(parse_final_row "$row")
    [[ "${#parsed[@]}" -eq 6 ]] || continue
    FINAL_SUCCEEDED["${parsed[0]}"]="${parsed[2]}"
    FINAL_FAILURE["${parsed[0]}"]="${parsed[3]}"
    FINAL_TIMING["${parsed[0]}"]="${parsed[4]}"
  fi
done < <(find "$SRC_DIR" -maxdepth 1 -type f -name '*.csv' -print0 | sort -z)

while IFS= read -r -d '' file; do
  header="$(sed -n '1p' "$file")"
  row="$(sed -n '2p' "$file")"

  [[ -n "$row" ]] || continue

  case "$header" in
    '"RunID","Tableau","YoungDiagram","SucceededQ","FailureReason","TimingSeconds","BetheRoots"')
      mapfile -t parsed < <(parse_final_row "$row")
      [[ "${#parsed[@]}" -eq 6 ]] || {
        printf 'Skipping unparsable final-row file: %s\n' "$file" >&2
        continue
      }
      tableau="${parsed[0]}"
      yd="${parsed[1]}"
      succeeded_q="${parsed[2]}"
      failure_reason="${parsed[3]}"
      timing_seconds="${parsed[4]}"
      bethe_roots="${parsed[5]}"
      parent_tableau=""
      ;;
    '"Tableau","YoungDiagram","ParentTableau","BetheRoots"')
      mapfile -t parsed < <(parse_subsyt_row "$row")
      [[ "${#parsed[@]}" -eq 4 ]] || {
        printf 'Skipping unparsable sub-SYT file: %s\n' "$file" >&2
        continue
      }
      tableau="${parsed[0]}"
      yd="${parsed[1]}"
      parent_tableau="${parsed[2]}"
      bethe_roots="${parsed[3]}"
      succeeded_q="${FINAL_SUCCEEDED[$parent_tableau]:-true}"
      failure_reason="${FINAL_FAILURE[$parent_tableau]:-None}"
      timing_seconds="${FINAL_TIMING[$parent_tableau]:-}"
      ;;
    *)
      printf 'Skipping unknown CSV format: %s\n' "$file" >&2
      continue
      ;;
  esac

  append_grouped_row \
    "$yd" \
    "$tableau" \
    "$parent_tableau" \
    "$bethe_roots" \
    "$file" \
    "$succeeded_q" \
    "$failure_reason" \
    "$timing_seconds"
done < <(find "$SRC_DIR" -maxdepth 1 -type f -name '*.csv' -print0 | sort -z)

printf 'Grouped YD files written to %s\n' "$DST_DIR"
