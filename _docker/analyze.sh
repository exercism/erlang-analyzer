#!/usr/bin/env bash

set -o nounset

exec /opt/analyzer/bin/erlang_analyzer $1 $2
