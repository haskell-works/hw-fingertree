#!/usr/bin/env bash

STACK_FLAGS="
"

case $1 in
  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks \
      $STACK_FLAGS
    ;;

  test)
    stack test \
      $STACK_FLAGS
    ;;

  bench)
    stack bench \
      $STACK_FLAGS
    ;;

  repl)
    stack repl \
      $STACK_FLAGS
    ;;
esac
