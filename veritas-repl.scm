(module veritas-repl ()
  (import scheme chicken)
  (require-library veritas veritas-verifiers veritas-base-reporter veritas-repl-reporter)
  (reexport veritas-base-reporter veritas veritas-verifiers veritas-repl-reporter))
