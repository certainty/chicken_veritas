(module veritas-console ()
  (import chicken scheme)
  (require-library veritas veritas-verifiers veritas-base-reporter veritas-console-reporter)
  (reexport veritas veritas-verifiers veritas-base-reporter veritas-console-reporter))
