# kakuro-elisp
Kakuro solver

## Dependencies
Uses emacs 24.5, dash.el

## Lexical scope
(setq lexical-binding t)

## Test
emacs -batch -l ert -l kakuro-tests.el -f ert-run-tests-batch-and-exit

