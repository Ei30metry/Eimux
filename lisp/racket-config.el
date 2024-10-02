;;; racket-config.el --- Description -*- lexical-binding: t; -*-
(use-package racket-mode
  :straight t
  :demand t
  :hook
  (racket-mode . smartparens-strict-mode)
  (racket-repl-mode . smartparens-strict-mode)
  (racket-mode . racket-xp-mode)
  :bind
  (:map racket-mode-map
   ("C-M-u" . sp-forward-slurp-sexp) ;; TODO This is horrible. Study keymaps.
   ("C-M-y" . sp-forward-barf-sexp))
  (:map racket-repl-mode-map
   ("C-M-u" . sp-forward-slurp-sexp)
   ("C-M-y" . sp-forward-barf-sexp)))

(provide 'racket-config)
