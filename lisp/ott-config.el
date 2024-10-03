;;; ott-config.el --- Description -*- lexical-binding: t; -*-

(use-package ott-mode
    :demand t
    :hook
    (ott-mode . smartparens-mode)
    (ott-mode . (lambda () (display-line-numbers-mode 1))))

(with-eval-after-load 'compile
  (push 'ott compilation-error-regexp-alist)
  (push '(ott "File \\([a-zA-Z0-9/\\._-]+\\) on line \\([0-9]+\\).*$" 1 2 nil) compilation-error-regexp-alist-alist))

(provide 'ott-config)
