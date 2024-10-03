(use-package helpful
  :straight t
  :demand t
  :bind
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable)
  ("C-h f" . helpful-callable)
  ("C-h x" . helpful-command)
  ("C-h ." . helpful-at-point)
  ("C-h q" . helpful-kill-buffers))

(global-set-key (kbd "C-c C-.") 'eldoc)

(provide 'docs-config)
