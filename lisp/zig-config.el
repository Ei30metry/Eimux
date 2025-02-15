(use-package zig-mode
  :straight t
  :bind
  (:map zig-mode-map
        ("C-c C-c" . zig-compile)
        ("C-c C-u" . zig-insert-undefined)))

(defun zig-insert-undefined ()
  (interactive)
  (insert "undefined"))


(provide 'zig-config)
