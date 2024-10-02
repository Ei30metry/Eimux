;;; init.el --- Description -*- lexical-binding: t; -*-

(require 'telega)

(defun use-telega-fonts ()
 (interactive)
 (setq buffer-face-set '(:family "Dejavu Sans")))

(define-key telega-chat-mode-map (kbd "C-c C-p") 'telega-button-backward)
(define-key telega-chat-mode-map (kbd "C-c C-n") 'telega-button-forward)

(add-hook 'telega-chat-mode-hook 'use-telega-fonts)
(add-hook 'telega-root-mode-hook 'use-telega-fonts)

(setq telega-chat-bidi-display-reordering t
          telega-use-images t)

(provide 'telega-config)
