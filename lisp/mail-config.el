;;; init.el --- Description -*- lexical-binding: t; -*-

(setq user-full-name "Artin Ghasivand"
      user-mail-address "ghasivand.artin@gmail.com"
      message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      message-sendmail-envelope-from 'header
      message-kill-buffer-on-exit t
      message-auto-save-directory "~/.mail-drafts"
      message-kill-buffer-on-exit t)

(add-hook 'message-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'message-mode-hook (lambda () (jinx-mode 1)))

(add-hook 'notmuch-message-mode-hook
          (lambda () (auto-save-mode -1)))

(add-hook 'message-mode-hook
          (lambda () (auto-save-mode -1)))

(dolist (x '(message-mode-hook notmuch-message-mode-hook))
  (add-hook x (lambda () (auto-fill-mode -1))))


(use-package notmuch
  :straight t
  :demand t
  :bind
  ("C-x m" . notmuch)
  ("C-x M" . notmuch-mua-mail))

(use-package notmuch-addr
  :straight t)

(provide 'mail-config)
