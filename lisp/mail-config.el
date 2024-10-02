;;; init.el --- Description -*- lexical-binding: t; -*-

(setq user-full-name "Artin Ghasivand"
      user-mail-address "ghasivand.artin@gmail.com"
      message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      message-sendmail-envelope-from 'header
      message-kill-buffer-on-exit t)

(add-hook 'message-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'message-mode-hook (lambda () (jinx-mode 1)))
(setq message-auto-save-directory "~/.mail/drafts"
      message-kill-buffer-on-exit t)

(defun fetch-mail-and-refresh ()
  (interactive)
  (shell-command "mbsync -aV")
  (notmuch-poll))

(use-package notmuch
  :straight t
  :demand t
  :bind
  ("C-x m" . notmuch)
  ("C-x M" . notmuch-mua-mail)
  (:map notmuch-hello-mode-map
        ("F" . fetch-mail-and-refresh)))

(use-package notmuch-addr
  :straight t)

(provide 'mail-config)
