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

(mapcar (lambda (fun) (add-hook 'message-mode-hook fun))
        (list (lambda () (display-line-numbers-mode 1))
              (lambda () (auto-save-mode 1))
              (lambda () (jinx-mode 1))))

(mapcar (lambda (fun) (add-hook 'notmuch-message-mode-hook fun))
        (list (lambda () (display-line-numbers-mode 1))
              (lambda () (auto-save-mode 1))
              (lambda () (jinx-mode 1))))

(defun artin/lieer-sync ()
  "Sync Gmail using lieer"
  (interactive)
  (let ((default-directory "~/.mail/account.gmail"))
    (start-process-shell-command
     "lieer-sync"
     "*lieer-sync*"
     "gmi sync")))

(defun artin/notmuch-mua-empty-subject-check ()
  "Request confirmation before sending a message with empty subject"
  (when (and (null (message-field-value "Subject"))
             (not (y-or-n-p "Subject is empty, send anyway? ")))
    (error "Sending message cancelled: empty subject.")))

(use-package notmuch
  :straight t
  :demand t
  :bind
  ("C-x m" . notmuch)
  :hook
  (message-send      . artin/notmuch-mua-empty-subject-check)
  (notmuch-show-mode . visual-line-mode)
  :custom
  (notmuch-init-file (expand-file-name "~/.emacs.d/lisp/mail-config.el"))
  (notmuch-fcc-dirs nil)
  (notmuch-show-logo nil)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-thousands-separator "")
  (notmuch-show-all-tags-list t)
  (notmuch-search-oldest-first nil)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-message-headers-visible t)
  (notmuch-show-empty-saved-searches t)
  (notmuch-show-relative-dates t)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-indent-messages-width 0)
  (notmuch-search-result-format
   '(("date" . "%12s ")
     ("count" . "%-7s ")
     ("authors" . "%-20s ")
     ("subject" . "%s "))))

(use-package ol-notmuch :straight t)

(use-package notmuch-addr
  :straight t)

(provide 'mail-config)
