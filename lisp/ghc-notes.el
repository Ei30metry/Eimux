;; ghc-notes.el --- Navigating ghc-style notes -*- lexical-binding: t; -*-
;; Copyright (C) 2025 by Artin Ghasivand

;; Maintainer: ghasivand.artin@gmail.com

;;; Commentary:

;; This package provides utility functions for navigating ghc-style Notes
;; in projecs

;;; Code:
(require 'project)

(defun ghc-notes-goto-note-at-point ()
  "Ripgrep a ghc style note at point."
  (interactive)
  (let* ((title-in-bracket (prin1-to-string (sexp-at-point)))
         (title-in-bracket-length (length title-in-bracket))
         (title (substring title-in-bracket 1 (- title-in-bracket-length 1))))
    (ghc-notes-goto-note title)))

(defun ghc-notes-goto-note (note-str)
  "Go to ghc style a ghc style note"
  (interactive "MNote name: ")
  (let* ((pattern (format "Note \\[%s\\]\n-*\s*~+" (regexp-quote note-str)))
         (default-directory (project-root (project-current)))
         (cmd (format "rg -nS --no-heading --multiline -e \"%s\"" pattern))
         (output (shell-command-to-string cmd)))
    (if (string-match "^\\([^:\n]+\\):\\([0-9]+\\):" output)
        (let ((file (match-string 1 output))
              (line (string-to-number (match-string 2 output))))
          (find-file (expand-file-name file default-directory))
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter-top-bottom))
      (message (format "No note found with title: %s" note-str)))))

;; TODO: Shouldn't list definitions
(defun list-ghc-style-note-references-in-buffer (buffer)
  (interactive "bBuffer: ")
  (with-current-buffer buffer
    (occur (rx-to-string '(seq "Note" (1+ blank) "[" (1+ (not (any "]"))) "]") t))))

;; TODO: Combine with list-ghc-style-notes-project
(defun ghc-notes-list-notes-current-buffer ()
  (interactive)
  (list-ghc-style-notes-in-buffer (current-buffer)))

(defun consult-ghc-notes-project ()
  "Search for ghc-style note definitions across the project with preview support."
  (interactive)
  (let* ((pattern "Note \\[.*\\]\n~+")
         (default-directory (project-root (project-current)))
         (cmd (format "rg -nS --no-heading --multiline -e \"%s\"" pattern))
         (output (shell-command-to-string cmd))
         candidates-alist)
    (dolist (line (split-string output "\n" t))
      (unless (string-match-p "^[^:\n]+:[0-9]+:[~]+$" line)
        (when (string-match "^\\([^:\n]+\\):\\([0-9]+\\):\\(.*\\)$" line)
          (let* ((file (match-string 1 line))
                 (linenum (string-to-number (match-string 2 line)))
                 (text  (string-clean-whitespace ((lambda (note-str)
                                                    (if (string= (substring note-str 0 2) "{-")
                                                        (substring note-str 3)
                                                      note-str))
                                                  (string-trim (match-string 3 line))))))
            (push (cons text (cons file linenum)) candidates-alist)))))
    (if candidates-alist
        (let ((selection (consult--read
                          (mapcar #'car candidates-alist)
                          :prompt "ghc Notes: "
                          :require-match t
                          :state
                          (lambda (_ cand)
                            (let* ((meta (cdr (assoc cand candidates-alist)))
                                   (file (car meta))
                                   (line (cdr meta)))
                              (when (and file line)
                                (find-file-other-window file)
                                (goto-char (point-min))
                                (forward-line (1- line))
                                (recenter))))))))
      (message "No ghc notes found."))))

(provide 'ghc-notes)
