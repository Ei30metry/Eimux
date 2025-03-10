;;; media-config.el --- Description -*- lexical-binding: t; -*-

(global-unset-key (kbd "C-x C-v"))

(use-package emms
  :straight t
  :demand t
  :config
  (require 'emms-setup)
  (require 'podcast-utils)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions
          '(emms-info-native
            emms-info-metaflac
            emms-info-ogginfo)
        emms-add-directory
          '("/Users/artin/.telega/cache/music/")
        emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128)
   :bind
   ("C-x C-v b" . emms-browser)
   ("C-x C-v p" . emms-pause)
   ("C-x C-v s" . emms-seek)
   ("C-x C-v C-v" . podcast-utils/open-write-note)
   ("C-x C-v C-o" . podcast-utils/open-episode-buffer))

(use-package empv
  :straight t
  :config
  (setq empv-invidious-instance "https://inv.nadeko.net/")
  (with-eval-after-load 'embark (empv-embark-initialize-extra-actions))
  (add-to-list 'empv-mpv-args "--ytdl-format=bestvideo+bestaudio/best[ext=mp4]/best"))

(provide 'media-config)
