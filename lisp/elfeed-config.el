;;; init.el --- Description -*- lexical-binding: t; -*-

(use-package elfeed
   :straight t
   :commands (elfeed)
   :config
   (setq elfeed-db-directory "~/.elfeed"
         elfeed-feeds '(("https://planet.haskell.org/atom.xml" haskell PL)
                        ("https://haskell.pl-a.net/atom.xml" haskell PL)
                        ("http://www.masteringemacs.org/feed" emacs blog)
                        ("https://lexi-lambda.github.io/feeds/all.atom.xml" haskell lisp blog)
                        ("https://ianthehenry.com/feed.xml" blog)
                        ("https://coot.me/feed.rss" blog haskell)
                        ("https://www.haskellforall.com/feeds/posts/default" blog)
                        ("https://www.williamyaoh.com/feed.atom" haskell blog)
                        ("https://ekmett.github.io/reader/feed/index.html" haskell blog)
                        ("https://math.andrej.com/feed.xml" math logic CS PL blog)
                        ("https://www.michaelpj.com/blog/feed.xml" haskell blog)
                        ("https://rss.arxiv.org/atom/math.LO" math logic)
                        ("https://rss.arxiv.org/rss/cs.DS" CS DS)
                        ("https://rss.arxiv.org/rss/cs.LO" CS logic)
                        ("https://okmij.org/ftp/rss.xml" CS logic haskell ocaml)
                        ("https://rss.arxiv.org/rss/cs.DM" CS DM)
                        ("https://rss.arxiv.org/rss/cs.PL" CS PL)
                        ("https://rss.arxiv.org/rss/cs.FL" CS PL)
                        ("https://blog.haskell.org/atom.xml" haskell blog)
                        ("https://www.well-typed.com/blog/rss2.xml" haskell))))

(use-package elfeed-score :straight t)

(provide 'elfeed-config)
