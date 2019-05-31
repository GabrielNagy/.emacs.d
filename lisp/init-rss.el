;;; init-rss.el -- RSS setup
;;;
;;; Code: down below
;;;
;;; Commentary: elfeed and related packages

(use-package elfeed
  :defer t
  :commands (elfeed)
  :bind ("C-c f" . elfeed)
  :ensure t
  :config
  (use-package elfeed-protocol
    :ensure t
    :config
    (elfeed-protocol-enable)
    (use-package password-store
      :ensure t))
  (use-package elfeed-goodies
    :ensure t
    :config (elfeed-goodies/setup)))

(setq elfeed-feeds
      '(("http://nullprogram.com/feed/" cs)
       ("https://jvns.ca/atom.xml" cs linux)
       ("https://zwischenzugs.com/feed/" cs linux)
       ("http://nullprogram.com/feed/" emacs cs linux)
       ("https://xkcd.com/rss.xml" webcomics)
       ("http://angad19.github.io/blog/feed.xml" emacs cs)
       ("http://cestlaz.github.io/rss.xml" emacs education cs)
       ("http://pragmaticemacs.com/feed/" emacs)
       ("http://sachachua.com/blog/feed/" emacs)
       ("https://mdk.fr/feeds/all.atom.xml" emacs cs)
       ("http://emacsblog.org/feed/" emacs)
       ("https://kieranhealy.org/index.xml" emacs sociology)
       ("http://planet.emacsen.org/atom.xml" emacs)))


(provide 'init-rss)
;;; init-rss.el ends here
