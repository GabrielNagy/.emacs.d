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
       ("https://worthdoingbadly.com/feed.xml" cs linux)
	   ("https://possiblywrong.wordpress.com/feed" cs math)
	   ("https://hnrss.org/frontpage?points=100" news)
	   ("http://around.com/feed" science)
	   ("https://apod.nasa.gov/apod.rss" news)
       ("https://astr0baby.wordpress.com/feed/" linux qemu)
       ("https://xkcd.com/rss.xml" webcomics)
       ("http://pragmaticemacs.com/feed/" emacs)
       ("http://sachachua.com/blog/feed/" emacs)
       ("https://mdk.fr/feeds/all.atom.xml" emacs cs)
       ("http://planet.emacsen.org/atom.xml" emacs)))


(provide 'init-rss)
;;; init-rss.el ends here
