;;; init-ruby.el -- ruby config
;;;
;;; Code: down below
;;;
;;; Commentary: ruby dependent configuration and packages

(use-package ruby-mode
  :ensure t
  :interpreter "ruby"

  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)

  :bind
  (([(meta down)] . ruby-forward-sexp)
   ([(meta up)]   . ruby-backward-sexp)
   (("C-c C-e"    . ruby-send-region))))

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(provide 'init-ruby)
;;; init-ruby.el ends here
