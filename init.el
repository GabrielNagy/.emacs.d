;; Turn off mouse interface early in startup to avoid momentary display
(setq backup-directory-alist `(("." . "~/.saves")))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Turn off bell alarms
(setq ring-bell-function 'ignore)

;; Disable splash screen
(setq inhibit-startup-screen t)

(package-initialize)
;; package.el
(require 'package)
(setq package-enable-at-startup nil)

(setq recentf-max-saved-items 200)
(setq recentf-max-menu-items 200)

;; add lisp directry to path (recursive)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
	  (dir (nth 1 file)))
      (when (and dir
		 (not (string-suffix-p "." filename)))
	(add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")

;; Take out the trash
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package deadgrep
  :ensure t
  :bind ((("C-c d" . deadgrep))))

(use-package diminish
  :ensure t
  :defer t)

(use-package puppet-mode
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :defer 1)

(use-package pug-mode :ensure t
  :config
  (custom-set-variables '(pug-tab-width 2)))

(use-package dash :ensure t)

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package coffee-mode
  :ensure t
  :config
  (custom-set-variables '(coffee-tab-width 4)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)

  (defun org-keyword-backend (command &optional arg &rest ignored)
    "Company backend for org keywords.
COMMAND, ARG, IGNORED are the arguments required by the variable
`company-backends', which see."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (let ((p (company-grab-line "^#\\+\\(\\w*\\)" 1)))
                     (if p (cons p t)))))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  (add-to-list 'company-backends 'org-keyword-backend)

  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (global-set-key (kbd "<C-tab>") 'company-manual-begin)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package counsel :ensure t)

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . counsel-grep-or-swiper)
  :config
  (require 'counsel)
  (setq counsel-grep-base-command "grep -niE \"%s\" %s")
  (setq ivy-height 20))

(use-package dictionary :ensure t)

(use-package emmet-mode
  :ensure t
  :commands emmet-mode)

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "C-\\")  'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c '") 'fence-edit-code-at-point)
  (define-key markdown-mode-map (kbd "C-c 1") 'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2") 'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3") 'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4") 'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5") 'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6") 'markdown-insert-header-atx-6)

  (add-hook 'markdown-mode-hook (lambda ()
                                  (yas-minor-mode t)
                                  (set-fill-column 80)
                                  (turn-on-auto-fill)
                                  (flyspell-mode))))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
    (if (fboundp 'evil-normal-state)
	(evil-normal-state)))
  (ad-activate 'wgrep-change-to-wgrep-mode)

  (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
    (if (fboundp 'evil-motion-state)
	(evil-motion-state)))
  (ad-activate 'wgrep-finish-edit))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :commands (ag ag-project)
  :config
  (add-hook 'ag-mode-hook
	    (lambda ()
	      (wgrep-ag-setup)
	      (define-key ag-mode-map (kbd "n") 'evil-search-next)
	      (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (when (eq system-type 'darwin)
    (setq ag-executable "/usr/local/bin/ag"))
  (when (eq system-type 'gnu/linux)
    (setq ag-executable "ag"))
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package rg
  :ensure t
  :config
  (setq rg-group-result t))

(use-package clojure-mode
  :ensure t)

(use-package comment-dwim-2
  :ensure t)

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

(use-package sublime-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package zenburn-theme :ensure t :defer t)

(use-package mmm-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :defer 1
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
	(list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

(use-package writegood-mode
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (writegood-mode 0))))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package bm
  :ensure t
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)


  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("<left-fringe> <mouse-5>" . bm-next-mouse)
         ("<left-fringe> <mouse-4>" . bm-previous-mouse)
         ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
         ("C-<f2>" . bm-toggle))
  )

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))
(setq nxml-child-indent 4 nxml-attribute-indent 4)

(setq-default tab-width 4)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
	     :ensure t)
(load-theme 'sanityinc-tomorrow-eighties t)
;; (when (eq system-type 'gnu/linux)
  ;; (load-theme 'zenburn t))

(require 'init-platform)
(require 'init-global)
(require 'init-keybindings)
(require 'init-fonts)
(require 'init-powerline)
(require 'init-evil)
(require 'init-git)
(require 'init-org)
(require 'init-go)
(require 'init-python)
(require 'init-ruby)
(require 'init-cpp)
(require 'init-rss)

(use-package rust-mode
  :ensure t
  :config
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  (define-key rust-mode-map (kbd "C-c C-b") 'rust-compile)
  (define-key rust-mode-map (kbd "C-c C-t") 'rust-test))

(use-package lsp-mode
  :hook (prog-mode . lsp))

(use-package lsp-ui
  :ensure t)
(use-package company-lsp
  :ensure t)

(use-package crystal-mode
  :ensure t)

(use-package yafolding
  :ensure t
  :config
      (evil-define-key 'normal yafolding-mode-map
      (kbd "TAB") 'yafolding-toggle-element
      (kbd "SPC") 'yafolding-go-parent-element)
  :hook ((prog-mode . yafolding-mode)))

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-delay 0.02)
  (setq highlight-indent-guides-responsive 'top))

;; (use-package neotree
;;   :ensure t
;;   :bind (("C-x C-n" . neotree-toggle))
;;   :defer
;;   :config
;;   (setq neo-window-fixed-size nil)
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;     (evil-set-initial-state 'neotree-mode 'normal)
;;     (evil-define-key 'normal neotree-mode-map
;;       (kbd "RET") 'neotree-enter
;;       (kbd "TAB") 'neotree-quick-look
;;       (kbd "c")   'neotree-create-node
;;       (kbd "r")   'neotree-rename-node
;;       (kbd "d")   'neotree-delete-node
;;       (kbd "j")   'neotree-next-line
;;       (kbd "k")   'neotree-previous-line
;;       (kbd "g")   'neotree-refresh
;;       (kbd "C")   'neotree-change-root
;;       (kbd "I")   'neotree-hidden-file-toggle
;;       (kbd "H")   'neotree-hidden-file-toggle
;;       (kbd "q")   'neotree-hide
;;       (kbd "l")   'neotree-enter)
;;     :init
;;     ;; Set the neo-window-width to the current width of the
;;     ;; neotree window, to trick neotree into resetting the
;;     ;; width back to the actual window width.
;;     ;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
;;     (eval-after-load "neotree"
;;       '(add-to-list 'window-size-change-functions
;;                     (lambda (frame)
;;                       (let ((neo-window (neo-global--get-window)))
;;                         (unless (null neo-window)
;;                           (setq neo-window-width (window-width neo-window))))))))

;;; The Emacs Shell
(defun company-eshell-history (command &optional arg &rest ignored)
  "Complete from shell history when starting a new line.

Provide COMMAND and ARG in keeping with the Company Mode backend spec.
The IGNORED argument is... Ignored."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill term buffer when term is ended."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)
;; Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "gcc " (buffer-name) " && ./a.out" ))
  (shell-command foo))

(global-set-key [C-f1] 'execute-c-program)
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))
;; fails on ubuntu

(cua-selection-mode 1)
(setq shift-select-mode 1)
(setq delete-selection-mode 1)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq scroll-conservatively most-positive-fixnum)
