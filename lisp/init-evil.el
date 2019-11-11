;;; init-evil.el --- Custom evil mode configuration.
;;; Commentary: for flycheck purposes
;;; Code: below

(defun g--config-evil-leader ()
  "Configure evil leader mode"
  (evil-leader/set-leader ",")
  (evil-leader/set-key
	"," 'other-window
	"." 'mode-line-other-buffer
	":" 'eval-expression
    "b" 'helm-mini
    "d" 'lsp-find-definition
    "B" 'magit-blame-toggle
    "c" 'comment-dwim-2
    "e" 'flymake-show-diagnostics-buffer
    "f" 'helm-imenu
    "g" 'magit-status
    "l" 'whitespace-mode
    "L" 'delete-trailing-whitespace
    "o" 'delete-other-windows
    "p" 'helm-show-kill-ring
    "r" 'lsp-find-references
    "s" 'rg-project
    "S" 'rg-dwim
    "t" 'gtags-reindex
    "w" 'save-buffer
    "x" 'helm-M-x)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
        (call-interactively 'magit-blame))))

(defun g--config-evil ()
  "Configure evil mode"

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  custom-mode
                  custom-new-theme-mode
                  dired-mode
                  eshell-mode
                  git-rebase-mode
                  special-mode
                  org-capture-mode
                  sunshine-mode
				  magit-blame-mode
                  bm-show-mode
                  elfeed-search-mode
                  elfeed-show-mode
                  nov-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these modes
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")		'evil-search-forward
    (kbd "n")		'evil-search-next
    (kbd "N")		'evil-search-previous
    (kbd "C-w C-w")	'other-window)

  ;; Global bindings
  (evil-define-key 'normal global-map (kbd "C-p")	'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p")	'helm-projectile-switch-project)
  (evil-define-key 'normal global-map (kbd "C-`")	(lambda ()
                                                      (interactive)
                                                      (dired (expand-file-name "~"))))

  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  ;; Try to quit everything with escape
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package evil
  :ensure t
  :init
  (setq evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        evil-symbol-word-search t
        shift-select-mode nil
        evil-visual-state-cursor 'hollow)
  :commands (evil-mode evil-define-key)
  :config
  (add-hook 'evil-mode-hook 'g--config-evil)
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (g--config-evil-leader))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))

(provide 'init-evil)
;;; init-evil.el ends here
