;;; init-go.el -- make emacs work nice with golang
;;;
;;; Code: down below
;;;
;;; Commentary: golang dependent configuration and packages

(add-to-list 'exec-path "/usr/local/go/bin")
(add-to-list 'exec-path (expand-file-name "~/.local/go/bin"))
(setenv "GOPATH" (expand-file-name "~/.local/go"))

(use-package go-playground
  :ensure t)

(use-package gorepl-mode
  :ensure t)

(use-package go-projectile
  :ensure t)

(use-package go-mode
  :requires (go-playground go-guru godoctor go-errcheck go-eldoc gorepl-mode go-projectile)
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  )

(use-package go-direx
  :ensure t
  :config
  (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer))

(use-package go-guru
  :ensure t)

(defun my-go-mode-hook ()
  (company-mode 1)
  (yas-minor-mode-on)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "C-c C-c") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg
  (local-set-key (kbd "<C-return>") 'go-run))

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

(provide 'init-go)
;;; init-go.el ends here
