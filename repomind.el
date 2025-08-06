;;; repomind.el --- Emacs configuration for RepoMind Scheme development

;; Project-specific Emacs configuration for RepoMind
;; Provides Scheme/Guile development environment with Geiser, ParEdit, etc.

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure required packages are installed
(defvar repomind-packages
  '(geiser
    geiser-guile
    paredit
    rainbow-delimiters
    company
    flycheck
    flycheck-guile
    org))

(dolist (package repomind-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; Load packages
(require 'geiser)
(require 'geiser-guile)
(require 'paredit)
(require 'rainbow-delimiters)
(require 'company)
(require 'flycheck)
(require 'org)
(require 'tramp)

;; Project configuration
(defvar repomind-root (or (getenv "PROJECT_ROOT")
                          (file-name-directory
                           (or load-file-name buffer-file-name))))

(defvar repomind-name (or (getenv "PROJECT_NAME") "repomind"))

;; Geiser configuration for Guile 3
(setq geiser-guile-binary "guile3")
(setq geiser-active-implementations '(guile))
(setq geiser-default-implementation 'guile)
(setq geiser-repl-history-filename "~/.geiser_history")
(setq geiser-repl-query-on-kill-p nil)
(setq geiser-repl-query-on-exit-p nil)

;; Add project paths to Guile load path
(setq geiser-guile-load-path
      (list (concat repomind-root "/src")
            (concat repomind-root "/experiments")
            (concat repomind-root "/tests")))

;; ParEdit configuration
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)

;; Enable ParEdit for Scheme modes
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

;; Rainbow delimiters for better paren matching
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; Company mode for autocompletion
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)

;; Flycheck for syntax checking
(add-hook 'scheme-mode-hook #'flycheck-mode)
(setq flycheck-scheme-chicken-executable "guile3")

;; Org mode configuration for literate programming
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

;; TRAMP configuration for remote development
(setq tramp-default-method "ssh")
(setq tramp-verbose 6)

;; Project-specific key bindings
(global-set-key (kbd "C-c C-z") 'geiser-mode-switch-to-repl)
(global-set-key (kbd "C-c C-k") 'geiser-compile-current-buffer)
(global-set-key (kbd "C-c C-d") 'geiser-doc-symbol-at-point)

;; File associations
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;; Custom functions for RepoMind development
(defun repomind-run-test ()
  "Run RepoMind tests."
  (interactive)
  (compile (format "cd %s && make test" repomind-root)))

(defun repomind-run-experiment (experiment)
  "Run a specific RepoMind experiment."
  (interactive "sExperiment number: ")
  (compile (format "cd %s && make experiment-%s" repomind-root experiment)))

(defun repomind-repl ()
  "Start RepoMind REPL."
  (interactive)
  (let ((default-directory repomind-root))
    (run-geiser 'guile)))

;; Key bindings for custom functions
(global-set-key (kbd "C-c r t") 'repomind-run-test)
(global-set-key (kbd "C-c r e") 'repomind-run-experiment)
(global-set-key (kbd "C-c r r") 'repomind-repl)

;; Display startup message
(message "RepoMind development environment loaded for %s at %s"
         repomind-name repomind-root)

(provide 'repomind)
;;; repomind.el ends here