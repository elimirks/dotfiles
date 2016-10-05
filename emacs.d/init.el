;  _____  _  _  __  __                   
; | ____|| |(_)|  \/  |  __ _   ___  ___ 
; |  _|  | || || |\/| | / _` | / __|/ __|
; | |___ | || || |  | || (_| || (__ \__ \
; |_____||_||_||_|  |_| \__,_| \___||___/

;; Setup package control
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Specifies local directory to load packages from
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; Always ensure package is downloaded

;; Essential Settings
(setq inhibit-splash-screen t ; No welcome screen
    inhibit-startup-message t
    inhibit-startup-echo-area-message t)
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; Hide scrollbars
(menu-bar-mode -1) ; Hide menu bar
(show-paren-mode t) ; Highlights matching parenthesis
;(electric-pair-mode t) ; Add closing pairs automatically
(setq initial-scratch-message "") ; No scratch text
(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
; No tabs use spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Org Settings
(setq org-pretty-entities t) ; Alows org to displayed UTF-8 chars like \alpha

;; Theme
(load-theme 'spolsky)

;; Base evil package
(use-package evil
  :init
  ;; Unbind <C-u> for evil mode'
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  ;; Move up and down through wrapped lines
  ;(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  ;(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
	; Put the cursor in newly created panes
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
	; Automatically opens ido after :e
  (define-key evil-ex-map "e " 'ido-find-file))

;; evil leader key
(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>") ; Bind leader to space
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-key
	  ; Set leader bindings here
    "w" 'save-buffer
    "k" 'kill-this-buffer
    "e" 'eshell
    "m" 'ido-switch-buffer))

;; Tpope's surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; External configuration for powerline and evil powerline (~/.emacs.d/lisp/init-powerline.el)
(require 'init-powerline)

;; Nicer Completion
(use-package ido
    :init
    (defun my-ido-keys ()
        "Has tab cycle through the matches"
        (define-key ido-completion-map [tab] 'ido-next-match))
    (add-hook 'ido-setup-hook #'my-ido-keys)
    :config
    (setq ido-enable-flex-matching t) ; Similar to fuzzy matching
    (setq ido-everywhere t)
    (ido-mode 1))

;; Use ido everywhere possible
(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

;; Nicer Completion in M-x
(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)) ;; This is your old M-x.

;; Intelligently chooses between a space or hyphen when using ido
(use-package ido-complete-space-or-hyphen)

;; Git porcelen
(use-package magit
  :config
  (global-set-key "\C-x\g" 'magit-status))

;; Vim bindings for magit
(use-package evil-magit)

;; Vim bindings for org mode
(use-package evil-org)

;; Relative line numbers n' stuff
(use-package nlinum-relative
  :config
  (setq nlinum-relative-redisplay-delay 0)
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; Better looking org headers
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Backup options
(setq backup-by-copying t) ; Stop shinanigans with links
(setq backup-directory-alist '((".*" . "~/.bak.emacs/backup/")))
(if (eq nil (file-exists-p "~/.bak.emacs/")) ; Creates auto directory if it doesn't already exist
    (make-directory "~/.bak.emacs/"))
(if (eq nil (file-exists-p "~/.bak.emacs/auto")) ; Creates auto directory if it doesn't already exist
    (make-directory "~/.bak.emacs/auto"))
(setq auto-save-file-name-transforms '((".*" "~/.bak.emacs/auto/" t))) ; backup in one place. flat, no tree structure

;; esc quits like vim - nice evil mode stuff
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d1177a530659bb01a72d05e5194e9ce7f08fffa58ba1220ea351e1f5492d9882" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
