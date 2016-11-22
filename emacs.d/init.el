;  _____  _  _  __  __                   
; | ____|| |(_)|  \/  |  __ _   ___  ___ 
; |  _|  | || || |\/| | / _` | / __|/ __|
; | |___ | || || |  | || (_| || (__ \__ \
; |_____||_||_||_|  |_| \__,_| \___||___/

;; Setup package control
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
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
; For ansi-term n' stuff
(setq explicit-shell-file-name
      (if (file-readable-p "/usr/bin/zsh") "/usr/bin/zsh" "/bin/bash"))
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
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
;; Org Settings
(setq org-pretty-entities t) ; Alows org to displayed UTF-8 chars like \alpha
(setq org-startup-truncated nil)
(setq org-src-fontify-natively t)

;; Handy functions for starting and stopping dropbox (e.g. mostly used for Orgzly)
;; On the Linux lab computers at UTM, we have a disk quota so this is a hack fix :)
(defun eli/dropbox-start ()
  (interactive)
  (if (eq nil (file-exists-p "/virtual/mireckie/dropbox/.dropbox-dist"))
      (call-process-shell-command "(python ~/.emacs.d/dropbox.py start -i&)")
    (call-process-shell-command "(python ~/.emacs.d/dropbox.py start&)")))
(defun eli/dropbox-stop ()
  (interactive)
  (call-process-shell-command "python ~/.emacs.d/dropbox.py stop&"))

;; Disable the annoying audible bell
(setq visible-bell nil)

;; Theme
(load-theme 'spolsky t)
(set-face-attribute 'default nil :height 100)
;; When in terminal
(unless (display-graphic-p) 
  (setq nlinum-format "%d ")
  (add-to-list 'default-frame-alist '(background-color . "color-16"))
  (custom-set-faces
   '(linum ((t (:background "color-16" :foreground "#9fc59f"))))))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "dark green"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "firebrick"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange red"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "magenta")))))

; TODO: Get this to work :|
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))
;(format ":( [%s]" eshell-last-command-status)
;(setq eshell-prompt-function
;      (lambda ()
;        (format "%s $ "
;                (if (eq 0 eshell-last-command-status)
;                    (with-face "hi" :foreground "#0f0")
;                    (with-face "hi"
;                               :foreground "#f00")))))
;(setq eshell-prompt-regexp "^\(\:(\|:)\)\n[#$] ")

(defun eshell-bindings ()
	(define-key evil-normal-state-map (kbd "<up>")
      'eshell-previous-matching-input-from-input)
	(define-key evil-normal-state-map (kbd "<down>")
      'eshell-next-matching-input-from-input))

;; For leader bindings
(use-package general)
;; Base evil package
(use-package evil
  :demand
  :init
  ;; Unbind <C-u> for evil mode'
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)

  ;; Put the cursor in newly created panes
  ;;(setq evil-split-window-below t)
  ;;(setq evil-vsplit-window-right t)

  ;; Automatically opens ido after :e
  (define-key evil-ex-map "e " 'ido-find-file)
  (eshell-bindings)
  
  (general-create-definer bind-leader
                          :keymaps 'global
                          :states '(normal emacs)
                          :prefix "SPC")

  :general
  (:states 'motion
           "k" 'evil-previous-visual-line
           "j" 'evil-next-visual-line)
  (:states 'operator
           "k" 'evil-previous-line
           "j" 'evil-next-line)
  

  (:states 'normal
           "C-z"  (lambda () (interactive)  (when (eq (display-graphic-p) nil) (suspend-frame))))

  (bind-leader
    "w" 'save-buffer
    "k" 'kill-this-buffer
    "e" 'eshell
    "t" '(lambda () (interactive) (ansi-term "/usr/bin/zsh"))
    "g" 'magit-status
    "m" 'ido-switch-buffer))

;; Tpope's surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; External configuration for powerline and evil powerline
;; (~/.emacs.d/lisp/init-powerline.el)
(require 'init-powerline)

;; Nicer Completion
(use-package ido
    :init
    (defun my-ido-keys ()
        "Has tab cycle through the matches"
        (define-key ido-completion-map [tab]
          'ido-next-match)
        (define-key ido-completion-map (kbd "C-c C-k")
          'ido-kill-buffer-at-head))
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
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; Intelligently chooses between a space or hyphen when using ido
(use-package ido-complete-space-or-hyphen)

;; Git porcelen
(use-package magit)

;; Help with keys
(use-package which-key
  :config
  (which-key-mode))

;; Vim bindings for magit
(use-package evil-magit)

;; Vim bindings for org mode
(use-package evil-org
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

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

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config)

;; Backup options
(setq backup-by-copying t) ; Stop shinanigans with links
(setq backup-directory-alist '((".*" . "~/.bak.emacs/backup/")))
; Creates auto directory if it doesn't already exist
(if (eq nil (file-exists-p "~/.bak.emacs/"))
    (make-directory "~/.bak.emacs/"))
(if (eq nil (file-exists-p "~/.bak.emacs/auto"))
    (make-directory "~/.bak.emacs/auto"))
; Backup in one place. Flat, no tree structure
(setq auto-save-file-name-transforms '((".*" "~/.bak.emacs/auto/" t)))

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
 '(package-selected-packages
   (quote
    (markdown-mode org-bullets nlinum-relative evil-org evil-magit which-key magit ido-complete-space-or-hyphen smex ido-ubiquitous powerline-evil powerline evil-surround evil-leader evil rainbow-delimiters use-package))))
