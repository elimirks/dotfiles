;;
;; Ivy Minibuffer
;;
(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<tab>" . ivy-next-line)
   ("<backtab>" . ivy-previous-line)
   ("C-n" . ivy-next-line)
   ("C-p" . ivy-previous-line)
   ("RET" . ivy-alt-done))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 20))

(use-package counsel)
(use-package smex)


;;
;; Compnay (autocompletion)
;;

(require 'color)
(use-package company
  :config
  (global-company-mode t)
  (setq company-idle-delay 0) ; Delay to complete
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-global-modes '(not org-mode)) ; Disable in org

  ;; Style nicely
  (let* ((bg (face-attribute 'default :background))
         (bg-light (color-lighten-name bg 2))
         (bg-lighter (color-lighten-name bg 5))
         (bg-lightest (color-lighten-name bg 10))
         (ac (face-attribute 'match :foreground)))
    (custom-set-faces
     `(company-tooltip
       ((t (:inherit default :background ,bg-light))))
     `(company-scrollbar-bg ((t (:background ,bg-lightest))))
     `(company-scrollbar-fg ((t (:background ,bg-lighter))))
     `(company-tooltip-selection
       ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common
       ((t (:inherit font-lock-constant-face))))
     `(company-preview-common
       ((t (:foreground ,ac :background ,bg-lightest))))))

  ;; Workaround for using company with FCI mode

  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook
            'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook
            'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook
            'company-maybe-turn-on-fci)

  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort)))

;;
;; Flycheck, for syntax checker
;;

(use-package flycheck
  :config
  (global-flycheck-mode t))

;;
;; Magit, a magically git plugin
;;

(use-package magit)
;; Vim bindings for magit
(use-package evil-magit)

;;
;; Misc stuff
;;

;; Shows help dialogs for keybindings
(use-package which-key
  :config
  (which-key-mode))

;; For consistent project code styles
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Zooms current pane!
(use-package zoom
  :config
  (custom-set-variables
   '(zoom-size '(0.8 . 0.8))))

;; Copies as Slack / Github / Etc formatted code blocks
(use-package copy-as-format)

;; Used for sending alerts / notifications
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style (if (eq system-type 'darwin)
                                'osx-notifier
                              'libnotify)))

;; Used for running a single buffer in an isolated environment
(use-package quickrun
  :config
  (add-hook 'quickrun--mode-hook 'turn-off-evil-mode)
  (quickrun-add-command "blarb"
    '((:command . "blarb")
      (:exec    . "%c %s"))
    :mode 'blarb-mode))

;; For word lookups

(use-package define-word)

;; Code snippets.
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-TAB") 'yas-expand))

;; LSP mode
(use-package lsp-mode
  :hook
  (scala-mode . lsp)
  ;;(lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil))

(when (display-graphic-p)
  (use-package lsp-ui))

;; Smart tabs
(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'python)
  (mapc (lambda (hook) (add-hook hook #'smart-tabs-mode))
        '(python-mode-hook)))

;; Google under symbol
(use-package google-this)

;; Projectile, for easily searching files in projects
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile)

;; Lastpass
(use-package lastpass)

;; TODO: Port to Ivy

(defun eli/lastpass-helm-list-all ()
  (let* ((cmd-out (shell-command-to-string "lpass ls --color=never"))
         (raw-lines (split-string cmd-out "\n" t)))
    (mapcar (lambda (line)
              (string-match "^\\(.*\\)/\\(.*\\) \\[id: \\([0-9]+\\)\\]$" line)
              (list (match-string 1 line)
                    (match-string 2 line)
                    (match-string 3 line)))
            raw-lines)))

(defun eli/lastpass-helm-row-for-candidate (candidate)
  (let ((name (nth 1 candidate)))
    (list name candidate)))

(defun eli/lastpass-helm-copy-candidate (candidates)
  "Copies the candidate password to clipboard"

  (let* ((candidate (car candidates))
         (id (nth 2 candidate))
         (name (nth 1 candidate))
         (cmd (concat "lpass show " id " --password --color=never"))
         (pw (shell-command-to-string cmd)))
    (kill-new pw)
    (message (concat "Copied password for " name " to the clipboard"))))

(defun eli/lastpass-helm-search ()
  "Search for a lastpass password to copy to clipboard"
  (interactive)
  (let* ((list-forms (eli/lastpass-helm-list-all))
         (rows (mapcar 'eli/lastpass-helm-row-for-candidate list-forms)))
    (helm
     :prompt "Find password: "
     :sources (helm-build-sync-source "Lastpass Search"
                :candidates 'rows
                :action 'eli/lastpass-helm-copy-candidate))))

;(global-set-key (kbd "C-c l p") 'eli/lastpass-helm-search)

;; AWS console plugin
;; TODO: Load from the git repo insteal
(require 'aws-console)
(global-set-key (kbd "C-c a e") 'aws-console/emr/ivy-ls)
(global-set-key (kbd "C-c a g") 'aws-console/glue/ivy-list-databases)

;; Silver searcher
(use-package ag
  :config
  (setq ag-executable "/usr/local/bin/ag"))

(use-package elcord)
