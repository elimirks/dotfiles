(provide 'eli/evil)


;; For defining the leader key
(use-package general)

;; Base evil package
(use-package evil
  :demand
  :init
  ;; Unbind <C-u> for evil mode'
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)

  ;; Make asterisk search for dash-included-words
  (setq-default evil-symbol-word-search t)
  ;; Put the cursor in newly created panes
  ;;(setq evil-split-window-below t)
  ;;(setq evil-vsplit-window-right t)

  ;; Automatically opens counsel after :e
  (define-key evil-ex-map "e " 'counsel-find-file)

  (general-create-definer bind-leader
    :keymaps 'global
    :states '(normal emacs)
    :prefix "SPC")

  (general-define-key
   :states 'motion
   "k" 'evil-previous-visual-line
   "j" 'evil-next-visual-line)

  (general-define-key
   :states 'operator
   "k" 'evil-previous-line
   "j" 'evil-next-line)

  ;; Suspend nicely in terminal
  (general-define-key
   :states 'normal
   "C-z"  (lambda () (interactive)
            (when (eq (display-graphic-p) nil)
              (suspend-frame))))

  (defun eli/counsel-projectile-grep () (interactive)
         (if (executable-find "ag")
             (counsel-projectile-ag)
           (counsel-projectile-grep)))

  (bind-leader
    "a" 'org-agenda
    "c" 'cfw:open-org-calendar
    "dd" (lambda () (interactive) (message (current-time-string)))
    "dl" 'define-word-at-point
    "ds" 'define-word
    "e" 'gnus
    "g" 'magit-status
    "kk" (lambda () (interactive) (kill-buffer (current-buffer)))
    "kw" 'kill-buffer-and-window
    "l" 'org-timeline
    "m" 'ivy-switch-buffer
    "pf" 'counsel-projectile-find-file
    "pg" 'eli/counsel-projectile-grep
    "q" 'quickrun
    "r" 'recompile
    "sco" 'slack-channel-join
    "scs" 'slack-channel-select
    "sil" 'slack-im-list-update
    "sio" 'slack-im-open
    "sis" 'slack-im-select
    "ss" 'slack-start
    "w" 'save-buffer
    "z" 'zoom))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

;; Tpope's surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Used for aligning (similar to align-regexp, but more vimy)
(use-package evil-lion
  :config
  (evil-lion-mode))

;; Disable evil-ex initial auto-fill
(define-key evil-normal-state-map (kbd ":")
  '(lambda () (interactive)
     (evil-ex "")))

;; Fix for using Emacs in the terminal with EVIL+org mode
(unless (display-graphic-p)
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))))

;;
;; Powerline config
;;

(use-package powerline
  :config
  (setq powerline-arrow-shape 'curve
        powerline-display-buffer-size nil
        powerline-display-mule-info nil)
  (powerline-default-theme)
  (remove-hook 'focus-out-hook 'powerline-unset-selected-window)
  (setq powerline-height 17)
  (defpowerline powerline-minor-modes ""))
