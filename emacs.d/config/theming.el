(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'spolsky t)

(if (display-graphic-p)
    ;; GUI
    (progn
      (set-face-attribute 'default nil :height 130))
  ;; Terminal
  (progn
    (setq nlinum-format "%d ")
    (add-to-list 'default-frame-alist '(background-color . "color-16"))))

;;
;; Fancy rainbox parens to sooth my eyes
;;

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#af0")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#fa0")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#a0f")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#0af")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#0fa")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#f0a")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "#fff"))
;;
;; Relative line numbers
;; https://github.com/CodeFalling/nlinum-relative
;;

(defun eli/line-numbers-mode () (interactive)
       (display-line-numbers-mode t)
       (setq display-line-numbers 'relative
             display-line-numbers-width 1))

(if (version< "26.0.50" emacs-version)
    (mapc (lambda (hook) (add-hook hook 'eli/line-numbers-mode))
          '(prog-mode-hook web-mode-hook))
  (use-package nlinum-relative
    :config
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0.25)
    (setq nlinum-relative-current-symbol "")
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)))

;;
;; Rainbow mode (highlight CSS style hex colors)
;; https://julien.danjou.info/projects/emacs-packages#rainbow-mode
;;

(use-package rainbow-mode
  :config
  (mapc (lambda (hook) (add-hook hook (lambda () (rainbow-mode 1))))
        '(emacs-lisp-mode-hook web-mode-hook css-mode-hook
                               js-mode-hook)))

;;
;; Delightful mode display
;; https://www.emacswiki.org/emacs/DelightedModes
;;

(use-package delight
  :config
  (delight '((company-mode " Co" company)
             (flycheck-mode " Fl" flycheck)
             (undo-tree-mode nil undo-tree)
             (which-key-mode nil which-key)
             (ivy-mode nil ivy)
             (editorconfig-mode nil editorconfig)
             (rainbow-mode)
             (smart-semicolon-mode nil smart-semicolon)
             (auto-revert-mode nil autorevert))))

;;
;; Unicode support
;; https://github.com/rolandwalker/unicode-fonts
;;

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))


;;
;; Column & line highlighting
;;

;; Highlight current line
(global-hl-line-mode)
(set-face-background 'hl-line "#222")
(set-face-underline 'hl-line nil)

;; Highlight 80th column
(use-package fill-column-indicator
  :config
  (setq fci-rule-color "#222")
  (setq fci-rule-column 80)
  ;; Display the column indicator in all programming modes
  (add-hook 'prog-mode-hook 'fci-mode))

;; Highlight TODO, FIXME, NOTE
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|NOTE\\):"
                1 font-lock-warning-face t)))))

;;
;; Fonts
;;

;; Nice windows font:
(when (eq window-system 'w32)
  (set-frame-font "Consolas"))

;;
;; Change cursor blink color
;; https://stackoverflow.com/questions/4642835/how-to-change-the-cursor-color-on-emacs
;;

(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'. 
     Warning: overwrites original version in `frame.el'.

     This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (mod (+ 1 blink-cursor-count)
                                  (length blink-cursor-colors))))
  (internal-show-cursor nil (not (internal-show-cursor-p))))
