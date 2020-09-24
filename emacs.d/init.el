;;  _____  _  _  __  __                   
;; | ____|| |(_)|  \/  |  __ _   ___  ___ 
;; |  _|  | || || |\/| | / _` | / __|/ __|
;; | |___ | || || |  | || (_| || (__ \__ \
;; |_____||_||_||_|  |_| \__,_| \___||___/
;;
;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________

;; The Org config is where all the magic happens:

;(org-babel-load-file "~/.emacs.d/README.org")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/config/main.el")

;; For environment specific things, such as super secret passwords
(when (file-exists-p "~/.emacs.d/user.org")
  (org-babel-load-file "~/.emacs.d/user.org"))

;; NTS: don't put anything except custom set stuff in this config

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c9e858c340109fc5f722e40ffd1c59e437ebfb858793fdf70e76732e1df8a6c5" default))
 '(flycheck-javascript-flow-args nil)
 '(org-agenda-files '("~/Dropbox/Notes/everything.org"))
 '(package-selected-packages
   '(dumb-jump psc-ide purescript-mode flycheck-rust rust-mode google-this jedi smart-tabs-mode objc-font-lock ox-mdx-deck swift-mode evil-lion groovy-mode gradle-mode lsp-intelij lsp-intellij kotlin-mode lsp-ui lsp-javascript-typescript eglot lsp-php fill-column-indicator helm-ag unicode-fonts company-jedi bison-mode emms-setup xkcd restclient-company company-restclient restclient-helm restclient general edit-indirect smart-semicolon smartparens evil-smartparens speed-type multi-term yaml-mode pdf-tools ac-php company-php 4clojure quickrun php-extras php-mode doc-view-mode gnus-desktop-notify copy-as-format slack flow-minor-mode arduino-mode omnisharp love-minor-mode eslint-fix web-mode tide ggtags helm-rtags rtags gtags markdown-mode nlinum-relative evil-org evil-magit which-key magit powerline-evil powerline evil-surround evil-leader evil rainbow-delimiters use-package))
 '(send-mail-function 'smtpmail-send-it)
 '(zoom-size '(0.8 . 0.8)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:foreground unspecified :background "#199919991999"))))
 '(company-scrollbar-bg ((t (:background "#199919991999"))))
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip ((t (:inherit default :background "#051e051e051e"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(linum ((t (:background "color-16" :foreground "#9fc59f"))))
 '(org-scheduled ((t (:foreground "magenta"))))
 '(org-scheduled-today ((t (:foreground "magenta"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "dark green")))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
