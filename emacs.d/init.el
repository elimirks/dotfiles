;  _____  _  _  __  __                   
; | ____|| |(_)|  \/  |  __ _   ___  ___ 
; |  _|  | || || |\/| | / _` | / __|/ __|
; | |___ | || || |  | || (_| || (__ \__ \
; |_____||_||_||_|  |_| \__,_| \___||___/

;; The Org config is where all the magic happens:
(org-babel-load-file "~/.emacs.d/config.org")

;; NTS: don't put anything except custom set stuff in this config

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode org-bullets nlinum-relative evil-org evil-magit which-key magit ido-complete-space-or-hyphen smex ido-ubiquitous powerline-evil powerline evil-surround evil-leader evil rainbow-delimiters use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-scheduled ((t (:foreground "magenta"))))
 '(org-scheduled-today ((t (:foreground "magenta"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "dark green"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "firebrick"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange red"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "magenta")))))

