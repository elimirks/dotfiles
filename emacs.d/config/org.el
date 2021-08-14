;;
;; Base
;;

(setq
 org-pretty-entities t ; Alows displaying UTF-8 chars like \alpha
 org-startup-truncated nil
 org-src-fontify-natively t
 org-agenda-files '("~/Dropbox/Notes/everything.org")
 org-src-window-setup 'current-window
 ;; Allows custom inline image sizes
 org-image-actual-width nil
 ;; Makes inline latex previews bigger
 org-format-latex-options (plist-put
                           org-format-latex-options :scale 1.7)
 org-export-latex-table-caption-above nil
 org-latex-table-caption-above nil
 org-latex-caption-above nil)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(define-key org-mode-map (kbd "M-h") 'org-metaleft)
(define-key org-mode-map (kbd "M-s") 'org-metaright)
(define-key org-mode-map (kbd "M-e") 'org-latex-export-to-pdf)

;; Code evaluation prompt settings.
(setq org-confirm-babel-evaluate nil)

;; Don't spell check in org source code blocks.
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_LATEX" . "#\\+END_LATEX"))

;;
;; Agenda
;;

;; Match those tagged with, are not scheduled/deadlined, are not DONE.
(setq org-agenda-custom-commands
      '(("d" "non-[d]eadlined tasks"
         tags (concat "-DEADLINE={.+}/!+TODO|+STARTED|+WAITING"
                      " -SCHEDULED={.+}/!+TODO|+STARTED|+WAITING"))))

;; Make the agenda schedule prettier
(setq org-agenda-prefix-format
      '((agenda . " %i %-12t% s %b\n                           ")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")))

;; Hide DONE items
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t)
;; Set a 30 day span, instead of a week view
(setq org-agenda-start-day "-3d"
      org-agenda-span 30)
(setq org-agenda-show-all-dates nil) ; Omit empty days in the agenda
(setq org-deadline-warning-days 0) ; Disable pre-warnings
;; Hide the time grid by default
(setq org-agenda-use-time-grid nil)

;;
;; Calendar
;;
(use-package calfw
  :config
  (use-package calfw-org)
  
  ;; Nicer Unicode characters
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))

;;
;; LaTeX
;;

;; Settings for exporting to LaTeX
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      ;; Need 3 of the same string, oddly.
      (make-list 3 (concat
                    "pdflatex -shell-escape -interaction nonstopmode"
                    " -output-directory %o %f")))

(use-package org-roam
      :ensure t
      :init
      (setq org-roam-v2-ack t)
      :custom
      (org-roam-directory (file-truename "~/Dropbox/roam/"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))
