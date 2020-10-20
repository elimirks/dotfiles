;;
;; Markdown
;;

(use-package markdown-mode
  :mode "\\.\\(m\\(ark\\)?down\\|md\\)$")
;;
;; Prolog
;;

(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))
(add-hook 'prolog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'prolog-compile-file)
            (local-set-key (kbd "<backtab>") 'ediprolog-dwim)))

;;
;; Matlab
;;

;; This Matlab mode breaks the built in Aquamacs matlab mode
(unless (boundp 'aquamacs-version)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/matlab-emacs")
  (load-library "matlab-load")
  (matlab-cedet-setup)
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (add-to-list
   'auto-mode-alist
   '("\\.m$" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab")
  ;; elisp setup for matlab-mode:
  (setq matlab-shell-command-switches (list "-nodesktop" "-nosplash")))

;;
;; C/C++
;;

(require 'cc-mode)

(use-package company-irony
  :config
  (add-hook 'irony-mode-hook
            (lambda () (add-to-list 'company-backends 'company-irony))))
(use-package flycheck-irony
  :config
  (add-hook 'irony-mode-hook 'flycheck-irony-setup))
(use-package irony
  :config
  ;;(add-hook 'c++-mode-hook 'irony-mode)
  ;;(add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;;
;; Blarb
;;

(use-package blarb-mode
  :straight (
   :type git
   :host github
   :repo "elimirks/BlarbVM"
   :files ("editors/blarb-mode.el")))

;;
;; CSV
;;
(use-package csv-mode
  :mode "\\.csv$"
  :config
  (define-key csv-mode-map (kbd "C-c C-c")
    (lambda ()
      (interactive "P")
      (csv-align-fields nil (window-start) (window-end)))))

;;
;; Python
;;

(add-hook 'python-mode-hook
          (lambda ()
            ;; Python smart tabs
            (smart-tabs-advice py-indent-line py-indent-offset)
            (smart-tabs-advice py-newline-and-indent py-indent-offset)
            (smart-tabs-advice py-indent-region py-indent-offset)
            (setq evil-indent-convert-tabs nil)))

(when (eq system-type 'darwin)
  (setq python-shell-interpreter "/usr/local/bin/python3"))

;;
;; Elisp
;;

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)

;;
;; Web
;;

(use-package web-mode
  :mode "\\.\\(tsx\\|html\\.twig\\)$"
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-indentation t)

  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Some functions for more easily narrowing script and style tags in web mode.

;; Based on a gist by ceving:
;; https://gist.github.com/ceving/7ba174960b9dd3516fff

(defun eli/narrow-to-html-style ()
  "Narrow a HTML buffer to the style part and switch to css-mode."
  (interactive)
  (widen)
  (goto-char (point-min))
  (re-search-forward "<style")
  (forward-line 1)
  (beginning-of-line)
  (let ((b (point)))
    (re-search-forward "</style>")
    (beginning-of-line)
    (let ((e (point)))
      (narrow-to-region b e)
      (css-mode))))

(defun eli/narrow-to-html-script ()
  "Narrow a HTML buffer to the script part and switch to js-mode."
  (interactive)
  (widen)
  (goto-char (point-min))
  (re-search-forward "<script")
  (forward-line 1)
  (beginning-of-line)
  (let ((b (point)))
    (re-search-forward "</script>")
    (beginning-of-line)
    (let ((e (point)))
      (narrow-to-region b e)
      (js-mode))))

(defun eli/widen-to-html ()
  "Widen a HTML buffer and reenable web-mode."
  (interactive)
  (widen)
  (web-mode))

(defun eli/html-smart-narrow ()
  "Intelligently narrow or widen an HTML script or style tag."
  (interactive)

  (let ((lang (web-mode-language-at-pos (point))))
    (cond ((string= 'web-mode major-mode)
           (cond ((string= lang "javascript")
                  (eli/narrow-to-html-script))
                 ((string= lang "css")
                  (eli/narrow-to-html-style))))
          ((string= 'js-mode major-mode)
           (eli/widen-to-html))
          ((string= 'css-mode major-mode)
           (when (string= lang "css")
             (eli/widen-to-html))))))

(defun eli/bind-html-smart-narrow ()
  (local-set-key (kbd "C-x n SPC") 'eli/html-smart-narrow))

(add-hook 'web-mode-hook 'eli/bind-html-smart-narrow)
(add-hook 'js-mode-hook 'eli/bind-html-smart-narrow)
(add-hook 'css-mode-hook 'eli/bind-html-smart-narrow)

;;
;; PHP
;;

(use-package php-mode
  :mode "\\.\\(php\\|inc\\)$")

;;
;; TypeScript
;;

(use-package typescript-mode)

;;
;; PureScript
;;

(use-package psc-ide)
(use-package purescript-mode
  :hook
  (purescript-mode . (lambda ()
                       (psc-ide-mode)
                       (company-mode)
                       (flycheck-mode)
                       (turn-on-purescript-indentation))))

;;
;; C#
;;

(use-package csharp-mode)
(use-package omnisharp
  :after company
  :config
  (setq omnisharp-server-executable-path
        "/usr/local/omnisharp/run.sh")
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)

  :bind
  (:map omnisharp-mode-map
        ("C-c C-c" . omnisharp-run-code-action-refactoring)))

;;
;; YAML
;;

(use-package yaml-mode)

;;
;; Bison
;;

(use-package bison-mode
  :mode "\\.y$")

;;
;; Kotlin
;;

(use-package kotlin-mode)

;; Nicer kotlin indentation

;; Overrides the default kotlin indent function
(defun kotlin-mode--indent-line ()
  "Indent current line as kotlin code"
  (interactive)
  (beginning-of-line)
  (if (bobp) ; 1.)
      (kotlin-mode--beginning-of-buffer-indent)
    (let ((not-indented t)
          (cur-indent 0))
      (cond ((looking-at "^[ \t]*\\.") ; line starts with .
             (save-excursion
               (kotlin-mode--prev-line)

               (setq cur-indent
                     (if (looking-at "^[ \t]*\\.")
                         (current-indentation)
                       (+ (current-indentation) kotlin-tab-width)))

               (when (< cur-indent 0)
                 (setq cur-indent 0))))

            ((looking-at "^[ \t]*}") ; line starts with }
             (save-excursion
               (kotlin-mode--prev-line)
               (while (and
                       (or (looking-at "^[ \t]*$") (looking-at "^[ \t]*\\."))
                       (not (bobp)))
                 (kotlin-mode--prev-line))
               (setq cur-indent
                     (if (or
                          (looking-at ".*{[ \t]*$")
                          (looking-at ".*{.*->[ \t]*$"))
                         (current-indentation)
                       (- (current-indentation) kotlin-tab-width))))

             (when (< cur-indent 0)
               (setq cur-indent 0)))

            ((looking-at "^[ \t]*)") ; line starts with )
             (save-excursion
               (kotlin-mode--prev-line)
               (setq cur-indent (- (current-indentation) kotlin-tab-width)))
             (when (< cur-indent 0)
               (setq cur-indent 0)))

            ((looking-at ".*[gs]et\(.*") ; line is a getter or setter
             (save-excursion
               (kotlin-mode--prev-line)
               (setq cur-indent (+ (current-indentation) kotlin-tab-width))))

            (t
             (save-excursion
               (while not-indented
                 (kotlin-mode--prev-line)
                 (cond ((looking-at ".*{[ \t]*$") ; line ends with {
                        (setq cur-indent
                              (+ (current-indentation) kotlin-tab-width))
                        (setq not-indented nil))

                       ((looking-at "^[ \t]*}") ; line starts with }
                        (setq cur-indent (current-indentation))
                        (setq not-indented nil))

                       ((looking-at ".*{.*->[ \t]*$") ; line ends with ->
                        (setq cur-indent
                              (+ (current-indentation) kotlin-tab-width))
                        (setq not-indented nil))

                       ((looking-at ".*([ \t]*$") ; line ends with (
                        (setq cur-indent
                              (+ (current-indentation) kotlin-tab-width))
                        (setq not-indented nil))

                       ((looking-at "^[ \t]*).*$") ; line starts with )
                        (setq cur-indent (current-indentation))
                        (setq not-indented nil))

                       ((bobp) ; 5.)
                        (setq not-indented nil)))))))
      (indent-line-to cur-indent))))

;;
;; Java
;;

(add-to-list 'auto-mode-alist '("\\.gradle$" . java-mode))

;;
;; Groovy
;;

(use-package groovy-mode
  :mode ("\\.gradle$" . groovy-mode))

;;
;; Objective-C
;;

(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(use-package objc-font-lock
  :config
  (objc-font-lock-global-mode 1))

;;
;; Eshell
;;

(add-hook 'eshell-mode-hook
          '(lambda () (company-mode -1)))

;;
;; Rust
;;

(use-package rust-mode)
(use-package flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;
;; Scala
;;

;; Config based on: https://scalameta.org/metals/docs/editors/emacs.html

;; Scala & SBT mode
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals
  :config
  (setq lsp-metals-treeview-show-when-views-received t))

;;
;; Lua
;;

(use-package lua-mode)

;;
;; Haskell
;;

(use-package haskell-mode)

(use-package lsp-haskell
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

;;
;; Monty
;;

(use-package monty-mode
  :straight (
   :type git
   :host github
   :repo "Mulan-Szechuan-Sauce/monty"
   :files ("plugins/monty-mode.el")))
