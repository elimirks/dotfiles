;; Specifies local directory to load packages from
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;
;; Package manager
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t) ; Always fetch packages
(straight-use-package 'use-package)

;; MacOS has some trouble finding some commands
(when (eq system-type 'darwin)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq-default counsel-ag-base-command "/usr/local/bin/ag --vimgrep --nocolor --nogroup %s")

  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; For ansi-term n' stuff
(setq explicit-shell-file-name "/bin/bash")
(setq inhibit-splash-screen t ; No welcome screen
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; Hide scrollbars
(menu-bar-mode -1) ; Hide menu bar
(show-paren-mode t) ; Highlights matching parenthesis
(setq ring-bell-function 'ignore) ; Disable ALL bells! They suck!
(electric-pair-mode -1)
(setq initial-scratch-message "") ; No scratch text
(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

;; No tabs, use spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Default browser for links
(setq browse-url-firefox-program
      (cond ((eq system-type 'darwin) "~/bin/firefox")
            ((eq system-type 'windows-nt) "/Program Files/Mozilla Firefox/firefox.exe")
            (t "firefox"))
      browse-url-generic-program browse-url-firefox-program
      browse-url-browser-function 'browse-url-generic)

(add-to-list 'exec-path "/usr/local/bin")

(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Disable killing scratch
;; From https://emacs.stackexchange.com/questions/19254/never-close-scratch
(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (not (equal (buffer-name) "*scratch*"))
                t
              (message "Not allowed to kill %s, burying instead" (buffer-name))
              (bury-buffer)
              nil)))

;;
;; Backup Settings
;;

(setq backup-by-copying t) ; Stop shinanigans with links
(setq backup-directory-alist '((".*" . "~/.bak.emacs/backup/")))
;; Creates auto directory if it doesn't already exist
(when (eq nil (file-exists-p "~/.bak.emacs/"))
  (make-directory "~/.bak.emacs/"))
(when (eq nil (file-exists-p "~/.bak.emacs/auto"))
  (make-directory "~/.bak.emacs/auto"))
;; Backup in one place. Flat, no tree structure
(setq auto-save-file-name-transforms '((".*" "~/.bak.emacs/auto/" t)))



;;
;; Handy Functions
;;

(defun eli/set-font-size ()
  (interactive)
  (set-face-attribute 'default nil
                      :height (read-number "New font size: ")))

(defun eli/load-init ()
  "Reloads init file (and therefore, README.org)"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun eli/replace-region-via-function (fun)
  "Replace a selected region via the given function"
  (interactive)
  (let* ((bounds (cons (region-beginning) (region-end)))
         (text   (buffer-substring-no-properties
                  (car bounds)
                  (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (funcall fun text)))))

(defun eli/url-hexify-region ()
  "Convert the selectd region to a URL hexified string."
  (interactive)
  (eli/replace-region-via-function 'url-hexify-string))


(defun eli/binary-string-to-ascii (s)
  "Convert a string such as '01100000' into an ascii character string"
  (let ((len (length s))
        (result 0))
    (while (not (= len 0))
      (setq result (+ result (if (string= (substring s 0 1) "1")
                                 (lsh 1 (- len 1))
                               0)))
      (setq s (substring s 1 nil))
      (setq len (- len 1)))
    (make-string 1 result)))

(defun eli/binary-strings-to-ascii-string (list)
  "Convert string list such as 01100000 01110000 into an ascii string"
  (if (= 0 (length list))
      ""
    (concat (eli/binary-string-to-ascii (car list))
            (eli/binary-strings-to-ascii-string (cdr list)))))

(defun eli/convert-binary-string-region-to-ascii ()
  "Convert a binary string region to a human readable string."
  (interactive)
  (eli/replace-region-via-function
   '(lambda (text)
      (eli/binary-strings-to-ascii-string (split-string text)))))

(defun eli/char-to-binary-string (num)
  "Converts a char to a binary string"
  (let ((res "")
        (count 8))
    (while (> count 0)
      (setq res (concat (if (= 1 (logand 1 num)) "1" "0") res))
      (setq num (lsh num -1))
      (setq count (- count 1)))
    res))

(defun eli/string-to-binary-string (s)
  "Converts a string to a binary representation string"
  (if (= 0 (length s))
      ""
    (let ((head (string-to-char (substring s 0 1)))
          (tail (substring s 1 nil)))
    (concat (eli/char-to-binary-string head)
            " "
            (eli/string-to-binary-string tail)))))

(defun eli/string-region-to-binary-string ()
  "Converts a region to a binary representation region"
  (interactive)
  (eli/replace-region-via-function 'eli/string-to-binary-string))

(defun eli/gcd (first second)
  "Return the gcd of the two given values."
  (let ((remainder (mod first second)))
    (if (eq remainder 0)
        second
      (eli/gcd second remainder))))

(defun eli/simplify-fraction (first second)
  "Simplifies the given fraction using their GCD."
  (let ((gcd (eli/gcd first second)))
    (list (/ first gcd) (/ second gcd))))

(defun eli/surround-fat-comment ()
  "Surrounds a line of text with a 'fat comment'"
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (indent (current-indentation))
         (comment-char (string-to-char comment-start))
         (surround (concat
                    (make-string indent ?\s)
                    (make-string
                     (- (length line) indent 1)
                     comment-char))))
    ;; Insert comment after
    (end-of-line)
    (open-line 1)
    (forward-line)
    (insert surround)
    ;; Insert comment before
    (forward-line -1)
    (beginning-of-line)
    (open-line 1)
    (insert surround)))

(defun eli/center-buffer ()
  "Centers the current buffer (based on column ruler)"
  (interactive)
  (let ((margin (/ (- (frame-width) fci-column) 3)))
    (when (> margin 0)
      (set-window-margins nil margin))))

(defun eli/uncenter-buffer ()
  (interactive)
  (set-window-margins nil nil))

(defun eli/align-regexp-eq ()
  "Aligns lines at the equals character."
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)="))
(global-set-key (kbd "C-c e a") 'eli/align-regexp-eq)

(defun eli/match-in-current-buffer (regex &optional match-number)
  "Return the first occurrence reegx in the current buffer, or nil if not found"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (and (search-forward-regexp regex nil t)
           (substring-no-properties (match-string (or match-number 0)))))))

;;
;; Window management
;;

(defun eli/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'eli/split-and-follow-horizontally)

(defun eli/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'eli/split-and-follow-vertically)

(global-set-key "\M-o" 'other-window)

;;
;; Human languages
;;

(defun eli/add-insert-char-key (sequence char)
  (global-set-key (kbd (concat "C-c l" sequence))
                  `(lambda () (interactive) (insert-char ',char))))

;; German
(eli/add-insert-char-key "\" a" ?ä)
(eli/add-insert-char-key "\" A" ?Ä)
(eli/add-insert-char-key "\" o" ?ö)
(eli/add-insert-char-key "\" O" ?Ö)
(eli/add-insert-char-key "\" u" ?ü)
(eli/add-insert-char-key "\" U" ?Ü)
(eli/add-insert-char-key "\" s" ?ß)

;; Polish
(eli/add-insert-char-key ", a" ?ą)
(eli/add-insert-char-key ", A" ?Ą)
(eli/add-insert-char-key "/ c" ?ć)
(eli/add-insert-char-key "/ C" ?Ć)
(eli/add-insert-char-key ", e" ?ę)
(eli/add-insert-char-key ", E" ?Ę)
(eli/add-insert-char-key "/ l" ?ł)
(eli/add-insert-char-key ", L" ?Ł)
(eli/add-insert-char-key "/ n" ?ń)
(eli/add-insert-char-key "/ N" ?Ń)
(eli/add-insert-char-key "/ o" ?ó)
(eli/add-insert-char-key "/ O" ?Ó)
(eli/add-insert-char-key "/ s" ?ś)
(eli/add-insert-char-key "/ S" ?Ś)
(eli/add-insert-char-key "/ z" ?ź)
(eli/add-insert-char-key "/ Z" ?Ź)
(eli/add-insert-char-key ". z" ?ż)
(eli/add-insert-char-key ". Z" ?Ż)

;; Maths
(eli/add-insert-char-key "m 2" ?²)
(eli/add-insert-char-key "m 3" ?³)
(eli/add-insert-char-key "m d" ?°)
(eli/add-insert-char-key "m n n" ?¬)
(eli/add-insert-char-key "m n e" ?≠)
(eli/add-insert-char-key "m n g" ?≯)
(eli/add-insert-char-key "m n l" ?≮)
(eli/add-insert-char-key "m n i" ?∉)
(eli/add-insert-char-key "m g e" ?≥)
(eli/add-insert-char-key "m l e" ?≤)
(eli/add-insert-char-key "m i" ?∈)


;;
;; Tramp config
;;

(when (eq window-system 'w32)
  (setq tramp-default-method "plink")
  (when (and (boundp 'putty-directory)
             (not (string-match putty-directory (getenv "PATH")))
             (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
    (add-to-list 'exec-path putty-directory)))

(load "~/.emacs.d/config/theming.el")
(load "~/.emacs.d/config/miscplugins.el")
(load "~/.emacs.d/config/evil.el")
(load "~/.emacs.d/config/apps.el")
(load "~/.emacs.d/config/org.el")
(load "~/.emacs.d/config/languages.el")

;; For environment specific things
(when (file-exists-p "~/.emacs.d/user.org")
  (org-babel-load-file "~/.emacs.d/user.org"))
(when (file-exists-p "~/.emacs.d/local.el")
  (load "~/.emacs.d/local.el"))
