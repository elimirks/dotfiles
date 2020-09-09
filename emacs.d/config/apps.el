;;
;; Gnus
;;

(use-package gnus
  :config
  (setq user-mail-address "elimirks@gmail.com"
        user-full-name "Elijah Mirecki")

  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl)))

  (setq gnus-posting-styles
        `((".*"
           (address "elimirks@gmail.com")
           (name "Elijah Mirecki")
           ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))
          ;; Carpages.ca (work) email
          ("^nnimap[+]rackspace*"
           (address "elijah@carpages.ca")
           (name "Elijah Mirecki")
           ("X-Message-SMTP-Method" "smtp smtp.emailsrvr.com 25"))))

  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "rackspace"
                        (nnimap-address "secure.emailsrvr.com")
                        (nnimap-server-port 993)
                        (nnimap-stream ssl)
                        (nnir-search-engine imap)
                        (nnmail-expiry-wait 90)))

  (setq gnus-permanently-visible-groups ".*INBOX.*")

  ;; Display attachment images inline
  (add-to-list 'mm-attachment-override-types "image/.*"))

;; "Big Brother DataBase", for address book
(use-package bbdb
  :config
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  ;; Disable helm for creating BBDB entries
  ;; (It caused annoying completion issues)
  (add-to-list 'helm-completing-read-handlers-alist
               '(bbdb-create . nil))
  (bbdb-insinuate-message)
  (setq
   bbdb-file "~/Dropbox/Notes/bbdb"
   bbdb-always-add-address t
   bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)
  (setq
   bbdb-ignore-some-messages-alist
   '(( "From" .
       "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))))

;;
;; DocViewMode
;;

;; This mode is for document viewing, such as PDFs.

;; Evil mode caused the document to blink - this fixes it
(evil-set-initial-state 'doc-view-mode 'emacs)
(add-hook 'doc-view-mode-hook
          (lambda ()
            (set
             (make-local-variable 'evil-emacs-state-cursor)
             (list nil))))

;;
;; ERC
;;

(use-package erc
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;;
;; Slack
;;

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config

  ;; Only use Slack company completion
  (make-local-variable 'company-backends)
  (setq company-backends '((company-slack-backend)))

  (add-hook 'slack-mode-hook
            '(lambda ()
               (flycheck-mode -1)
               (company-mode -1)))

  ;; Disable helm for file uploads - it gets stuck in a loop :/
  (add-to-list 'helm-completing-read-handlers-alist
               '(slack-file-upload . nil))

  (evil-define-key 'normal slack-mode-map
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

;; Function to select a new team programatically

(defun eli/slack-change-to-team (team-name)
  "Changes the current Slack team - to set a default team in config"
  (interactive)
  (let ((team (slack-team-find-by-name team-name)))
    (when team
      (progn
        (setq slack-current-team team)
        (slack-team-connect team))
      (message (concat "No such Slack team: " team-name)))))

(defun eli/slack-request-emoji ()
  "Pull emojis for the current team."
  (interactive)
  (if slack-current-team
      (slack-request-emoji slack-current-team)
    (message "Not connected to any team!")))

;;
;; MultiTerm
;;

(use-package multi-term
  :config
  (evil-define-key 'normal term-mode-map
    (kbd "RET") 'term-send-return)

  (setq multi-term-program "/bin/zsh"))

;;
;; REST Client
;; https://github.com/pashky/restclient.el
;;

(use-package restclient
  :bind (:map restclient-mode-map
              ("C-c C-e" . eli/url-hexify-region)
              ("C-c C-c" . restclient-http-send-current-stay-in-window))
  :mode ("\\.http$" . restclient-mode)
  :config
  (defconst restclient-method-url-regexp
    (concat
     "^\\("
     "GET\\|POST\\|DELETE\\|PUT\\|HEAD"
     "\\|OPTIONS\\|PATCH\\|LINK\\|UNLINK"
     "\\) \\(.*\\)$")))
(use-package restclient-helm)
(use-package company-restclient
  :config
  (add-hook 'restclient-mode-hook
            (lambda ()
              (set
               (make-local-variable 'company-backends)
               (list 'company-restclient)))))

;;
;; xkcd
;;

(use-package xkcd
  :if window-system
  :bind (:map xkcd-mode-map
              ("C-c C-n" . xkcd-next)
              ("C-c C-p" . xkcd-prev)
              ("C-c C-r" . xkcd-rand)
              ("C-c C-a" . xkcd-alt-text)))
