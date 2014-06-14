;;;; Jiwen "Steve" Cai's emacs config file

;; loading constants
(setq HOSTNAME (system-name))
(setq VERSION (emacs-version))
(message HOSTNAME)
(message VERSION)

(message "=================")
(message " Emacs Apperance ")
(message "=================")

;; display time
(display-time)

;; hiding the menu bar
(menu-bar-mode 0)

;; enable margin line number
(global-linum-mode 1)
(setq linum-format "%3d ")

;; enable line number and column number in status bar
(setq line-number-mode 1)
(setq column-number-mode 1)

;; show TAB as ^I
(standard-display-ascii ?\t "^I")
(setq-default indent-tabs-mode nil)

;; default indentation rules
(setq tab-width 4)
(setq js-indent-level 2)

;; highlight trailing whitespaces
(setq-default show-trailing-whitespace t)

;; show WS as _
(setq whitespace-space 'underline)

;; turning on text mode and auto-fill mode automatically
(setq default-major-mode 'text-mode)

;; use option for meta
(setq mac-option-key-is-meta 1)
(setq mac-option-modifier 'meta)

;; highlight the currnet line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#330")

;; show matching pairs of parentheses
(show-paren-mode 1)

(message "=================")
(message " Emacs Behaviour ")
(message "=================")

;; stop creating backup~ files
(setq make-backup-files nil)

;; stop creating those #autosave# files
(setq auto-save-default nil)

;; ignore shit during grep-find
(setq grep-find-ignored-directories
 (quote (".svn" ".git" "ENV" "oneoffs")))
(setq grep-find-ignored-files
 (quote ("core" "*.o" "*.pyc" "*.min.js" "*.min.css")))

(message "================")
(message " Util Functions ")
(message "================")

(defun indent-all()
  "indent all" (interactive) (indent-region 1 (point-max) nil))

(message "====================")
(message " Custom Key Binding ")
(message "====================")

;; C-z             => undo
(define-key global-map "\C-z" 'undo)

;; C-x C-r         => grep find
(define-key global-map "\C-x\C-r" 'rgrep)

;; C-c o           => uick switch between .h and .c files
(add-hook 'c-mode-common-hook
  (lambda() (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(add-hook 'c-mode-common-hook
  (lambda() (setq c-basic-offset 4)))

(message "======================")
(message " Third Party Packages ")
(message "======================")

;; MELPA
(require 'package)
(setq melpa-package-list '(jabber google-c-style magit))
(setq package-archives
  '(("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package melpa-package-list)
  (unless (package-installed-p package) (package-install package)))

;; Google C style
(require 'google-c-style)
(progn
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; Jabber
(require 'jabber-autoloads)
(setq jabber-account-list
 '(("goodcjw2@gmail.com"
    (:network-server . "talk.google.com")
    (:connection-type . ssl))))
(setq jabber-vcard-avatars-retrieve nil)
(setq jabber-show-resources nil)
(setq jabber-roster-line-format "%n (%j) : %S")
(setq jabber-roster-show-title nil)
(setq jabber-roster-show-bindings nil)
(eval-after-load "jabber-roster"
 '(defun jabber-fix-status (status)
   "Make status strings more readable"
   (when status
    (when (string-match "\n+$" status)
     (setq status (replace-match "" t t status)))
    (when jabber-remove-newlines
     (while (string-match "\n" status)
      (setq status (replace-match " " t t status))))
    (if (> (length status) 20)
     (concat (substring status 0 16) " ...")
     status))))

(message "======")
(message " Done ")
(message "======")

