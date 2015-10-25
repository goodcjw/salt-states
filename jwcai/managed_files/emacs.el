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
(setq css-indent-offset 2)

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
(set-face-background 'hl-line "#bada55")

;; show matching pairs of parentheses
(show-paren-mode 1)

;; show current file's full path in mini buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(require 'ido)
(ido-mode 'buffers)

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

;; Fix backward delete
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; C-z             => undo
(define-key global-map "\C-z" 'undo)

;; C-x C-r         => grep find
(define-key global-map "\C-x\C-r" 'rgrep)

;; C-c o           => uick switch between .h and .c files
(add-hook 'c-mode-common-hook
  (lambda() (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(add-hook 'c-mode-common-hook
  (lambda() (setq c-basic-offset 4)))

;; Org Mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-src-fontify-natively t)
(setq org-agenda-files (list "~/org/vessel.org"))
(setq org-todo-keyword-faces
      '(("ONIT" . "yellow")
        ("ABRT" . (:foreground "blue" :weight bold))
        ("EASY" . (:foreground "green" :weight bold))
        ("MODERATE" . (:foreground "yellow" :weight bold))
        ("HARD" . (:foreground "red" :weight bold))
        ))

(message "======================")
(message " Third Party Packages ")
(message "======================")

;; MELPA
(require 'package)
(setq melpa-package-list '(coffee-mode
                           dockerfile-mode
                           flycheck
                           flymake-coffee
                           flymake-easy
                           flymake-less
                           git-gutter
                           google-c-style
                           less-css-mode
                           magit))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

(unless package-archive-contents (package-refresh-contents))
(dolist (package melpa-package-list)
  (unless (package-installed-p package) (package-install package)))

;; Google C style
(require 'google-c-style)
(progn
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; Magit
;; C-c C-g C-g     => magit-status
(define-key global-map "\C-x\C-g\C-g" 'magit-status)
;; C-c C-g C-b     => magit-blame-mode
(define-key global-map "\C-x\C-g\C-b" 'magit-blame)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-blame-heading ((t (:foreground "blue" :weight light))))
 '(magit-diff-added ((t (:foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:foreground "#22aa22"))))
 '(magit-diff-base ((t (:foreground "black"))))
 '(magit-diff-base-highlight ((t (:foreground "black"))))
 '(magit-diff-context ((t (:foreground "black"))))
 '(magit-diff-context-highlight ((t (:foreground "black"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#bada55" :foreground "grey30"))))
 '(magit-diff-lines-heading ((t (:inherit magit-diff-hunk-heading-highlight))))
 '(magit-diff-removed ((t (:foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222"))))
 '(magit-hash ((t (:foreground "blue")))))

;; coffeescript
;; This gives you a tab of 2 spaces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "X"))
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-region)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))

;; Git Gutter
(global-git-gutter-mode 1)


;; XML Format
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
  )
)

;; PEP 8 Checker
(when (load "flymake" t)
 (defun flymake-pylint-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
          (local-file (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
         (list "pep8" (list "--repeat" local-file))))

 (add-to-list 'flymake-allowed-file-name-masks
              '("\\.py\\'" flymake-pylint-init))
)



(require 'flymake-coffee)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(require 'flymake-less)
(add-hook 'less-css-mode-hook 'flymake-less-load)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-def-config-file-var flycheck-coffeelintrc coffee-coffeelint "~/.coffeelint.json")
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; SQL
(setq sql-connection-alist
  '((cardinal-prod
     (sql-product 'postgres)
     (sql-server "cardinal-prod.ctl33fp7rji9.us-west-2.redshift.amazonaws.com")
     (sql-port 5439)
     (sql-user "scai")
     (sql-password "gqNJH7wGdlyXmQn4FAcJLJ5W")
     (sql-database "cardinal"))
    (goose-prod
     (sql-product 'postgres)
     (sql-server "goose.ctl33fp7rji9.us-west-2.redshift.amazonaws.com")
     (sql-port 5439)
     (sql-user "goose")
     (sql-password "CJlTT8TTTYAhLeDdxfJo5WP6")
     (sql-database "goose"))
    )
)

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(defun do-sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))

(message "======")
(message " Done ")
(message "======")
(put 'set-goal-column 'disabled nil)
