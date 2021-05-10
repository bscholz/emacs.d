;;; init.el --- emacs initialization
;;; Commentary:
;;; Code:
;;Put my custom files in .emacs.d
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Add key binding to revert buffer
(global-set-key (kbd "C-c C-r") '(lambda () (interactive) (revert-buffer t t)))

;; Thrush-like macro to thread through forms as the first arg (like Clojure)
(defmacro -> (&rest body)
  "Thread the first expression through the given BODY forms as the first argument."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

;; Thrush-like macro to thread through forms as the last arg (like Clojure)
(defmacro ->> (&rest body)
  "Thread the first expression through the given BODY forms as the last argument."
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defun unix-file ()
  "Change the current buffer to UTF 8 with Unix EOLs."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

(defun dos-file ()
  "Change the current buffer to UTF 8 with DOS EOLs."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))

(defun set-indentation-level (n)
  "Change the current indentation to N spaces."
  (interactive "NNumber of spaces: ")
  (if (boundp 'c-basic-offset) (setq c-basic-offset n))
  (if (boundp 'js-indent-level) (setq js-indent-level n))
  (if (boundp 'js2-basic-offset) (setq js2-basic-offset n))
  (if (boundp 'sgml-basic-offset) (setq sgml-basic-offset n))
  (setq indent-tabs-mode nil)
  (setq tab-width n)
  (redraw-display))
(global-set-key (kbd "C-c i") 'set-indentation-level)

(defun find-next-long-line ()
  "Find the next line longer than 80 characters."
  (interactive)
  (occur (make-string 81 ?.)))
(global-set-key (kbd "C-c l") 'find-next-long-line)

;; Join the current line to the following line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; emacs lisp shortcuts
(global-set-key (kbd "C-c C-k") 'emacs-lisp-byte-compile-and-load)
(global-set-key (kbd "C-c C-e") 'eval-current-buffer)

(defun eshell/clear()
  "Clear the eshell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Replace "old" with "new" everywhere it occurs within the given string,
;; starting from the given offset. Case sensitive.
(defun string-replace-from-offset (old new str offset)
  "Replace OLD with NEW everywhere it occurs within string STR, starting at OFFSET."
  (let ((case-fold-search nil)) ; be case sensitive
    (replace-regexp-in-string (regexp-quote old) new str t t nil offset)))

;; Replace "old" with "new" everywhere in the string. Case sensitive.
(defun string-replace (old new str)
  "Replace OLD with NEW everywhere it occurs within string STR."
  (string-replace-from-offset old new str 0))

;; Buffer sanitation
(defun cleanup-buffer ()
  "Clean up the current buffer.
Untabify, delete trailing whitespace, set to UTF-8, and re-indent."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;;
;; Clojure functions to switch between source code and unit tests
;; (because I don't like the one included in projectile mode)
;;

(defun clj-current-buffer-namespace ()
  "Return the Clojure namespace of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "(ns[[:space:]]+\\([[:alnum:]-.]+\\)")
    (match-string 1)))

(defun clj-src-to-test (file-path)
  "Return the file name of the test file corresponding to FILE-PATH."
  (->> file-path
       (string-replace "/src/" "/test/")
       (string-replace ".clj" "_test.clj")))

(defun clj-test-to-src (file-path)
  "Return the file name of the src file corresponding to FILE-PATH."
  (->> file-path
       (string-replace "/test/" "/src/")
       (string-replace "_test.clj" ".clj")))

(defun clj-create-test-file (src-ns test-file)
  "Create a Clojure test skeleton for SRC-NS namespace in TEST-FILE."
  (let ((content (concat "(ns " src-ns "-test\n"
                         "  (:require [" src-ns " :refer :all])\n"
                         "  (:use [clojure.test]))\n"
                         "\n"
                         "(deftest test-")))
    (find-file test-file)
    (insert content)))

(defun clj-create-src-file (test-ns src-file)
  "Create a Clojure src skeleton for TEST-NS namespace in SRC-FILE."
  (let* ((offset (- (length test-ns) 5))
         (src-ns (string-replace-from-offset "-test" "" test-ns offset))
         (content (concat "(ns " src-ns "\n\n")))
    (find-file src-file)
    (insert content)))

(defun clj-unit-test-p (file)
  "Return true if FILE seems to be a Clojure unit test."
  (let ((start (- (length file) (length "_test.clj"))))
    (and (> start 0)
         (string-match-p (regexp-quote "_test.clj") file start))))

(defun clj-jump-to-src (test-ns test-file)
  "Jump to the Clojure source code for TEST-NS in TEST-FILE."
  (let ((src-file (clj-test-to-src test-file)))
    (if (file-exists-p src-file) (find-file src-file)
      (clj-create-src-file test-ns src-file))))

(defun clj-jump-to-test (src-ns src-file)
  "Jump to the Clojure test code for SRC-NS in SRC-FILE."
  (let ((test-file (clj-src-to-test src-file)))
    (if (file-exists-p test-file) (find-file test-file)
      (clj-create-test-file src-ns test-file))))

(defun clj-jump-to-or-from-unit-test ()
  "Jump between clojure source and unit test."
  (interactive)
  (let ((ns (clj-current-buffer-namespace))
        (file (buffer-file-name)))
    (if (clj-unit-test-p file) (clj-jump-to-src ns file)
      (clj-jump-to-test ns file))))

;; Mac sensitive settings for keybindings
(when (eq system-type 'darwin)
  (message "Setting keybindings for darwin")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-function-modifier 'hyper))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-encryption-mode t)
 '(auto-revert-verbose nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(c-basic-offset 2)
 '(c-require-final-newline
   (quote
    ((c-mode . t)
     (c++-mode . t)
     (objc-mode . t)
     (java-mode . t))))
 '(column-number-mode t)
 '(desktop-path (quote (".")))
 '(desktop-save nil)
 '(desktop-save-mode t)
 '(ido-mode (quote buffer) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(java-mode-hook
   (quote
    ((lambda nil
       (c-set-offset
        (quote substatement-open)
        0)))))
 '(js-indent-level 2)
 '(js2-strict-trailing-comma-warning nil)
 '(menu-bar-mode t)
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(nxml-outline-child-indent 3)
 '(package-archive-priorities (quote (("gnu" . 10) ("melpa-stable" . 9) ("melpa" . 5))))
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (yaml-mode prettier-js add-node-modules-path google-translate dockerfile-mode flymd markdown-mode auctex rjsx-mode company projectile flycheck-clojure flycheck magit use-package)))
 '(safe-local-variable-values
   (quote
    ((js2-indent-level . 2)
     (eval add-hook
           (quote js2-mode-hook)
           (quote prettier-js-mode))
     (eval add-to-list
           (quote auto-mode-alist)
           (quote
            ("\\.js\\'" . js2-mode))))))
 '(scroll-bar-mode nil)
 '(sgml-slash-distance 10000)
 '(show-paren-mode t nil (paren))
 '(standard-indent 2)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(visual-basic-mode-indent 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Bootstrap use-package after setting package-archives custom variable
;;
(require 'package)
(package-initialize) ; add load path for packages

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'bind-key) ; use-package needs personal-keybindings variable
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t) ; download and install package if necessary

;;;
;;; Packages
;;;

;; magit is a git porcelain for emacs
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-status))

;; flycheck mode is for checking syntax on the fly
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; flycheck-clojure to check clojure syntax on the fly
(use-package flycheck-clojure)

;; projectile mode helps manage projects
(use-package projectile
  :commands (projectile-project-root))

;; Clojure mode for clojure syntax highlighting
(use-package clojure-mode
  :bind
  ("C-c t" . clj-jump-to-or-from-unit-test)
  :config
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'flycheck-clojure-setup)
  (add-hook 'clojure-mode-hook #'projectile-mode))

;; CIDER is the Clojure IDE that Rocks
(use-package cider
  :config
  (setq cider-cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
                                  (figwheel-sidecar.repl-api/start-figwheel!)
                                  (figwheel-sidecar.repl-api/cljs-repl))"))

;; Rainbow delimiters (used by parinfer, below)
(use-package rainbow-delimiters
  :init
  (progn (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
         (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))

;; Company mode is a generic completion framework (complete any)
(use-package company
  :config (global-company-mode))

;; Web-mode for files with a mix of HTML + javascript
;; (use-package web-mode
;;   :mode "\\.html?\\'")

;; adds project node modules to emacs' exec path
(use-package add-node-modules-path)

;; rjsx-mode for react
(use-package rjsx-mode
  :mode "\\.js?\\'"
  :config
  (add-hook 'rjsx-mode-hook #'flycheck-mode)
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(use-package tex-mode
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

;; Edit markdown
(use-package markdown-mode)
(use-package flymd)

(defun andy-replace ()
  "Replace all characters with dollar signs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "[^$]" nil t)
      (replace-match "$" nil t))))

;; Docker
(use-package dockerfile-mode)

;; Google Translate
(use-package google-translate
  :bind
  ("C-c s" . google-translate-smooth-translate)
  :config
  (setq google-translate-translation-directions-alist '(("en" . "es"))))

;; YAML files (CloudFormation + Serverless Framework)
(use-package yaml-mode)

;; GraphQL
(use-package graphql-mode)

;; Tide (Typescript IDE)
;(use-package tide)

;; Angular 2 mode - brings together typescript mode + tide + company
;(use-package ng2-mode)

;;
;; This is commented because actionscript-mode seems to mess up forward-word
;; somehow... I would like to understand what's up with that.
;;
;; (use-package actionscript-mode
;;   :mode ("\\.as\\'"))

;; (use-package ejc-sql
;;   :config (ejc-create-connection
;;            "bongo-remote-dev"
;;            :classpath "~/.m2/repository/mysql/mysql-connector-java/6.0.6/mysql-connector-java-6.0.6.jar"
;;            :classname "com.mysql.jdbc.Driver"
;;            :subprotocol "mysql"
;;            :subname "//bongo-remote-dev:
;;; init.el ends here
