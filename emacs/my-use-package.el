;;; init-use-package.el --- Emacs Init File - use-package declarations

;;; Commentary:
;;    Emacs Initialization with use-package

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)


;; Add paths
;;
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;; Enable use-package
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)


;; Package Install ---------------------------------------------------------- ;;

;; Diminish
;;
(use-package diminish
  :ensure t)

;; ------------------------------------------------------------

;; Recent file list
;;
(use-package recentf
  :ensure t
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-menu-items 20))

;; ------------------------------------------------------------

;; Automatically update packages
;;
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 7
        auto-package-update-prompt-before-update nil)
  (auto-package-update-maybe))

;; ------------------------------------------------------------

;; Helm
;;
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-r" . helm-mini)
         ("<f8>" . helm-mini)))

;; ------------------------------------------------------------

;; Speedbar
;;
(use-package sr-speedbar
  :ensure t
  :bind ("<f10>" . sr-speedbar-toggle))

;; ------------------------------------------------------------

;; Adaptive Wrap
;;
(use-package adaptive-wrap
  :ensure t)

;; ------------------------------------------------------------

;; Neotree mode
;;
(use-package neotree
  :ensure t
  ;; :bind ("<f8>" . neotree-toggle)
  :hook (neo-after-create . text-scale-decrease)
  :config
  (setq neo-smart-open t)
  (setq neo-theme 'ascii))

;; ------------------------------------------------------------

;; Org-bullets mode
(use-package org-bullets
  :ensure t
  :requires org
  :diminish)

;; ------------------------------------------------------------

;; Transpose frame
;;
(use-package transpose-frame
  :ensure t
  :bind ("C-x t" . transpose-frame))

;; ------------------------------------------------------------

;; Flyspell mode
;;
(use-package flyspell
  :ensure t
  :bind (("C-<f9>" . flyspell-check-next-highlighted-word)
         ("M-<f9>" . flyspell-check-previous-highlighted-word))
  :hook ((c++-mode . flyspell-prog)
         (text-mode . flyspell-mode)
         (org-mode . flyspell-mode))
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)))

;; ------------------------------------------------------------

;; Company mode
;;
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("C-<tab>" . company-complete)
  :hook (;(after-init . global-company-mode)
         (c++-mode . company-mode)
         (c-mode . company-mode)
         (emacs-lisp-mode . company-mode))
  :config
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'cperl-indent-level 'tab-width)
  ;; Company Irony
  (use-package company-irony
    :ensure t
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))
  ;; Company C Headers
  (use-package company-c-headers
    :ensure t
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-c-headers))))

;; ------------------------------------------------------------

;; Irony mode
;;
(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

;; ------------------------------------------------------------

;; Flycheck Irony mode
;;
(use-package flycheck-irony
  :ensure t
  :requires irony)

;; ------------------------------------------------------------

;; Smooth scrolling mode
;;
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 15)
  (setq scroll-preserve-screen-position 1))

;; ------------------------------------------------------------

;; Org mode
;;
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c m" . (lambda () (interactive) (org-capture nil "m")))
         ("C-c g" . (lambda () (interactive) (org-capture nil "g")))
         ("C-c t" . (lambda () (interactive) (org-capture nil "t"))))
  :hook ((org-mode . turn-on-visual-line-mode))
  :init
  (setq org-todos-file "~/org/todos.org"
        org-mitg-file "~/org/mitg.org"
        org-gl-file "~/org/gl.org"
        org-log-done 'time
        org-agenda-files '("~/org" "~/Dropbox/org"))
  (setq org-capture-templates
        '(("m" "Meeting" entry (file+olp+datetree org-slvnv-file)
           "* %?\n \n" :kill-buffer t)
          ("g" "GL Meeting" entry (file+olp+datetree org-gl-file)
           "* %?\n \n" :kill-buffer t)
          ("t" "todo" entry (file+headline org-todos-file)
           "* TODO %u%? [/]\n" :kill-buffer t)))
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "?" "|" "DONE" "CANCELED")
          (sequence "QUESTION" "DEFECT" "|" "FILED" "RESOLVED")))
  (setq org-agenda-custom-commands
        '(("w" "Weekly review" agenda ""
           ((org-agenda-span 'week)
            (org-agenda-start-on-weekday 0)
            (org-agenda-start-with-log-mode t)
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'nottodo 'done)))
           ("~/last-week-todos.html"))))
  ;; (setq org-agenda-custom-commands
  ;;       '(("r" "Last Week's TODOs" agenda
  ;;          ((org-agenda-span 'week)
  ;;           (org-agenda-start-on-weekday 0)
  ;;           (org-agenda-start-with-log-mode t))
  ;;          ("~/last-week-todos.html"))))
	    
  :config
  ;; Org mode LaTeX export
  (use-package ox-latex
    :requires org)
  ;; Org mode JIRA export
  (use-package ox-jira
    :ensure t
    :requires org))

;; ------------------------------------------------------------

;; Flycheck mode
;;
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (flycheck-mode . flycheck-irony-setup)
  :bind (("C-<f9>" . flycheck-next-error)
         ("M-<f9>" . flycheck-previous-error)))

;; ------------------------------------------------------------

;; Systemd Unit mode
;;
(use-package systemd
  :ensure t)

;; ------------------------------------------------------------

;; JSON mode
;;
(use-package js
  :mode "\\.json$"
  :interpreter "js"
  :ensure t)

;; ------------------------------------------------------------

;; MATLAB mode
;;
(use-package matlab
  :mode ("\\.m$" . matlab-mode)
  :hook ((matlab-mode . (lambda () (matlab-cedet-setup)))
         (matlab-mode . (lambda () (mlint-minor-mode t))))
  :config
  (setq matlab-indent-function t
        matlab-show-mlint-warnings t
        matlab-shell-command "matlab"
        matlab-shell-command-switches
        (list "-nosplash" "-nodesktop"))
  (autoload 'mlint-minor-mode "mlint" nil t))

;; ------------------------------------------------------------

(provide 'my-use-package)
;;; my-use-package ends here
