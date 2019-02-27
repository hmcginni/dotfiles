;; ========================================================================== ;;
;; Package Init/Install 
;;


;; Package Init ------------------------------------------------------------- ;;

;; Emacs Packages
;;
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
        auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

;; ------------------------------------------------------------

;; Helm
;;
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-r" . helm-mini)))

;; ------------------------------------------------------------

;; Speedbar
;;
(use-package sr-speedbar
  :ensure t
  :bind ("<f9>" . sr-speedbar-toggle))

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
  :bind (("<f7>" . flyspell-mode)
         ("C-M-<f8>" . flyspell-buffer)
         ("M-<f7>" . flyspell-check-previous-highlighted-word))
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (global-set-key (kbd "C-<f7>") 'flyspell-check-next-highlighted-word))

;; ------------------------------------------------------------

;; Company mode
;;
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("C-<tab>" . company-complete)
  :hook ((after-init . global-company-mode)
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
         ("C-c a" . org-agenda))
  :init
  (setq org-todos-file "~/org/todos.org"
	org-default-diary-file "~/org/diary.org"
	org-log-done 'time
	org-agenda-files '("~/org"))
  (setq org-capture-templates
        '(("t" "todo" entry
           (file+headline org-todos-file "Unfiled")
           "* TODO %u%? [/]\n\n*Captured from: %a*\n" :kill-buffer t)
          ("d" "diary" entry
           (file+olp+datetree org-default-diary-file)
           "* %?\n%U\n\nCaptured from: %a*\n" :kill-buffer t)))
  (setq org-refile-targets
        '((org-default-diary-file :level . 4)
          (org-todos-file :maxlevel . 2)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(p!)" "|" "DONE(d)" "NO ACTION")))
  :config
  ;; Org mode LaTeX export
  (use-package ox-latex
    :requires org))

;; ------------------------------------------------------------

;; Org mode JIRA export
;;
(use-package ox-jira
  :ensure t
  :requires org)

;; ------------------------------------------------------------

;; Viper mode
;;
(use-package viper
  :ensure t
  :defer t)

;; ------------------------------------------------------------

;; Flycheck mode
;;
(use-package flycheck
  :ensure t
  :hook ((c++-mode . global-flycheck-mode)
         (flycheck-mode . flycheck-irony-setup)))

;; ------------------------------------------------------------

;; MATLAB mode
;;
(use-package matlab-mode
  :mode "\\.m$"
  :interpreter "MATLAB"
  :hook (matlab-mode . (lambda ()
                         (auto-complete-mode 1)
                         (matlab-cedet-setup)
                         (matlab-toggle-show-mlint-warnings)))
  :config
  (setq matlab-indent-function t
        matlab-shell-command "matlab"
        matlab-shell-command-switches
        (list "-nosplash" "-nodesktop")))

; ------------------------------------------------------------------------------
