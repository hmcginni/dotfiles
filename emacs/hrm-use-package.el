;;; hrm-use-package.el --- Emacs Init File - use-package declarations

;;; Commentary:
;;    Emacs Initialization with use-package.

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)


;; Add paths
(let ((default-directory "~/.emacs.d/elpa")))


;; Enable use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)


;; ─────────────────────────────────────────────────────────
;;; Package packages:

;; Built-ins
(use-package emacs
  :delight
  (eldoc-mode)
  (visual-line-mode)
  :diminish
  (helm-mode)
  :hook
  ((emacs-lisp-mode . prettify-symbols-mode)
   (prog-mode . (lambda () (setq tab-width 4)))
   (c++-mode . (lambda () (setq tab-width 4)))
   (python-mode . (lambda () (setq python-indent-offset 4
							  python-indent 4)))))


;; Automatically update packages
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 7
        auto-package-update-prompt-before-update nil))


;; ─────────────────────────────────────────────────────────
;;; Language Modes:

;; JSON mode
(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode)
  :config
  (setq-default js-indent-level 2))


;; MATLAB mode
(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :delight
  (mlint-minor-mode)
  :hook ((matlab-mode . mlint-minor-mode)
         (matlab-mode . visual-line-mode))
  :config
  (setq-default matlab-indent-function-body t))


;; CMake mode highlighting
(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate)
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t))


;; CMake mode
(use-package cmake-mode
  :ensure t
  :defer t)


;; Systemd Unit mode
(use-package systemd
  :ensure t
  :defer t)


;; ─────────────────────────────────────────────────────────
;;; Completion and narrowing frameworks:

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
		 ("C-x C-r" . helm-for-files))
  :init (require 'helm-config)
  :config
  (setq helm-lisp-fuzzy-completion t))


;; Helm interface for GNU Global Tags
(use-package helm-gtags
  :ensure t
  :delight
  :bind
  (:map helm-gtags-mode-map
		("C-<f1>" . helm-gtags-dwim))
  :hook
  ((dired-mode . helm-gtags-mode)
   (c-mode . helm-gtags-mode)
   (c++-mode . helm-gtags-mode)
   (python-mode . helm-gtags-mode)))


;; Helm interface to Xref
(use-package helm-xref
  :ensure t)


;; Company completion mode
(use-package company
  :ensure t
  :bind
  (:map company-mode-map
		("C-<tab>" . company-complete))
  :hook
  ((c++-mode . company-mode)
   (python-mode . company-mode)
   (sh-mode . company-mode)
   (css-mode . company-mode)
   (emacs-lisp-mode . company-mode)
   (matlab-mode . company-mode)))


;; Icons for Company completion
(use-package company-box
  :ensure t
  :delight
  :hook (company-mode . company-box-mode))


;; Company shell mode
(use-package company-shell
  :ensure t
  :config
  (push 'company-shell company-backends))


;; Language Server Protocol mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :bind
  (:map lsp-mode-map
		("S-<f6>" . lsp-rename)
		("<f9>" . lsp-ui-imenu)
  		("M-?" . lsp-find-references)
		("<C-escape>" . lsp-ui-doc-hide))
  :hook
  (((c++-mode python-mode sh-mode) . lsp)
   (lsp-mode . lsp-ui-mode))
  :config
  (require 'lsp-clients)
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-enable-xref t
        lsp-auto-guess-root t))


;; LSP UI Tweaks
(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-imenu-mode-map
		("<f9>" . lsp-ui-imenu--kill))
  :config
  (setq lsp-ui-flycheck-enable t
		lsp-ui-sideline-ignore-duplicate t
		lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 1.25
		lsp-ui-doc-border "white"))


;; LSP backend for Company completion
(use-package company-lsp
  :ensure t
  :commands company-complete
  :bind
  (:map lsp-mode-map
		("C-<tab>" . company-lsp))
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
		company-transformers nil
		company-lsp-cache-candidates nil
        company-lsp-enable-recompletion t))


;; Debugging
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :bind ("C-c C-b" . dap-breakpoint-toggle)
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode t)
  (tooltip-mode t)
  (require 'dap-python))


;; ─────────────────────────────────────────────────────────
;;; Spell checking modes:

;; Flyspell mode
(use-package flyspell
  :ensure t
  :diminish
  :bind
  (("C-{" . flyspell-check-next-highlighted-word)
   ("M-}" . hrm\flyspell-check-previous-highlighted-word))
  :hook
  ((text-mode . flyspell-mode)
   (org-mode . flyspell-mode))
  :config
  (defun hrm\flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)))


;; Flycheck mode
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind
  (("C-{" . flycheck-next-error)
   ("M-}" . flycheck-previous-error)))


;; ─────────────────────────────────────────────────────────
;;; Note-taking and documentation modes:

;; Org mode
(use-package org
  :ensure t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c n" . (lambda () (interactive) (org-capture nil "n")))
   ("C-c m" . (lambda () (interactive) (org-capture nil "m")))
   ("C-c g" . (lambda () (interactive) (org-capture nil "g")))
   ("C-c t" . (lambda () (interactive) (org-capture nil "t"))))
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . (lambda ()
                 "Require export options"
                 (require 'ox-odt)
				 (require 'ox-md)
                 (require 'ox-jira))))
  :config
  (setq org-todos-file "~/org/todos.org"
        org-log-done 'time
        org-agenda-files '("~/org")
		org-src-fontify-natively t)
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline org-todos-file)
           "* TODO %u%? [/]\n" :kill-buffer t)))
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "?" "|" "DONE" "CANCELED")
          (sequence "QUESTION" "DEFECT" "|" "FILED" "RESOLVED")))
  (setq org-agenda-custom-commands
        '(("w" "Completed TODOs this week" agenda ""
           ((org-agenda-span 14)
            (org-agenda-start-on-weekday -7)
            (org-agenda-start-with-log-mode t)
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'notregexp ".*DONE.*:mdt:")))
           ("~/status/done.txt")))))


;; Org mode JIRA export
(use-package ox-jira
  :ensure t)


;; Org presentation mode
(use-package epresent
  :ensure t)


;; ─────────────────────────────────────────────────────────
;;; Display tweaks

;; Diminish mode
(use-package diminish
  :ensure t)


;; Delight mode
(use-package delight
  :ensure t)


;; Smooth scrolling mode
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 15)
  (setq scroll-preserve-screen-position 1))


;; Transpose frame
(use-package transpose-frame
  :ensure t
  :bind ("C-x t" . transpose-frame))


;; Neotree Mode
(use-package neotree
  :ensure t
  :bind ("<f10>" . neotree-toggle)
  :hook
  (neo-after-create . (lambda (unused) (interactive) (linum-mode nil))))


;; Icons
(use-package all-the-icons
  :ensure t
  :defer t)


;; Sudo edit mode
(use-package sudo-edit
  :ensure t
  :bind ("C-c C-r" . sudo-edit))


;; Adaptive Wrap
(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))


;; Display formfeed character (^L) as a line
(use-package page-break-lines
  :ensure t)


;; ─────────────────────────────────────────────────────────
;;; Git modes:

;; Magit
(use-package magit
  :ensure t
  :diminish
  :bind (("C-<f2>" . magit)
         ("C-S-b" . magit-blame)))


;; Git gutter mode
(use-package git-gutter
  :ensure t
  :bind ("C-<f5>" . git-gutter-mode)
  :config
  (git-gutter:linum-setup))


;; ─────────────────────────────────────────────────────────
;;; Project management modes:

;; Projectile
;; (use-package projectile
;;   :ensure t)


;;; End:

(provide 'hrm-use-package)
;;; hrm-use-package ends here
