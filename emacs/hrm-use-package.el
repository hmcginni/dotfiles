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
;;; Package packages

;; Built-ins
(use-package emacs
  :diminish
  (helm-mode
   eldoc-mode
   visual-line-mode
   hs-minor-mode)
  :hook
  ((prog-mode . hs-minor-mode)
   (text-mode . hs-minor-mode)
   (emacs-lisp-mode . prettify-symbols-mode)
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
;;; Language modes

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
  :diminish
  (mlint-minor-mode
   matlab-functions-have-end-minor-mode)
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


;; C++ syntax highlighting mode
(use-package modern-cpp-font-lock
  :ensure t
  :defer t)


;; Python Sphinx docstring mode
(use-package sphinx-doc
  :ensure t
  :delight
  :hook (python-mode . sphinx-doc-mode)
  :config (sphinx-doc-mode t))

;; ─────────────────────────────────────────────────────────
;;; Helm Modes

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
		 ("C-x C-r" . helm-for-files)
		 ("C-?" . helm-apropos)
		 ("C-<f1>" . xref-find-definitions))
  :init (require 'helm-config)
  :config
  (setq helm-lisp-fuzzy-completion t))


;; Helm interface for GNU Global Tags
(use-package helm-gtags
  :ensure t
  :delight
  :bind
  (:map helm-gtags-mode-map
		("C-c C-<f1>" . helm-gtags-dwim))
  :hook
  ((dired-mode . helm-gtags-mode)
   (c-mode . helm-gtags-mode)
   (c++-mode . helm-gtags-mode)
   (python-mode . helm-gtags-mode)))


;; Helm interface to Xref
(use-package helm-xref
  :ensure t
  :after helm)


;; ─────────────────────────────────────────────────────────
;; Company Modes

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
   (matlab-mode . company-mode))
  :config
  (setq company-idle-delay 1
		company-tooltip-limit 20))


;; Icons for Company completion
(use-package company-box
  :ensure t
  :delight
  :hook (company-mode . company-box-mode))


;; Company shell mode
(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))


;; ─────────────────────────────────────────────────────────
;; LSP Modes

;; Language Server Protocol mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind
  (:map lsp-mode-map
		("S-<f6>" . lsp-rename)
		("<f9>" . lsp-ui-imenu))
  :hook
  (((c++-mode python-mode) . lsp)
   (lsp . (lsp-ui-mode lsp-enable-imenu)))
  :config
  (require 'lsp-clients)
  (setq lsp-restart 'auto-restart
		lsp-enable-imenu t
		lsp-enable-semantic-highlighting t
		lsp-enable-snippet nil
        lsp-enable-xref t
		lsp-response-timeout 15
		lsp-pyls-plugins-pydocstyle-enabled t
		lsp-pyls-plugins-pyflakes-enabled t
		lsp-pyls-plugins-pylint-enabled nil
		lsp-signature-render-all t
		lsp-imenu-show-container-name t
        lsp-prefer-flymake nil
        lsp-auto-guess-root t))


;; LSP UI Tweaks
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
  		("M-?" . lsp-ui-peek-find-references)
		("M-." . lsp-ui-peek-find-definitions)
		("<escape>" . lsp-ui-doc-hide))
  (:map lsp-ui-imenu-mode-map
		("<f9>" . lsp-ui-imenu--kill))
  :hook
  (lsp-ui-doc-frame . (lambda (frame _w)
						(set-face-attribute
						 'default frame
						 :height 80
						 :family "IBM Plex Sans")))
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-sideline-enable t
		lsp-ui-sideline-ignore-duplicate t
		lsp-ui-sideline-show-code-actions t
		lsp-ui-sideline-show-hover t
		lsp-ui-sideline-show-symbol t
		lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 1
		lsp-ui-doc-border "gray20"))


;; LSP backend for Company completion
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :bind
  (:map lsp-mode-map
		("C-<tab>" . company-lsp))
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
		company-transformers nil
		company-lsp-cache-candidates t
        company-lsp-enable-recompletion t))


;; Debugging
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :delight
  :bind (:map dap-mode-map
			  ("C-c C-b" . dap-breakpoint-toggle)
			  ("S-<f5>" . dap-debug)
			  ("<f5>" . dap-continue)
			  ("<f10>" . dap-next)
			  ("<f11>" . dap-step-in)
			  ("S-<f11>" . dap-step-out))
  :config
  (require 'dap-python)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode t)
  (tooltip-mode t))


;; ─────────────────────────────────────────────────────────
;;; Spell checking modes

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
;;; Note-taking and documentation modes

;; Org mode
(use-package org
  :ensure t
  :bind
  (:map org-mode-map
		("C-x C-k" . org-insert-link-global))
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


;; Org presentation mode
(use-package epresent
  :ensure t
  :defer t)


;; ─────────────────────────────────────────────────────────
;;; Navigation

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'helm))


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
  :hook (neo-after-create . (lambda (&rest _) (display-line-numbers-mode -1)))
  :config
  (setq neo-window-width 30))


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


;; ─────────────────────────────────────────────────────────
;;; Git modes

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
;;; Project management modes

;; Projectile
;; (use-package projectile
;;   :ensure t)


;;; End:

(provide 'hrm-use-package)
;;; hrm-use-package ends here
