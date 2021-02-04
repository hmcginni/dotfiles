;;; hrm-use-package.el --- Emacs Init File - use-package declarations

;;; Commentary:
;;    Emacs Initialization with use-package.

;;; Code:

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)


;; Add paths
(let ((default-directory "~/.emacs.d/elpa")))


;; Enable use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'bind-key)


;; ─────────────────────────────────────────────────────────
;;; Package packages

;; Built-ins
(use-package emacs
  :diminish (helm-mode
			 eldoc-mode
			 visual-line-mode
			 hs-minor-mode)
  :hook ((prog-mode . hs-minor-mode)
		 ((emacs-lisp-mode org-mode) . prettify-symbols-mode)
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
;;; Emacs configuration packages

;; Which-key mode
;; (use-package which-key
;;   :ensure t)


;; ─────────────────────────────────────────────────────────
;;; Languages

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
  :diminish (mlint-minor-mode
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
(use-package cmake-mode :ensure t :defer t)


;; Systemd Unit mode
(use-package systemd :ensure t :defer t)


;; C++ syntax highlighting mode
(use-package modern-cpp-font-lock :ensure t :defer t)


;; Python Sphinx docstring mode
(use-package sphinx-doc
  :ensure t
  :delight
  :hook (python-mode . sphinx-doc-mode)
  :config (sphinx-doc-mode t))


;; Python "Black" formatter
(use-package python-black :ensure t :after python)

;; ─────────────────────────────────────────────────────────
;;; Narrowing Lists (Helm)

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
		 ("C-x C-r" . helm-for-files))
  :init (require 'helm-config)
  :config
  (setq helm-candidate-number-limit 100
		helm-ff-skip-boring-files t
		helm-lisp-fuzzy-completion t
		helm-locate-command "locate -b %s -e -A --regex %s"))


;; Helm interface for GNU Global Tags
(use-package helm-gtags
  :ensure t
  :delight
  :bind (:map helm-gtags-mode-map
			  ("C-x C-<f1>" . helm-gtags-dwim))
  :hook ((dired-mode . helm-gtags-mode)
		 (c-mode . helm-gtags-mode)
		 (c++-mode . helm-gtags-mode)
		 (python-mode . helm-gtags-mode)))


;; Helm interface to Xref
(use-package helm-xref :ensure t :after helm)


;; ─────────────────────────────────────────────────────────
;; Completion

;; Company completion mode
(use-package company
  :ensure t
  :bind (:map company-mode-map
			  ("C-<tab>" . company-capf))
  :hook (after-init . global-company-mode)
  :config
  (require 'company-capf)
  (setq company-idle-delay 1
		company-tooltip-limit 15))

;; ;; Company-mode icons
;; (use-package company-box
;;   :diminish
;;   :ensure t
;;   :hook (company-mode . company-box-mode))
  


;; ─────────────────────────────────────────────────────────
;; Language Servers

;; Language Server Protocol mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
			  ("<f2>" . lsp-rename)
			  ("<f9>" . lsp-ui-imenu))
  :hook (((c++-mode python-mode) . lsp)
		 (lsp . lsp-ui-mode))
  :config
  (setq lsp-restart 'auto-restart
		lsp-enable-semantic-highlighting t
		lsp-enable-snippet nil
        lsp-enable-xref t
		lsp-response-timeout 5
		lsp-pyls-plugins-pydocstyle-enabled t
		lsp-pyls-plugins-pyflakes-enabled t
		lsp-pyls-plugins-pylint-enabled nil
		lsp-signature-render-all t
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
  :config
  (setq lsp-ui-flycheck-enable nil
        lsp-ui-sideline-enable t
		lsp-ui-sideline-ignore-duplicate t
		lsp-ui-sideline-show-code-actions t
		lsp-ui-sideline-show-symbol t
		lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 1
		lsp-ui-doc-border "gray20"))


;; ─────────────────────────────────────────────────────────
;;; Spell checking modes

;; Flyspell mode
(use-package flyspell
  :ensure t
  :diminish
  :hook
  ((text-mode . flyspell-mode)
   (org-mode . flyspell-mode)))


;; Flycheck mode
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; ─────────────────────────────────────────────────────────
;;; Org-related packages

(defun hrm/org-show-todos ()
  "Show `org-agenda' TODO list, grouped by priority."
  (interactive)
  (org-agenda nil "T")
  (beginning-of-buffer))

(use-package org
  :ensure t
  :hook (org-mode . visual-line-mode)
  :bind (("C-c a" . org-agenda)
		 ("C-c C-t" . hrm/org-show-todos))
  :config
  (setq org-mood-log-file "~/Dropbox/org/mood.org"
        org-log-done 'time
        org-agenda-files '("~/org/todos.org" "~/Dropbox/org/todos.org")
		org-src-fontify-natively t
		org-todo-keywords '((sequence "todo" "in progress" "|" "done" "canceled"))))


;; Org Jira export backend
;; (use-package ox-jira :ensure t :defer t)


;; Org presentation mode
(use-package epresent :ensure t :defer t)


;; Org Query Language
(use-package org-ql :ensure t :defer t)


;; Customize TODO item priorities
(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :ensure t
  :defer t)


;; ─────────────────────────────────────────────────────────
;;; Display tweaks

;; Diminish mode
(use-package diminish :ensure t)


;; Delight mode
(use-package delight :ensure t)


;; ;; Indentation markers
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :hook (python-mode . highlight-indent-guides-mode)
;;   :config
;;   (set-face-background 'highlight-indentation-face "#626d82"))


;; Smooth scrolling mode
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 10
		scroll-preserve-screen-position nil))


;; Transpose frame
(use-package transpose-frame
  :ensure t
  :bind ("C-x '" . transpose-frame))


;; ;; Neotree Mode
;; (use-package neotree
;;   :ensure t
;;   :bind ("<f10>" . neotree-toggle)
;;   :hook (neo-after-create . (lambda (&rest _) (display-line-numbers-mode -1)))
;;   :config
;;   (setq neo-window-width 20))


;; Icons
(use-package all-the-icons :ensure t :defer t)


;; Sudo edit mode
(use-package sudo-edit
  :ensure t
  :bind ("C-c C-r" . sudo-edit))


;; Adaptive Wrap
(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))


;;; End:

(provide 'hrm-use-package)
;;; hrm-use-package ends here
