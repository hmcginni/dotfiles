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


;; Diminish and Delight modes
;;
(use-package diminish
  :ensure t)
(use-package delight
  :ensure t)


;; Packages ---------------------------------------------------------- ;;


;; Built-ins
;;
(use-package emacs
  :delight
  (adaptive-wrap-prefix-mode)
  (visual-line-mode)
  (global-prettify-symbols-mode)
  (eldoc-mode))


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


;; Transpose frame
;;
(use-package transpose-frame
  :ensure t
  :bind ("C-x t" . transpose-frame))


;; Magit
;;
(use-package magit
  :ensure t
  :diminish
  :bind (("C-<f2>" . magit)
         ("C-S-b" . magit-blame)))

;; Helm
;;
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-r" . helm-for-files))
  :init (require 'helm-config)
  :config
  (helm-mode 1)
  (setq helm-lisp-fuzzy-completion t))

;; Helm interface for GNU Global Tags
(use-package helm-gtags
  :ensure t
  :delight
  :bind (("C-<f1>" . helm-gtags-dwim))
  :hook ((dired-mode . helm-gtags-mode)
         (c-mode . helm-gtags-mode)
         (c++-mode . helm-gtags-mode)
         (python-mode . helm-gtags-mode)))


;;-------------------------------------------
;; Flyspell and Flycheck modes
;;
(use-package flyspell
  :ensure t
  :diminish
  :bind (("C-<f9>" . flyspell-check-next-highlighted-word)
         ("M-<f9>" . hrm\flyspell-check-previous-highlighted-word))
  :hook ((c++-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)
         (org-mode . flyspell-mode))
  :config
  (defun hrm\flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)))

;; Flycheck mode
;;
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (("C-<f9>" . flycheck-next-error)
         ("M-<f9>" . flycheck-previous-error)))


;;-------------------------------------------
;; Company and Irony modes
;;
(use-package company
  :ensure t
  :bind ("C-<tab>" . company-complete)
  :hook ((irony-mode . company-mode)
         (emacs-lisp-mode . company-mode)
         (sh-mode . company-mode)
		 (anaconda-mode . company-mode)
         (matlab-mode . company-mode)))

(use-package company-irony
  :ensure t
  :requires (irony company)
  :diminish
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package company-c-headers
  :ensure t
  :diminish
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-c-headers)))

(use-package company-anaconda
  :ensure t
  :diminish
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package flycheck-irony
  :ensure t
  :diminish
  :requires irony
  :hook ((flycheck-mode . flycheck-irony-setup)))


;;----------------------------------------------
;; Python
;;
(use-package anaconda-mode
  :ensure t
  :delight
  (anaconda-mode)
  (anaconda-eldoc-mode)
  :bind ("C-c C-d" . anaconda-mode-show-doc)
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (setq python-shell-interpreter "ipython3"))


;; Smooth scrolling mode
;;
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 15)
  (setq scroll-preserve-screen-position 1))


;;---------------------------------------------
;; Org mode
;;
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
				 (require 'ox-jira)))
   (org-mode . (lambda ()
                 "Beautify Org Checkbox Symbol"
                 (push '("[ ]" . "☐") prettify-symbols-alist)
                 (push '("[X]" . "☑" ) prettify-symbols-alist)
                 (push '("[-]" . "☒" ) prettify-symbols-alist)
                 (prettify-symbols-mode))))
  :init
  (setq org-todos-file "~/org/todos.org"
        org-mitg-file "~/org/mitg.org"
        org-gl-file "~/org/gl.org"
        org-log-done 'time
        org-agenda-files '("~/org"))
  (setq org-capture-templates
        '(("m" "Meeting" entry (file+olp+datetree org-mitg-file)
           "* %?\n \n" :kill-buffer t)
          ("g" "GL Meeting" entry (file+olp+datetree org-gl-file)
           "* %?\n \n" :kill-buffer t)
          ("t" "todo" entry (file+headline org-todos-file)
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
;;
(use-package ox-jira
  :ensure t)

;; Org-bullets mode
;;
(use-package org-bullets
  :ensure t
  :diminish)


;; Neotree Mode
;;
(use-package neotree
  :ensure t
  :bind (("<f9>" . neotree-toggle))
  :hook (neo-after-create . (lambda (_unused)
							  "Disable line numbers"
							  (interactive)
							  (linum-mode -1))))


;; Icons
;;
(use-package all-the-icons
  :ensure t)


;; Systemd Unit mode
;;
(use-package systemd
  :ensure t)


;; JSON mode
;;
(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode)
  :config
  (setq-default js-indent-level 2))


;; MATLAB mode
;;
(use-package matlab-mode
  :ensure t
  :mode ("\\.m$" . matlab-mode)
  :delight
  (mlint-minor-mode)
  :hook ((matlab-mode . mlint-minor-mode)
         (matlab-mode . visual-line-mode))
  :config
  (setq-default matlab-indent-function-body t))


;; Sudo edit mode
;;
(use-package sudo-edit
  :ensure t
  :bind ("C-c C-r" . sudo-edit))


;;------------------------------------------
;; CMake modes
;;
(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate)
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t))

(use-package cmake-mode
  :ensure t)

(use-package cmake-ide
  :ensure t
  :diminish
  :config (cmake-ide-setup))


;; END
;;
(provide 'my-use-package)
;;; my-use-package ends here
