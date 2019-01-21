;; Custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cursor-type (quote box))
 '(custom-safe-themes
   (quote
    ("a4c2c731654f9269f07044bb893bf88f1263e4bee317661638e7c794a6f32f23" "92588014550aedbbecc6f1be9f65fcda99e3733fe7c620bd885ca53614c5b9d2" "05d2292db5ae6531a279f33a61754e7ac5ac6db3348d87389df3f9e7638d8712" "aed525734775471a0feb7787e5f412abfca25885c6ccac49780315d8b5cc97aa" "2313b7550e9143cc315a363b7660ccfa055d14c0dbc4ed1694c3c6018152fcfd" "2c2bd5a09b6738cd50978e9e9a705f1b2209ab6b25144ccb8c7ddc3c86833f3d" "59fe6fab028ae15105e3a0317aa2973caf47485e7909d20732e8533618bb538c" "dfcd0579ccb40fa8682afbb05d51f99d7b20c095e0b66155c4b239523cff65ae" "b25a447790ff23f81bb3805e8ff4aa603568facfa516e36d4cbd7c2c9347fe55" "37d1b503efa46c7998930bd7611f78ec91c43c82a22799881c588b968a0f04c3" "455b282ed6e8fd47821c065c7d274c28f96e7805df5e383ca7f75dc4d852959a" "ffff2f8b99614d0e3d8d1b143d6cc53647760d5cea49f5314166be68f0d59c57" "c6e9656d02f2d3411536633282cb2b5677586b5410b637e28e3e17bac779a142" "504fbb22f7f6fd96e417401d25c6d6b884eb6b36880db7c7e0a0be6356c63fa2" "8b0ec40d36a2216a64788ddecd94bbc260c2ed61c3a9cfb50e7e4a395ab8e55e" "63743f5499643852e14bced8a0d089aa6c578a2e7bbae4d603b6f32c6fae367c" "4464d77656ba267984d8ab41338eab72548b92445005bf6bd5d7c64db834ca17" "7cbae9092a2138833ec757bd7547111bc29daf10cec8013f1b51d359ba547c99" "43bede8b8b3c9f35562ce029d80ee71c1c67e7769c4028ba647c773385c6aa76" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-theme-load-path (quote (custom-theme-directory t)))
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(irony-additional-clang-options (quote ("-pthread" "-std=c++11")))
 '(line-spacing nil)
 '(org-clock-into-drawer 2)
 '(org-entities-user (quote (("chcl" "" nil "&#x2610;" "" "" ""))))
 '(org-export-headline-levels 4)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-reverse-note-order t)
 '(org-use-sub-superscripts (quote {}))
 '(show-paren-mode t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 16)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(speedbar-verbosity-level 0)
 '(sr-speedbar-default-width 30)
 '(sr-speedbar-right-side nil)
 '(text-scale-mode-step 1.1))


;; Package Init ================================================================
;;


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


;; PACKAGE INIT/INSTALL ========================================================
;;


;; Recent file list
;;
(use-package recentf
  :ensure t
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 35))

  
;; Automatically update packages
;;
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


;; Persistent Scratch
;;
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))


;; Speedbar
;;
(use-package sr-speedbar
  :ensure t
  :bind ("<f9>" . sr-speedbar-toggle))


;; Transpose frame
;;
(use-package transpose-frame
  :ensure t
  :bind ("C-x t" . transpose-frame))


;; Company mode
;;
(use-package company
  :ensure t
  :bind ("C-<tab>" . company-complete)
  :hook ((after-init . global-company-mode)
         (c++-mode . company-mode)
         (c-mode . company-mode)
         (emacs-lisp-mode . company-mode))
  :config
  (use-package company-irony
    :ensure t
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))
  (use-package company-c-headers
    :ensure t
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-c-headers))))


;; Irony mode
;;
(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))


;; Smooth scrolling mode
;;
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 15)
  (setq scroll-preserve-screen-position 1))


;; Org mode
;;
(use-package org
  :ensure t)


;; Org-mode JIRA export
;;
(use-package ox-jira
  :ensure t
  :defer t)


;; Flycheck mode
;;
(use-package flycheck
  :ensure t
  :hook ((c++-mode . global-flycheck-mode)
         (flycheck-mode . flycheck-irony-setup)))


;; MATLAB mode
;;
(use-package matlab-mode
  :mode "\\.m$"
  :interpreter "MATLAB"
  :hook (matlab-mode . (lambda ()
                         (auto-complete-mode 1)
                         (matlab-cedet-setup)
                         (matlab-toggle-show-mlint-warnings))))

;;   :config
;;   (autoload 'matlab-mode "matlab" "MATLAB Editing Mode" t)
;;   (add-to-list
;;    'auto-mode-alist
;;    '("\\.m$" . matlab-mode))
;;   (setq matlab-indent-function t)
;;   (setq matlab-shell-command "matlab")
;;   (setq matlab-shell-command-switches
;; 	(list "-nosplash" "-nodesktop")))


;; EMACS configurations ========================================================
;;


;; Server mode
;;
(server-mode 0)


;; Global keyboard shortcuts
;;
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<S-mouse-2>") 'menu-bar-mode)
(global-set-key (kbd "M-q") 'visual-line-mode)


;; Font
;;
(set-default-font "SF Mono:pixelsize=12:weight=medium")
;; (set-default-font "Fantasque Sans Mono:pixelsize=15")
;; (set-default-font "Roboto Mono:pixelsize=13:weight=regular")
;; (set-default-font "IBM Plex Mono:pixelsize=12:weight=medium")

;; Resize Emacs
;;
(define-key global-map (kbd "C-c C-8")
  (lambda()
    (interactive)
    (set-frame-width (selected-frame) 86)))

;; Set Emacs Title
;;
(setq frame-title-format
      (list
	   "GNU Emacs • %b • "
	   (getenv "USER")))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(require 'linum)
(global-linum-mode 1)
(global-visual-line-mode t)

(set-face-foreground 'linum "#c0c0c0")
(setq linum-format "%4d\u2502")
(setq-default line-spacing 2)
(setq debug-on-error t)
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))
(add-hook 'text-mode-hook 'flyspell-mode)

;; Try to fix Emacs colors in tmux
;;
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))

(setq ediff-window-setup-function
      'ediff-setup-windows-plain)

;; Navigation
;;
(global-set-key (kbd "M-n") (kbd "C-u 2 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 2 M-v"))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))


(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-<up>") 'move-line-up)
(global-set-key (kbd "s-<down>") 'move-line-down)

(defun narrow-to-eof ()
  "Narrow from (point) to end-of-file"
  (interactive)
  (save-excursion
    (narrow-to-region (- (point)
                         (current-column))
                      (point-max))))

(global-set-key (kbd "C-x n f") 'narrow-to-eof)


;; Commenting
;;
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
		(setq beg (region-beginning)
              end (region-end))
	  (setq beg (line-beginning-position)
            end (line-end-position)))
	(comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-x C-g") 'comment-or-uncomment-region-or-line)


;; Highlight current line
;;
(global-hl-line-mode 1)


;; Copy current buffer to clipboard
;;
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region
         (point-min)
         (point-max)))
      (message filename))))

(global-set-key (kbd "C-x M-s") 'my-put-file-name-on-clipboard)


;; Switch to previous buffer
;;
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-b") 'switch-to-previous-buffer)


;; Reload emacs
;;
(defun reload-emacs-init-file ()
  "reload your init.el file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el") )

(global-set-key (kbd "C-S-r") 'reload-emacs-init-file)


;; Shell Commands ==============================================================
;;


;; Print date in 'ddMMMyyyy' form
;;
(defun date-command-on-buffer ()
  (interactive)
  (shell-command "printf '%s' $(date +%Y%m%d)" t)
  (forward-word))

(global-set-key (kbd "C-c M-d") 'date-command-on-buffer)


;; Org-mode ====================================================================
;;

(setq-default major-mode 'org-mode)

(setq org-todos-file "~/org/todos.org")
(setq org-default-diary-file "~/org/diary.org")
(setq org-log-done 'time)
(setq debug-on-message
      "Template is not a valid Org entry or tree")
(global-set-key [C-iso-lefttab] 'pcomplete)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map (kbd "C-c d")
  (lambda ()
    (interactive)
    (org-capture nil "d")))
(define-key global-map (kbd "C-c t")
  (lambda ()
    (interactive)
    (org-capture nil "t")))
(define-key global-map (kbd "C-c C-t")
  (lambda ()
    (interactive)
    (find-file org-todos-file)))
(define-key global-map (kbd "C-c C-d")
  (lambda ()
    (interactive)
    (find-file org-default-diary-file)))
(define-key global-map (kbd "C-c C-o")
  (lambda ()
    (org-open-at-point)
    (delete-window)))

(defun generate-new-filename (path)
  (let ((name (read-string
               "File name: ")))
    (expand-file-name
     (format "%s-%s.org"
             (format-time-string "%Y%m%d")
             name)
     path)))

(setq org-capture-templates
      '(("t" "todo" entry
         (file+headline org-todos-file "Unfiled")
         "* TODO %u%? [/]\n\n*Captured from: %a*\n" :clock-in t :clock-resume t :kill-buffer t)
        ("d" "diary" entry
         (file+olp+datetree org-default-diary-file)
         "* %?\n%U\n\nCaptured from: %a*\n" :clock-in t :clock-resume t :kill-buffer t)))

(add-to-list 'org-structure-template-alist
             '("H" "#+STARTUP: overview\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"/home/hrm/org/hrm.css\"/>\n#+LATEX_CLASS: beamer\n#+LATEX_CLASS_OPTIONS: [presentation]\n#+BEAMER_THEME: metropolis\n#+OPTIONS: toc:nil title:nil num:nil \n:nil ::t -:t\n#+TITLE:\n#+AUTHOR: Hassan McGinnis\n#+DATE: %u\n\n* "))

(setq org-agenda-files
      (list org-todos-file
            org-default-diary-file))

(setq org-refile-targets
      '( (org-default-diary-file :level . 4)
		 (org-todos-file :maxlevel . 2)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(p!)" "|" "DONE(d)" "NO ACTION")))

;; Refile shortcuts
(defun refile-to (headline)
  (interactive)
  (let ((pos (save-excursion
               org-todos-file
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline org-todos-file nil pos))))

(global-set-key (kbd "C-c C-1") (lambda () (interactive) (refile-to "Today")))
(global-set-key (kbd "C-c C-2") (lambda () (interactive) (refile-to "This Week")))
(global-set-key (kbd "C-c C-3") (lambda () (interactive) (refile-to "Next Week")))
(global-set-key (kbd "C-c C-4") (lambda () (interactive) (refile-to "Low Priority")))

;; Start diff from Command Line
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
		(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; Spell-check (flyspell)
;;

(global-set-key (kbd "<f7>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "M-<f7>") 'flyspell-check-previous-highlighted-word)

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "C-<f7>") 'flyspell-check-next-highlighted-word)


;; Company-mode and Irony-mode ==================================================
;;

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq tab-width 4
	  c-default-style "linux"
	  indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)





;; Color themes ================================================================
;;

(defun color-theme-almost-monokai ()
  (interactive)
  (color-theme-install
   '(color-theme-almost-monokai
     ((background-color . "#191919")
      (foreground-color . "#F8F8F2")
      (cursor-color . "#e0e0e0"))
     (default ((t (nil))))
     ;; (mode-line ((t (:font "IBM Plex Sans:pixelsize=13:slant=italic:weight=medium" :background "#505050" :foreground "#F8F8F8" :box (:line-width 6 :color "#191919") ))))
     (font-lock-builtin-face ((t (:foreground "#A6E22A"))))
     (font-lock-comment-face ((t (:italic t :foreground "#75715D"))))
     (font-lock-constant-face ((t (:foreground "#A6E22A"))))
     (font-lock-doc-string-face ((t (:foreground "#65B042"))))
     (font-lock-string-face ((t (:foreground "#c9b980")))) ;dfd874
     (font-lock-function-name-face ((t (:foreground "#F1266F" :italic t))))
     (font-lock-keyword-face ((t (:foreground "#66D9EF"))))
     (font-lock-type-face ((t (:foreground "#89BDFF"))))
     (font-lock-variable-name-face ((t (:foreground "#A6E22A"))))
     (font-lock-warning-face ((t (:bold t :foreground "#FD5FF1"))))
     (highlight-80+ ((t (:background "#D62E00"))))
     (hl-line ((t (:background "#333333"))))
     (region ((t (:background "#6DC5F1"))))
     (ido-subdir ((t (:foreground "#F1266F")))))))

(provide 'color-theme-almost-monokai)
;; (require 'color-theme)
;; (color-theme-almost-monokai)
(load-theme 'atom-one-light t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(speedbar-button-face ((t (:foreground "green4" :height 70 :family "IBM Plex Sans"))))
 '(speedbar-directory-face ((t (:foreground "dim gray" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-file-face ((t (:foreground "cyan4" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-highlight-face ((t (:background "light gray" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-selected-face ((t (:foreground "red" :height 90 :family "IBM Plex Sans")))))
(put 'narrow-to-region 'disabled nil)
