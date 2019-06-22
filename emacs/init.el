;; Custom
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-wrap-extra-indent 3)
 '(column-number-mode t)
 '(cursor-type (quote (bar . 1)))
 '(custom-safe-themes t)
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-theme-load-path (quote (custom-theme-directory t)) t)
 '(debug-on-error nil)
 '(display-time-mode nil)
 '(fill-column 80)
 '(flycheck-checker-error-threshold 500)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-flake8rc "~/.config/flake8/.flake8")
 '(flycheck-matlab-mlint-executable nil)
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-pylint-executable "pylint3")
 '(flycheck-shellcheck-follow-sources t)
 '(font-use-system-font t)
 '(fringe-mode (quote (10 . 20)) nil (fringe))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message
   (concat "#+OPTIONS: toc:nil num:nil \\n:nil ::t -:t

* "
		   (shell-command-to-string "printf '%s' $(date +%Y%m%d)")
		   ": "))
 '(irony-additional-clang-options (quote ("-pthread" "-std=c++11")))
 '(line-spacing 1)
 '(linum-format "%4d  ")
 '(matlab-functions-have-end t)
 '(matlab-indent-function-body t)
 '(matlab-shell-command-switches (quote ("-nojvm")))
 '(menu-bar-mode nil)
 '(mlint-programs (quote ("/opt/matlab/2017a/bin/glnxa64/mlint")))
 '(org-clock-into-drawer 2)
 '(org-entities-user (quote (("chcl" "" nil "&#x2610;" "" "" ""))))
 '(org-export-headline-levels 4)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-file-apps
   (quote
	((auto-mode . emacs)
	 ("\\.mm\\'" . default)
	 ("\\.x?html?\\'" . default)
	 ("\\.pdf\\'" . "evince %s"))))
 '(org-reverse-note-order t)
 '(org-use-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
	(fill-column-indicator visual-fill-column magit delight ox-gfm adaptive-wrap-mode helm-gtags format-all github-theme dired-toggle sudo-edit matlab-mode markdown-mode company-anaconda flycheck-pycheckers json-mode anaconda-mode company-box csv-mode cmake-font-lock cmake-ide cmake-mode systemd org-bullets neotree adaptive-wrap ox-jira smooth-scrolling flycheck-irony company-c-headers company-irony company transpose-frame sr-speedbar helm auto-package-update diminish use-package)))
 '(python-indent-guess-indent-offset nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-menu-items 20)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(speedbar-frame-parameters
   (quote
	((minibuffer)
	 (width . 15)
	 (border-width . 0)
	 (menu-bar-lines . 0)
	 (tool-bar-lines . 0)
	 (unsplittable . t)
	 (left-fringe . 0))))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(speedbar-verbosity-level 0)
 '(sr-speedbar-default-width 30)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))


;; ========================================================================== ;;
;; Package Init/Install
;;

(load "~/.emacs.d/my-use-package.el")


;; ========================================================================== ;;
;; Custom Lisp Functions
;;

(load "~/.emacs.d/my-lisp-functions.el")


;; ========================================================================== ;;
;; EMACS configurations
;;

;; Start server
;;
(server-start)

;; Set variables
;;
(setq-default tab-width 4
              c-basic-offset 4
              indent-tabs-mode t
              c-default-style "linux"
              ediff-window-setup-function 'ediff-setup-windows-plain
              frame-title-format (list "GNU Emacs • %b • "
                                       (getenv "USER"))
              speedbar-initial-expansion-list-name "buffers")

;; Global keyboard shortcuts
;;
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<S-mouse-2>") 'menu-bar-mode)
(global-set-key (kbd "M-;") 'visual-line-mode)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer)
(global-set-key (kbd "C-j") 'fill-paragraph)
(global-set-key (kbd "M-p") (kbd "C-u 3 M-v")) ; ----- scroll page behind cursor
(global-set-key (kbd "M-n") (kbd "C-u 3 C-v"))
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)

(global-set-key (kbd "s-<up>") 'hrm/move-line-up)
(global-set-key (kbd "s-<down>") 'hrm/move-line-down)
(global-set-key (kbd "C-c C-<left>") (lambda () (interactive) (hrm/resize "narrow")))
(global-set-key (kbd "C-c C-<right>") (lambda () (interactive) (hrm/resize "wide")))
(global-set-key (kbd "C-x n f") 'hrm/narrow-to-eof)
(global-set-key (kbd "C-c M-d") 'hrm/date-command-on-buffer)
(global-set-key (kbd "C-S-r") 'hrm/reload-emacs-init-file)
(global-set-key (kbd "C-b") 'hrm/switch-to-previous-buffer)
(global-set-key (kbd "C-x C-g") 'hrm/toggle-comment-region)
(global-set-key (kbd "<f8>") 'hrm/toggle-theme)
(global-set-key (kbd "C-c C-b") 'hrm/switch-to-scratch)
(global-set-key (kbd "C-x n i") 'hrm/narrow-to-defun-indirect)


;; =============================================================================
;; Hooks
;;

(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)


;; =============================================================================
;; Start
;;

;; Theme
;;
(hrm/set-theme nil)

;; Font
;;
(if (display-graphic-p)
    (hrm/set-scaled-font "Roboto Mono" "medium"))

;; Auto-mode-alist
;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Customize ===================================================================
;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "#a0a0a0"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(speedbar-button-face ((t (:foreground "green4" :height 70 :family "Roboto"))))
 '(speedbar-directory-face ((t (:foreground "dim gray" :height 90 :family "Roboto"))))
 '(speedbar-file-face ((t (:foreground "cyan4" :height 90 :family "Roboto"))))
 '(speedbar-highlight-face ((t (:background "light gray" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-selected-face ((t (:foreground "red" :height 90 :family "IBM Plex Sans"))))
 '(speedbar-separator-face ((t (:foreground "white" :overline "gray" :family "IBM Plex Sans")))))
(put 'narrow-to-region 'disabled nil)
