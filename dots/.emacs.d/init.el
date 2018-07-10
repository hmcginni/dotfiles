;; Custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cursor-type (quote box))
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(irony-additional-clang-options (quote ("-pthread" "-std=c++11")))
 '(mlint-programs
   (quote
    ("mlint" "/usr/local/MATLAB/R2017a/bin/glnxa64/mlint")))
 '(org-clock-into-drawer 2)
 '(org-entities-user (quote (("chcl" "" nil "&#x2610;" "" "" ""))))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-list-allow-alphabetical t)
 '(org-use-sub-superscripts (quote {}))
 '(show-paren-mode t)
 '(sr-speedbar-default-width 30)
 '(sr-speedbar-right-side nil)
 '(tabbar-separator (quote (0.5))))


;; Add paths
;;
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;; Server mode
;;
(server-mode 0)


;; MELPA
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))


;; Speedbar
;;
(require 'sr-speedbar)
(global-set-key (kbd "<f9>") 'sr-speedbar-toggle)

;; Company mode
;;
(add-hook 'after-init-hook 'global-company-mode)


;; Basic EMACS configurations ==================================================
;;

;; Appearance
(global-set-key (kbd "C-=") 'text-scale-increase)          ;; Dynamic font size {in,de}crease
(global-set-key (kbd "C--") 'text-scale-decrease)          ;;         ||
;; (set-default-font "Fantasque Sans Mono:pixelsize=14")      ;; Font
(set-frame-font "SF Mono:pixelsize=13:weight=Semibold")      ;; Font
;; (set-default-font "Roboto Mono:pixelsize=14:weight=regular")      ;; Font

;; (add-to-list 'default-frame-alist '(height . 30))          ;; Startup window size
;; (set-default-font "IBM Plex Mono:pixelsize=12:weight=medium")      ;; Font
;; (add-to-list 'default-frame-alist '(width . 90))          ;;         ||

(define-key global-map (kbd "C-c C-8")
  (lambda()
    (interactive)
    (set-frame-width (selected-frame) 86))) 

(setq frame-title-format
      (list
	   "GNU Emacs • %b • "
	   (getenv "USER")))       ;; ·• Set title to name of open file
(define-key global-map "\M-q" 'visual-line-mode)           ;; Toggle line wrap
(global-set-key (kbd "<S-mouse-2>") 'menu-bar-mode)        ;; Menu bar mode
;; (setq sml/theme 'dark)                                    ;; Smart Mode Line Theme
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(require 'linum)                                           ;; Enable line numbers globally
(global-linum-mode 1)                                      ;;             ||
;; (global-set-key (kbd "<f8>") 'linum-mode)
(set-face-foreground 'linum "#c0c0c0")
(setq linum-format "%4d\u2502")
(setq-default line-spacing 2)
(setq debug-on-error t)
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))

;; Scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 15)
(setq scroll-preserve-screen-position 1)                   ;; keep cursor at same position when scrolling
(global-set-key (kbd "M-n") (kbd "C-u 2 C-v"))             ;; scroll window up/down by one line
(global-set-key (kbd "M-p") (kbd "C-u 2 M-v"))             ;;               ||

;; Try to fix Emacs colors in tmux
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))
;(set-buffer-file-coding-system 'utf-8-dos)                   ;; Windows-style line endings (MedAcuity)
(global-set-key (kbd "C-x t") 'transpose-frame)              ;; Transpose frame
(global-set-key (kbd "C-x M-x b") 'buffer-menu-other-window) ;; List buffers 
(windmove-default-keybindings 'meta)                         ;; Windmove
(setq ediff-window-setup-function                            ;; Ediff stuff
      'ediff-setup-windows-plain)                            ;;      ||

;; Navigation
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

;; Commenting
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
		(setq beg (region-beginning) end (region-end))
	  (setq beg (line-beginning-position) end (line-end-position)))
	(comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-x C-g") 'comment-or-uncomment-region-or-line)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")

;; Copy current buffer to clipboard
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "C-x M-s") 'my-put-file-name-on-clipboard)

;; Switch to previous buffer
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; Reload emacs
(defun reload-emacs-init-file ()
  "reload your init.el file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el") )
(global-set-key (kbd "C-S-r") 'reload-emacs-init-file)

;; Shell Commands ==============================================================
;;

;; Start diff from Command Line
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
		(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; Print date in 'ddMMMyyyy' form
(defun date-command-on-buffer ()
  (interactive)
  (shell-command "printf '%s' $(date +%d%^b%Y)" t)
  (forward-word))
(global-set-key (kbd "C-c C-1") 'date-command-on-buffer)

;; Print date in 'ddMMMyyyy' form
(defun deadline-date ()
  (interactive)
  (shell-command "printf 'DEADLINE: <%s %s>' $(date '+%Y-%m-%d %a')" t)
  (forward-word))
(global-set-key (kbd "C-c C-2") 'deadline-date)

;; Insert html<br>
(defun html-break-on-buffer ()
  (interactive)
  (shell-command "echo \"@@html:<br>@@\"" t))
(global-set-key (kbd "C-c C-<return>") 'html-break-on-buffer)

;; pandoc current buffer to markdown
(defun to-markdown ()
  (interactive)
  (my-put-file-name-on-clipboard)
  (let currentfile ('yank))
  (let filename
	(concat "pandoc -s " currentfile " " currentfile ".md")))



;; Org-mode ====================================================================
;;

(require 'org)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(require 'ox-confluence)

(setq org-meetings-file "~/org/meetings.org")
(setq org-todos-file "~/org/todos.org")
(setq org-default-diary-file "~/org/diary.org")
(setq org-presentations-file "~/org/presentations.org")
(setq org-log-done 'time)									;(setq org-return-follows-link t)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cm"
  (lambda()
    (interactive)
    (org-capture nil "m")))
(define-key global-map "\C-cp"
  (lambda()
    (interactive)
    (org-capture nil "p")))
(define-key global-map "\C-cd"
  (lambda()
    (interactive)
    (org-capture nil "d")))
(define-key global-map "\C-ct"
  (lambda()
    (interactive)
    (org-capture nil "t")))
(define-key global-map "\C-ce"
  (lambda()
    (interactive)
    (org-capture nil "e")))
(define-key global-map (kbd "C-c C-x m")
  (lambda()
    (interactive)
    (find-file org-meetings-file))) 
(define-key global-map (kbd "C-c 1")
  (lambda()
    (interactive)
    (find-file org-todos-file)))
(define-key global-map (kbd "C-c 2")
  (lambda()
    (interactive)
    (find-file org-default-diary-file)))
(define-key global-map (kbd "C-c C-o")
  (lambda()
    (org-open-at-point)
    (delete-window)))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline org-todos-file "Ongoing")
		 "* TODO [#A] %u%? [/]\n\n*Captured from: %a*\n" :clock-in t :clock-resume t :kill-buffer t)
		("m" "meeting" entry (file+datetree org-meetings-file)
		 "* MEETING: %? :MEETING:NOTES:\n:PROPERTIES:\n:EXPORT_TITLE:\n:EXPORT_FILE_NAME:\n:END:\n** Meeting Participants\n\n** Todos and Questions\n\n%t\n" :clock-in t :clock-resume t :kill-buffer t)
		("p" "presentation" entry (file+datetree org-presentations-file)
		 "* %? :PRESENTATION:\n:PROPERTIES:\n:EXPORT_TITLE:\n:EXPORT_FILE_NAME:\n:END:\n%t\n" :clock-in t :clock-resume t :kill-buffer t)
		("d" "diary" entry (file+datetree org-default-diary-file)
		 "* %?\n%U\n" :clock-in t :clock-resume t :kill-buffer t)
		("e" "email" entry (file+datetree org-default-diary-file)
		 "* EMAIL to: %? :EMAIL:\n:PROPERTIES:\n:EXPORT_FILE_NAME: email\n:END:\n%t\n" :clock-in t :clock-resume t :kill-buffer t) ))

(setq org-agenda-files
      (list org-todos-file
			org-default-diary-file
			org-meetings-file
			org-presentations-file
			)
      )

(setq org-refile-targets
      '( (org-meetings-file :level . 3)
		 (org-presentations-file :level . 3)
		 (org-default-diary-file :level . 4)
		 (org-todos-file :maxlevel . 2)
		 )
      )

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(p!)" "|" "DONE(d)" "NO ACTION")))


;; (defun org-html-remove-first-line (html)
;;   (org-map-entries
;;    (lambda ()
;; 	 (org-html-trim-html-for-word))))


(defun org-html-remove-first-line (backend)
  (lambda () (delete-region
			  (progn (beginning-of-buffer) (point))
			  (progn (forward-line) (point)) )))

(add-hook
 'org-export-preprocess-final-hook
 'org-html-remove-first-line)


;; Trim first line of org-mode HTML export
(defun org-html-trim-html-for-word ()
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   "tail -n+2 "
   (current-buffer) t nil))

										; (add-hook
;;  'org-export-preprocess-final-hook
;;  'org-html-remove-first-line)

;; (defun my-headline-removal (backend)
;;   "Remove all headlines in the current buffer.
;; BACKEND is the export back-end being used, as a symbol."
;;   (org-map-entries
;;    (lambda () (delete-region (point) (progn (forward-line) (point))))))



;; (add-hook 'org-export-before-parsing-hook 'my-headline-removal)

;; (shell-command
;; 	 (format "tail -n+2 %s > %s-msword.html"
;; 			 outfile
;; 			 (file-name-sans-extension outfile))))

;; (add-to-list 'org-emphasis-alist
;;              '("*" (:foreground "red")
;;                ))


;; Attempt at modifying PDF export to go through pandoc and wkhtmltopdf
;;     which would allow use of CSS in the PDF export
;;
;; (defun org-html-to-pdf (plist filename pub-dir)
;;   "Export .org file to HTML (using CSS if desired) and then convert to PDF with pandoc"
;;   (let ((outfile
;; 	 (org-publish-org-to 'html filename ".html" plist pub-dir)))
;;     (shell-command (format "pandoc -f html -t pdf -o %s %s.pdf"
;; 			   outfile
;; 			   (file-name-sans-extension outfile)))))


;; Extra Command Line Args =====================================================
;;


;; Spell-check (flyspell)
;;

(global-set-key (kbd "<f7>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "M-<f7>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "C-<f7>") 'flyspell-check-next-highlighted-word)


;; Company-mode and Irony-mode ==================================================
;;

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq tab-width 4
	  c-default-style "linux"
	  indent-tabs-mode . nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c++-mode-hook 'global-flycheck-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)


(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(global-set-key [C-tab] 'company-complete)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))



;; MATLAB Integration ==========================================================
;;

(autoload 'matlab-mode "matlab" "MATLAB Editing Mode" t)

(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))

(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches
	  (list "-nosplash" "-nodesktop"))

(add-hook 'matlab-mode
		  (lambda ()
			(auto-complete-mode 1)
			(matlab-cedet-setup)
			(matlab-toggle-show-mlint-warnings)
			))


;; Open recent File Menu option ================================================
;;

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 35)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; Color themes ================================================================
;;

(defun color-theme-almost-monokai ()
  (interactive)
  (color-theme-install
   '(color-theme-almost-monokai
     ((background-color . "#191919")
      (foreground-color . "#F8F8F2")
      (cursor-color . "#dddddd"))
     (default ((t (nil))))
     (modeline ((t (:background "white" :foreground "black" :box (:line-width 1 :style released-button)))))
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
     (ido-subdir ((t (:foreground "#F1266F"))))
	 )
   )
  )
(provide 'color-theme-almost-monokai)

(require 'color-theme)
										;(color-theme-initialize)
(color-theme-almost-monokai)


;; Tabbar
;; 
;; This are setting for nice tabbar items
;; to have an idea of what it looks like http://imgur.com/b0SNN
;; inspired by Amit Patel screenshot http://www.emacswiki.org/pics/static/NyanModeWithCustomBackground.png
;;
;;(global-semantic-stickyfunc-mode -1)
;; (require 'tabbar)
;; ;; Tabbar settings
;; (set-face-attribute
;;  'tabbar-default nil
;;  :background "gray20"
;;  :foreground "gray20"
;;  :box '(:line-width 1 :color "gray20" :style nil))
;; (set-face-attribute
;;  'tabbar-unselected nil
;;  :background "gray30"
;;  :foreground "white"
;;  :box '(:line-width 5 :color "gray30" :style nil))
;; (set-face-attribute
;;  'tabbar-selected nil
;;  :background "gray75"
;;  :foreground "black"
;;  :box '(:line-width 5 :color "gray75" :style nil))
;; (set-face-attribute
;;  'tabbar-highlight nil
;;  :background "white"
;;  :foreground "black"
;;  :underline nil
;;  :box '(:line-width 5 :color "white" :style nil))
;; (set-face-attribute
;;  'tabbar-button nil
;;  :box '(:line-width 1 :color "gray20" :style nil))
;; (set-face-attribute
;;  'tabbar-separator nil
;;  :background "gray20"
;;  :height 0.6)


;; ;; Tab keybindings
;; ;;
;; (global-set-key (kbd "C-M-<right>") 'tabbar-forward)
;; (global-set-key (kbd "C-M-<left>") 'tabbar-backward)

;; ;; Change padding of the tabs
;; ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;; ;;

;; ;; adding spaces
;; ;;
;; (defun tabbar-buffer-tab-label (tab)
;;   "Return a label for TAB.
;; That is, a string used to represent it on the tab bar."
;;   (let ((label  (if tabbar--buffer-show-groups
;;                     (format "[%s]  " (tabbar-tab-tabset tab))
;;                   (format "%s  " (tabbar-tab-value tab)))))
;;     ;; Unless the tab bar auto scrolls to keep the selected tab
;;     ;; visible, shorten the tab label to keep as many tabs as possible
;;     ;; in the visible area of the tab bar.
;;        ;;
;;     (if tabbar-auto-scroll-flag
;;         label
;;       (tabbar-shorten
;;        label (max 1 (/ (window-width)
;;                        (length (tabbar-view
;;                                 (tabbar-current-tabset)))))))))

;; (tabbar-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
