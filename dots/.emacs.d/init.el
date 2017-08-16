;; Custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cursor-type (quote bar))
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(mlint-programs
   (quote
    ("mlint" "/usr/local/MATLAB/R2017a/bin/glnxa64/mlint")))
 '(show-paren-mode t)
 '(tabbar-separator (quote (0.5))))


;; MELPA
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))


;; Add paths
;;
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

  
;; Basic EMACS configurations
;;
;- Appearance
(global-set-key [C-kp-add] 'text-scale-increase)             ;; Dynamic font size {in,de}crease
(global-set-key [C-kp-subtract] 'text-scale-decrease)        ;;         ||
(set-default-font "Consolas-10:spacing=80")                  ;; Font
(add-to-list 'default-frame-alist '(height . 64))            ;; Startup window size
(add-to-list 'default-frame-alist '(width . 110))            ;;         ||
(setq frame-title-format "GNU Emacs 24 · [%b]")               ;; Set title to name of open file
(define-key global-map "\M-q" 'visual-line-mode)             ;; Toggle line wrap
(global-set-key (kbd "<S-mouse-2>") 'menu-bar-mode)          ;; Menu bar mode
(setq sml/theme 'dark)                                       ;; Smart Mode Line Theme
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(require 'linum)                                             ;; Enable line numbers globally
(global-linum-mode 1)                                        ;;             ||
;- Scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)
(setq scroll-preserve-screen-position 1)                     ;; keep cursor at same position when scrolling
(global-set-key (kbd "M-n") (kbd "C-u 4 C-v"))               ;; scroll window up/down by one line
(global-set-key (kbd "M-p") (kbd "C-u 4 M-v"))               ;;               ||
;- Try to fix Emacs colors in tmux
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
   ;; Use the xterm color initialization code.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))
(set-buffer-file-coding-system 'utf-8-dos)                   ;; Windows-style line endings (MedAcuity)
(global-set-key (kbd "C-x t") 'transpose-frame)              ;; Transpose frame
(global-set-key (kbd "C-x M-x b") 'buffer-menu-other-window) ;; List buffers 
(windmove-default-keybindings 'meta)                         ;; Windmove
(setq ediff-window-setup-function                            ;; Ediff stuff
      'ediff-setup-windows-plain)                            ;;      ||


;; Org-mode
;;
(require 'org)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq org-default-notes-file "/home/hmcginnis/Dropbox/Notes/notes.org")
(setq org-default-diary-file "~/Dropbox/Notes/stream-of-consciousness.org")
(define-key global-map "\C-cc" 'org-capture)
;; Define the custum capture templates
(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t :kill-buffer t)
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t :kill-buffer t)
	 ("s" "Stream of Consciousness" entry (file+datetree org-default-diary-file)
	  "* %?\n%U\n" :clock-in t :clock-resume t :kill-buffer t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t :kill-buffer t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t" :kill-buffer t) ))


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


;; Spell-check (flyspell)
;;
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)


;; MATLAB Integration
;;
(autoload 'matlab-mode "matlab" "MATLAB Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches (list "-nosplash" "-nodesktop"))
(add-hook 'matlab-mode
	  (lambda ()
	    (auto-complete-mode 1)
	    (matlab-cedet-setup)
	    (matlab-toggle-show-mlint-warnings)
	    ))


;; Open recent File Menu option
;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 35)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

  
;; Color themes
;;
(defun color-theme-almost-monokai ()
  (interactive)
  (color-theme-install
   '(color-theme-almost-monokai
     ((background-color . "#191919")
      (foreground-color . "#F8F8F2")
      (cursor-color . "#DAD085"))
     (default ((t (nil))))
     (modeline ((t (:background "white" :foreground "black" :box (:line-width 1 :style released-button)))))
     (font-lock-builtin-face ((t (:foreground "#A6E22A"))))
     (font-lock-comment-face ((t (:italic t :foreground "#75715D"))))
     (font-lock-constant-face ((t (:foreground "#A6E22A"))))
     (font-lock-doc-string-face ((t (:foreground "#65B042"))))
     (font-lock-string-face ((t (:foreground "#DFD874"))))
     (font-lock-function-name-face ((t (:foreground "#F1266F" :italic t))))
     (font-lock-keyword-face ((t (:foreground "#66D9EF"))))
     (font-lock-type-face ((t (:underline t :foreground "#89BDFF"))))
     (font-lock-variable-name-face ((t (:foreground "#A6E22A"))))
     (font-lock-warning-face ((t (:bold t :foreground "#FD5FF1"))))
     (highlight-80+ ((t (:background "#D62E00"))))
     (hl-line ((t (:background "#1A1A1A"))))
     (region ((t (:background "#6DC5F1"))))
     (ido-subdir ((t (:foreground "#F1266F"))))
    )
  )
)
(provide 'color-theme-almost-monokai)

(require 'color-theme)
;(color-theme-initialize)
(color-theme-almost-monokai)


;; Highlight current line
;;
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")


;; Reload emacs
;;
(defun reload-emacs-init-file ()
	"reload your init.el file without restarting Emacs"
	(interactive)
	(load-file "~/.emacs.d/init.el") )
(global-set-key (kbd "C-r") 'reload-emacs-init-file)






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
