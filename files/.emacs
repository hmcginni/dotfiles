(global-set-key [C-kp-add] 'text-scale-increase)
(global-set-key [C-kp-subtract] 'text-scale-decrease)


;; PuTTY fix. Ugly. Bad. But it works. (Good)
;;
(define-key input-decode-map "\e\eOA" (kbd "<M-up>"))
(define-key input-decode-map "\e\eOB" (kbd "<M-down>"))
(define-key input-decode-map "\e\eOC" (kbd "<M-right>"))
(define-key input-decode-map "\e\eOD" (kbd "<M-left>"))


;; MELPA
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))


;; Custom
;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tabbar-separator (quote (0.5))))


;; Add paths
;;
(let ((default-directory "~/.emacs.d/addons/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

  
;; Dynamically change font size
;;
(global-set-key [C-kp-add] 'text-scale-increase)
(global-set-key [C-kp-subtract] 'text-scale-decrease)
(set-default-font "Consolas-10:spacing=80")


;; Ediff stuff
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; Windmove
;;
(windmove-default-keybindings 'meta)
;; (global-set-key (kbd "<M-left>") 'windmove-left)          ; move to left window
;; (global-set-key (kbd "<M-right>") 'windmove-right)        ; move to right window
;; (global-set-key (kbd "<M-up>") 'windmove-up)              ; move to upper window
;; (global-set-key (kbd "<M-down>") 'windmove-down)          ; move to lower window


;; Transpose frame
;;
(global-set-key (kbd "C-x t") 'transpose-frame)


;; Menu bar mode
;;
(global-set-key (kbd "<S-mouse-2>") 'menu-bar-mode)


;; Open recent File Menu option
;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 35)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; Turn off Toolbar/Scrollbar and turn on line numbers
;;
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)
(require 'linum)
(global-linum-mode 1)

  
;; Color themes
;;
(defun sweyla670742 ()
  "Theme generated by Sweyla: http://themes.sweyla.com/seed/670742/"
  (interactive)
  (color-theme-install
   '(sweyla670742
     ((background-color . "#000000")
      (foreground-color . "#FFFFFF")
      (background-mode . dark)
      (border-color . "#323232")
      (cursor-color . "#FFFFFF")
      (mouse-color . "#323232"))
     (mode-line ((t (:foreground "#FFFFFF" :background "#323232"))))
     (region ((t (:background "#323232"))))
     (font-lock-comment-face ((t (:foreground "#7E602A"))))
     (font-lock-constant-face ((t (:foreground "#8B0C70"))))
     (font-lock-builtin-face ((t (:foreground "#1CB51A"))))
     (font-lock-function-name-face ((t (:foreground "#676A1A"))))
     (font-lock-variable-name-face ((t (:foreground "#0C9679"))))
     (font-lock-keyword-face ((t (:foreground "#50885C"))))
     (font-lock-string-face ((t (:foreground "#E00C2E"))))
     (font-lock-doc-string-face ((t (:foreground "#E00C2E"))))
     (font-lock-type-face ((t (:foreground "#CF2B33"))))
    )
  )
)
(provide 'sweyla670742)

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

(defun mw-dark ()
  (interactive)
  (color-theme-install
   '(color-theme-almost-monokai
     ((background-color . "#191919")
      (foreground-color . "#F8F8F2")
      (cursor-color . "#DAD085"))
     (default ((t (nil))))
     (modeline ((t (:background "white" :foreground "black" :box (:line-width 1 :style released-button)))))
     (font-lock-builtin-face ((t (:foreground "#A6E22A"))))
     (font-lock-comment-face ((t (:italic t :foreground "#993200"))))           ;brown(red?)
     (font-lock-constant-face ((t (:foreground "#1B3049"))))                    
     (font-lock-doc-string-face ((t (:foreground "#65B042"))))                  
     (font-lock-string-face ((t (:foreground "#5D465F"))))                      ;purple
     (font-lock-function-name-face ((t (:foreground "#178C64" :italic t))))     ;green
     (font-lock-keyword-face ((t (:foreground "#1B3049"))))                     ;dark green
     (font-lock-type-face ((t (:underline t :foreground "#ABC8D1"))))           ;light green
     (font-lock-variable-name-face ((t (:foreground "#FFCC00"))))               ;yellow
     (font-lock-warning-face ((t (:bold t :foreground "#FD5FF1"))))
     (highlight-80+ ((t (:background "#D62E00"))))
     (hl-line ((t (:background "#1A1A1A"))))
     (region ((t (:background "#6DC5F1"))))
     (ido-subdir ((t (:foreground "#F1266F"))))
    )
  )
)
(provide 'mw-dark)

(require 'color-theme)
;(color-theme-initialize)
(color-theme-almost-monokai)


;; Highlight current line
;;
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")


;; Reload emacs
;;
(defun reload-dot-emacs-file ()
	"reload your .emacs file without restarting Emacs"
	(interactive)
	(load-file "~/.emacs") )
(global-set-key (kbd "C-R") 'reload-dot-emacs-file)


;; Scrolling fix
;;
(setq scroll-step           1
      scroll-conservatively 10000)


;; Try to fix Emacs colors in tmux
;;
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
   ;; Use the xterm color initialization code.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))




;; Tabbar
;; 
;; This are setting for nice tabbar items
;; to have an idea of what it looks like http://imgur.com/b0SNN
;; inspired by Amit Patel screenshot http://www.emacswiki.org/pics/static/NyanModeWithCustomBackground.png
;;
(global-semantic-stickyfunc-mode -1)
(require 'tabbar)
;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)


;; Tab keybindings
;;
(global-set-key (kbd "C-M-<right>") 'tabbar-forward)
(global-set-key (kbd "C-M-<left>") 'tabbar-backward)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;;
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
;;
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
       ;;
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)
