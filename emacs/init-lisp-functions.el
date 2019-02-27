;; ========================================================================== ;;
;; Package Init/Install 
;;

;; Resize Emacs
(defun narrow-window ()
  (interactive)
  (set-frame-width (selected-frame) 86))

;; ------------------------------------------------------------

;; Try to fix Emacs colors in tmux
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))

;; ------------------------------------------------------------

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; ------------------------------------------------------------

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; ------------------------------------------------------------

(defun narrow-to-eof ()
  "Narrow from (point) to end-of-file"
  (interactive)
  (save-excursion
    (narrow-to-region (- (point)
                         (current-column))
                      (point-max))))

;; ------------------------------------------------------------

;; Comment or uncomment a region/line
(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
	(if (region-active-p)
		(setq beg (region-beginning)
              end (region-end))
	  (setq beg (line-beginning-position)
            end (line-end-position)))
	(comment-or-uncomment-region beg end)))

;; ------------------------------------------------------------

;; Copy current buffer to clipboard
(defun file-name-on-clipboard ()
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region
         (point-min)
         (point-max)))
      (message filename))))

;; ------------------------------------------------------------

;; Switch to previous buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ------------------------------------------------------------

;; Reload emacs
(defun reload-emacs-init-file ()
  (interactive)
  (load-file "~/.emacs.d/init.el") )

;; ------------------------------------------------------------

;; Print date in 'ddMMMyyyy' form
(defun date-command-on-buffer ()
  (interactive)
  (shell-command "printf '%s' $(date +%Y%m%d)" t)
  (forward-word))

;; ------------------------------------------------------------

;; Create a new date-stamped filename
(defun generate-new-filename (path)
  (interactive)
  (let ((name (read-string
               "File name: ")))
    (expand-file-name
     (format "%s-%s.org"
             (format-time-string "%Y%m%d")
             name)
     path)))

;; ------------------------------------------------------------

;; Start diff from Command Line
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
		(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

;; ------------------------------------------------------------

;; Refile shortcuts
(defun refile-to (headline)
  (interactive)
  (let ((pos (save-excursion
               org-todos-file
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline org-todos-file nil pos))))

;; ------------------------------------------------------------

(defun color-theme-almost-monokai ()
  (interactive)
  (color-theme-install
   '(color-theme-almost-monokai
     ((background-color . "#191919")
      (foreground-color . "#F8F8F2")
      (cursor-color . "#e0e0e0"))
     (default ((t (nil))))
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

;; ------------------------------------------------------------

(defun post-theme-customizations ()
  (interactive)
  (set-face-attribute
   'mode-line nil
   :font "Roboto:pixelsize=12:weight=medium" )
  (set-face-attribute
   'mode-line-inactive nil
   :font "Roboto:pixelsize=12:weight=medium:slant=italic" ))
(provide 'post-theme-customizations)

;; ------------------------------------------------------------

(defun cmd ()
  (interactive)
  (setq viper-mode t)
  (viper-mode)
  (color-theme-almost-monokai))
(provide 'cmd)

;; ------------------------------------------------------------

(defun light ()
  (interactive)
  (load-theme 'atom-one-light t)
  (post-theme-customizations))
(provide 'gui)

;; ------------------------------------------------------------
