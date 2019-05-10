;;; init-lisp-functions.el --- Emacs Init File - custom lisp functions

;;; Commentary:
;;    Custom Elisp Functions

;;; Code:

(defun hrm/narrow-window ()
  "Resize Emacs."
  (interactive)
  (set-frame-width (selected-frame) 86))

;; ------------------------------------------------------------

;; Try to fix Emacs colors in tmux
(defun hrm/terminal-init-screen ()
  "Terminal initialization function for screen."
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))

;; ------------------------------------------------------------

(defun hrm/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; ------------------------------------------------------------

(defun hrm/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; ------------------------------------------------------------

(defun hrm/xor (a b)
  "XOR of inputs A and B."
  (and (not (and a b))
       (or a b)))
(provide 'xor)

;; ------------------------------------------------------------

(defun hrm/narrow-to-eof ()
  "Narrow from (point) to end-of-file."
  (interactive)
  (save-excursion
    (narrow-to-region (- (point)
                         (current-column))
                      (point-max))))

;; ------------------------------------------------------------

(defun hrm/comment-or-uncomment-region-or-line ()
  "Comment or uncomment a region/line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; ------------------------------------------------------------

(defun hrm/file-name-on-clipboard ()
  "Copy current buffer to clipboard."
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

(defun hrm/switch-to-previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; ------------------------------------------------------------

(defun hrm/reload-emacs-init-file ()
  "Reload Emacs."
  (interactive)
  (load-file "~/.emacs.d/init.el") )

;; ------------------------------------------------------------

(defun hrm/date-command-on-buffer ()
  "Print date in 'ddMMMyyyy' form."
  (interactive)
  (shell-command "printf '%s' $(date +%Y%m%d)" t)
  (forward-word))

;; ------------------------------------------------------------

(defun hrm/generate-new-filename (path)
  "Create a new date-stamped filename in PATH."
  (interactive)
  (let ((name (read-string
               "File name: ")))
    (expand-file-name
     (format "%s-%s.org"
             (format-time-string "%Y%m%d")
             name)
     path)))

;; ------------------------------------------------------------

(defun hrm/command-line-diff ()
  "Start diff from Command Line."
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

;; ------------------------------------------------------------

(defun hrm/post-theme-customizations ()
  "Fix font weirdness."
  (interactive)
  (set-face-attribute
   'mode-line nil
   :font "IBM Plex Sans:pixelsize=12:weight=medium" )
  (set-face-attribute
   'mode-line-inactive nil
   :font "IBM Plex Sans:pixelsize=12:weight=medium:slant=italic" ))
(provide 'post-theme-customizations)

;; ------------------------------------------------------------
;; ------------------------------------------------------------

(defvar hrm/global-is-light-theme t)

(defun hrm/light-theme ()
  "Apply a light GUI theme."
  (interactive)
  (setq hrm/global-is-light-theme t)
  (if (display-graphic-p)
      (progn (load-theme 'atom-one-light t)
             (hrm/post-theme-customizations))
    (progn (load-theme 'cmd-atom-one-light t)
           (hrm/post-theme-customizations))))
(provide 'light-theme)

;; ------------------------------------------------------------

(defun hrm/dark-theme ()
  "Apply a dark GUI theme."
  (interactive)
  (setq hrm/global-is-light-theme nil)
  (if (display-graphic-p)
      (progn (load-theme 'atom-one-dark t)
             (hrm/post-theme-customizations))
    (progn (load-theme 'cmd-atom-one-dark t)
           (hrm/post-theme-customizations))))
(provide 'dark-theme)

;; ------------------------------------------------------------

(defun hrm/set-theme (light)
  "Customize Emacs theme depending on UI.
Use a light color theme if LIGHT and dark otherwise."
  (if light
      (hrm/light-theme)
    (hrm/dark-theme)))
(provide 'set-theme)

;; ------------------------------------------------------------

(defun hrm/toggle-theme ()
  "Switch between light and dark themes."
  (interactive)
  (setq hrm/global-is-light-theme (hrm/xor hrm/global-is-light-theme t))
  (hrm/set-theme hrm/global-is-light-theme))
(provide 'toggle-theme)

;; ------------------------------------------------------------
;; ------------------------------------------------------------

(provide 'my-lisp-functions)

;;; my-lisp-functions.el ends here
