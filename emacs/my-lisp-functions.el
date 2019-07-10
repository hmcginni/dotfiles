;;; init-lisp-functions.el --- Emacs Init File - custom lisp functions

;;; Commentary:
;;    Custom Elisp Functions

;;; Code:

(defun hrm/resize (size)
  "Resize Emacs to specified SIZE."
  (interactive)
  (let ((width (cond ((string= size "narrow") 86)
					 ((string= size "half") 127)
					 ((string= size "wide") 159))))
	(set-frame-width (selected-frame) width)))


;; Try to fix Emacs colors in tmux
(defun hrm/terminal-init-screen ()
  "Terminal initialization function for screen."
  (tty-run-terminal-initialization (selected-frame) "rxvt")
  (tty-run-terminal-initialization (selected-frame) "xterm"))


(defun hrm/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))


(defun hrm/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(defun hrm/xor (a b)
  "XOR of inputs A and B."
  (and (not (and a b))
       (or a b)))



(defun hrm/narrow-to-eof ()
  "Narrow from (point) to end-of-file."
  (interactive)
  (save-excursion
    (narrow-to-region (- (point)
                         (current-column))
                      (point-max))))


(defun hrm/toggle-comment-region ()
  "Comment or uncomment a region/line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)))


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


(defun hrm/switch-to-previous-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defun hrm/reload-emacs-init-file ()
  "Reload Emacs."
  (interactive)
  (load-file "~/.emacs.d/init.el") )


(defun hrm/date-command-on-buffer ()
  "Print date in 'ddMMMyyyy' form."
  (interactive)
  (shell-command "printf '%s' $(date +%Y%m%d)" t)
  (forward-word))


(defun hrm/inline-code (lang)
  "Insert inline LANG snippet for Org mode."
  (insert (format "src_%s[:exports code]{}" lang))
  (backward-char 1))


(defun hrm/switch-to-scratch ()
  "Go to the *scratch* buffer."
  (interactive)
  (let ((scratch "*scratch*"))
    (if (get-buffer scratch)
        (switch-to-buffer scratch)
      (switch-to-buffer scratch)
      (insert initial-scratch-message))))


;;
;; Theme Functions ------------------------------------------------------------
;;

(defvar hrm/global-is-light-theme t
  "Global variable tracking whether or not we're using the LIGHT theme.")


(defun hrm/post-theme-customizations ()
  "Fix font weirdness."
  (interactive)
  (set-face-attribute
   'mode-line nil
   :font "IBM Plex Sans Condensed:pixelsize=12:weight=medium" )
  (set-face-attribute
   'mode-line-inactive nil
   :font "IBM Plex Sans Condensed:pixelsize=12:weight=medium:slant=italic" ))


(defun hrm/light-theme ()
  "Apply a light GUI theme."
  (interactive)
  (setq hrm/global-is-light-theme t)
  (if (display-graphic-p)
      (progn (load-theme 'atom-one-light t)
             (hrm/post-theme-customizations))
    (load-theme 'cmd-atom-one-light t)
    (hrm/post-theme-customizations)))


(defun hrm/dark-theme ()
  "Apply a dark GUI theme."
  (interactive)
  (setq hrm/global-is-light-theme nil)
  (if (display-graphic-p)
      (progn (load-theme 'atom-one-dark t)
             (hrm/post-theme-customizations))
    (load-theme 'cmd-atom-one-dark t)
    (hrm/post-theme-customizations)))


(defun hrm/set-theme (light)
  "Customize Emacs theme; use a LIGHT color theme if `t` and dark if `nil`."
  (if light
      (hrm/light-theme)
    (hrm/dark-theme)))


(defun hrm/toggle-theme ()
  "Switch between light and dark themes."
  (interactive)
  (setq hrm/global-is-light-theme (hrm/xor hrm/global-is-light-theme t))
  (hrm/set-theme hrm/global-is-light-theme))


;;
;; DPI Scaling ----------------------------------------------------------------
;;

(defun hrm/get-dpi ()
  "Get the DPI of the display."
  (interactive)
  (let* ((attrs (car (display-monitor-attributes-list)))
         (display-size (assoc 'mm-size attrs))
         (display-width (/ (cadr display-size)
                           25.4))
         (resolution (cdr (assoc 'geometry attrs)))
         (x-resolution (cadr (cdr resolution)))
         (dpi (/ x-resolution
                 display-width)))
    (message "DPI: %f" dpi)
    dpi))


(defun hrm/scale-font-for-dpi ()
  "Pick a font size based on the DPI."
  (let ((dpi (hrm/get-dpi)))
    (cond ((< dpi 135) 12)  ;; dpi=96 => 12
          ((< dpi 145) 13)  ;; dpi=140 => 14
          ((< dpi 155) 14)  ;; [145, 155) must be ?16?
          ((< dpi 165) 15)
          (t 16))))


(defun hrm/set-scaled-font (face weight)
  "Set the scaled font spec string for the specified FACE and WEIGHT."
  (let* ((size (hrm/scale-font-for-dpi))
         (font (format "%s:size=%d:weight=%s" face size weight)))
    (set-frame-font font)))


;;
;; Narrowing ------------------------------------------------------------------
;;

(defun hrm/narrow-to-defun-indirect ()
  "Create a new indirect buffer narrowed to the current function."
  (interactive)
  (let ((new-buffer-name (generate-new-buffer-name (buffer-name)))
        (current-buffer-name (buffer-name)))
    (make-indirect-buffer current-buffer-name
                          new-buffer-name
                          t)
    (switch-to-buffer new-buffer-name)
    (narrow-to-defun)))


;;
;; End
;;

;;; my-lisp-functions.el ends here
