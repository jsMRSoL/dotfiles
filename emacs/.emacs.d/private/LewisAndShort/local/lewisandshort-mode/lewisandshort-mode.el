;; lewisandshort-mode.el
;; Minor mode for preparing set texts.
;; Main use is to enable buffer-specific keymaps.

(defvar lewisandshort-windows-setup nil
  "Whether lewisandshort-set-up-windows has been called yet.")

(defun lewisandshort-get-defs()
  "Look up Latin word under cursor and display lemmas
in a vertical split."
  (interactive)
  (unless lewisandshort-windows-setup (lewisandshort-set-up-windows))
  (setq latin-word (current-word t t))
  (select-window (get-buffer-window "*Definitions*"))
  (erase-buffer)
  (insert (shell-command-to-string
           (concat "/home/simon/Projects/python/latindictionary/query_Lewis_and_Short.py"
                   " "
                   latin-word)))
  (beginning-of-buffer)
  (when (re-search-forward "^# S" nil t)
    (forward-line 1)))


(defun lewisandshort-send-to-list()
  "Add approved meaning to vocab list window"
  (interactive)
  (setq lewisandshort-def (buffer-substring
                           (line-beginning-position) (line-end-position)))
  (re-search-backward "^~~~~")
  (forward-line 1)
  (setq lewisandshort-hd-wd (buffer-substring
                             (line-beginning-position) (line-end-position)))
  (with-current-buffer (get-buffer "Vocabulary List")
    (goto-char (point-max))
    (newline)
    (insert (concat lewisandshort-hd-wd
                    " : "
                    lewisandshort-def)))
  (select-window (get-buffer-window lewisandshort-text-bfr)))

(defun lewisandshort-next-entry()
  "Jump to next definition entry."
  (interactive)
  (re-search-forward "^~~~~")
  (forward-line 1)
  (recenter 1)
  (when (re-search-forward "^# S" nil t)
    (forward-line 1)))

(defun lewisandshort-set-up-windows()
  (setq lewisandshort-text-bfr (buffer-name))
  (split-window nil 50 'left)
  (switch-to-buffer (get-buffer-create "*Definitions*"))
  (lewisandshort-mode t)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer (get-buffer-create "Vocabulary List"))
  (other-window 1)
  (setq lewisandshort-windows-setup t))

(define-minor-mode lewisandshort-mode
  "A temporary minor mode to be activated specific to a buffer.")

(provide 'lewisandshort-mode)
