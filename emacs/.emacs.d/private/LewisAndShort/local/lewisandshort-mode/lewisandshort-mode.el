;; lewisandshort-mode.el
;; Minor mode for preparing set texts.
;; Main use is to enable buffer-specific keymaps.

(defun lewisandshort-get-defs()
  "Look up Latin word under cursor and display lemmas
in a vertical split."
  (interactive)
  (setq lewisandshort-text-bfr (buffer-name)
        latin-word (current-word t t))
  (if (get-buffer-window "*Definitions*")
      (select-window (get-buffer-window "*Definitions*"))
    (progn (split-window nil 40 'left)
           (switch-to-buffer (get-buffer-create "*Definitions*"))))
  (erase-buffer)
  (insert (shell-command-to-string
           (concat "/home/simon/Projects/python/latindictionary/query_Lewis_and_Short.py"
                   " "
                   latin-word)))
  (beginning-of-buffer)
  (lewisandshort-mode t))


(defun lewisandshort-send-to-list()
  "Add approved meaning to vocab list window"
  (interactive)
  (setq lewisandshort-def (buffer-substring
                       (line-beginning-position) (line-end-position)))
  ;; (goto-char (point-min))
  (re-search-backward "^~~~~")
  (forward-line 1)
  (setq lewisandshort-hd-wd (buffer-substring
                       (line-beginning-position) (line-end-position)))
  (if (get-buffer-window "Vocabulary List")
      (select-window (get-buffer-window "Vocabulary List"))
    (progn (split-window-below-and-focus)
           (switch-to-buffer (get-buffer-create "Vocabulary List"))))
  (insert (concat lewisandshort-hd-wd
                  " : "
                  lewisandshort-def))
  (newline)
  (select-window (get-buffer-window lewisandshort-text-bfr)))


(define-minor-mode lewisandshort-mode
  "A temporary minor mode to be activated specific to a buffer."
  )

(provide 'lewisandshort-mode)
