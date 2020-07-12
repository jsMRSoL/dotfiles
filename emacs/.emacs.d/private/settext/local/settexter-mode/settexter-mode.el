;; settexter-mode.el
;; Minor mode for preparing set texts.
;; Main use is to enable buffer-specific keymaps.

(setq settext-dictionary-file "/home/simon/.emacs.d/private/settext/apuleius-vocab.txt"
      settext-analyses-file "/home/simon/.emacs.d/private/settext/latin-analyses2.txt")

(defun settext-set-dict-file ()
  "Set the dictionary file."
  (setq settext-dictionary-file (read-file-name "Choose dictionary file: "))
  )

(defun settext-change-dict-file ()
  "Set the dictionary file."
  (interactive)
  (setq settext-dictionary-file (read-file-name "Choose dictionary file: "))
  )

(defun settext-get-suggested-definitions()
  "Look up current line of Latin words and open suggested definitions
in a vertical split."
  (interactive)
  (while (not (boundp 'settext-dictionary-file))
    (settext-set-dict-file))
  (setq my-settext-bfr (buffer-name)
        my-clean-ctr 0
        my-line
        (split-string
         (buffer-substring
          (line-beginning-position) (line-end-position))
         " "))
  (split-window nil 40 'left)
  (switch-to-buffer (get-buffer-create "*Vocab Output*"))
  (erase-buffer)
  (dolist (elmt my-line)
    (when (> (length (replace-regexp-in-string "\\W" "" elmt)) 0)
      (setq my-clean-ctr (1+ my-clean-ctr)
          my-clean-wd (replace-regexp-in-string "\\W" "" elmt)
          my-hdwds (split-string (shell-command-to-string
                                  (concat "hermes2.sh"
                                          " "
                                          settext-analyses-file
                                          " "
                                          my-clean-wd))
                                 "\n"))
    (insert (format "%d: '%s'\n" my-clean-ctr my-clean-wd))
    (when (eq (length (nth 0 my-hdwds)) 0)
      (setcar my-hdwds my-clean-wd))
    (setq my-hdwd-ctr 0)
    (dolist (a-hdwd my-hdwds)
      (when (> (length a-hdwd) 0)
        (setq my-hdwd-ctr (1+ my-hdwd-ctr))
        (insert (format "## Def. %d: '%s'\n" my-hdwd-ctr a-hdwd))
        (setq my-defs (split-string (shell-command-to-string
                                     (concat "deffinder.sh"
                                             " "
                                             settext-dictionary-file
                                             " "
                                             (format "%s" a-hdwd)))
                                    "\n"))
        (dolist (a-def my-defs)
          (when (> (length a-def) 0)
            (insert (format "%s\n" (replace-regexp-in-string ", " "\n" a-def))))
          )
        )
      )
    (insert "-------------\n")
    ))
  (while (search-backward-regexp "^I " nil t nil)
    (replace-match ""))
  (beginning-of-buffer)
  (settext-edits-mode t)
  )

(defun settext-check-definitions ()
  "Check if the definitions are in the correct format."
  (interactive)
  (beginning-of-buffer)
  (flush-lines "^[0-9]\\|^##")
  (if (search-forward-regexp "----\n----" nil t nil)
    (message "Vocab in wrong format")
    (settext-return-definitions))
  )

(defun settext-return-definitions ()
  "Return definitions to main text window."
  (beginning-of-buffer)
  (save-excursion
    (delete-trailing-whitespace)
    (while (search-forward-regexp "\\([A-z]\\)\\(\n\\)\\([A-z]\\)" nil t)
      (replace-match ", " nil nil nil 2)))
  (while (search-forward-regexp "\n-------------\n" nil t nil)
    (replace-match "  "))
  (setq vocabline (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
  (delete-windows-on "*Vocab Output*")
  (set-buffer my-settext-bfr)
  (beginning-of-line)
  (insert (format "%s\n" vocabline))
  (settext-realign-words-columns)
  (select-window (get-buffer-window my-settext-bfr))
  )

(defun settext-realign-words-columns ()
  "Align words in columns with two spaces padding."
  (forward-line -1)
  (setq my-vocab-line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
        my-vocab-wds (split-string my-vocab-line "  " t))
  (forward-line 1)
  (beginning-of-line)
  (if (search-forward-regexp "^\s*" (line-end-position) t)
    (setq my-indent (match-string 0))
    (setq my-indent ""))
  (setq my-lang-line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
        my-lang-wds (split-string my-lang-line " " t))
  (setq my-lang-ctr 0
        my-vocab-ctr 0
        my-new-vocab-line ""
        my-new-lang-line "")
  (while (< my-lang-ctr (length my-lang-wds))
    (if (equal (length (replace-regexp-in-string "\\W" "" (nth my-lang-ctr my-lang-wds))) 0)
        (progn (setq my-str-fmt (format "%s%d%s" "%-" (+ 2 (length (nth my-lang-ctr my-lang-wds))) "s")
                     my-new-vocab-line (concat
                                        my-new-vocab-line
                                        (format my-str-fmt ""))
                     my-new-lang-line (concat
                                       my-new-lang-line
                                       (format my-str-fmt (nth my-lang-ctr my-lang-wds)))
                     my-lang-ctr (1+ my-lang-ctr)
                     )
               )
         (progn
                (if (> (length (nth my-vocab-ctr my-vocab-wds))
                        (length (nth my-lang-ctr my-lang-wds)))
                    (setq my-str-fmt (format "%s%d%s" "%-" (+ 2 (length (nth my-vocab-ctr my-vocab-wds))) "s"))
                    (setq my-str-fmt (format "%s%d%s" "%-" (+ 2 (length (nth my-lang-ctr my-lang-wds))) "s")))
                (setq my-new-vocab-line (concat
                                          my-new-vocab-line
                                          (format my-str-fmt (nth my-vocab-ctr my-vocab-wds)))
                      my-new-lang-line (concat
                                        my-new-lang-line
                                        (format my-str-fmt (nth my-lang-ctr my-lang-wds)))
                      my-lang-ctr (1+ my-lang-ctr)
                      my-vocab-ctr (1+ my-vocab-ctr)))))
  (setq my-new-vocab-line (concat
                            my-indent
                            my-new-vocab-line)
        my-new-lang-line (concat
                          my-indent
                          my-new-lang-line))
  (delete-region (line-beginning-position) (line-end-position))
  (insert my-new-lang-line)
  (forward-line -1)
  (delete-region (line-beginning-position) (line-end-position))
  (insert my-new-vocab-line)
  (forward-line 1)
  )

(defun settext-edit-vocab-line ()
  "Open current vocab line in a new vertical split for editing."
  (interactive)
  (setq my-settext-bfr (buffer-name)
        my-vocab-line
        (buffer-substring
         (line-beginning-position) (line-end-position))
        my-vocab-line (replace-regexp-in-string "^\s*"
                                                ""
                                                my-vocab-line)
        my-vocab-line (replace-regexp-in-string "\s\s\s*"
                                                "\n-------------\n"
                                                my-vocab-line)
        my-vocab-line (replace-regexp-in-string ",\s*"
                                                "\n"
                                                my-vocab-line))
  (kill-whole-line)
  (split-window nil 40 'left)
  (switch-to-buffer (get-buffer-create "*Vocab Output*"))
  (erase-buffer)
  (insert my-vocab-line)
  (beginning-of-buffer)
  (settext-edits-mode t))


(define-minor-mode settexter-mode
  "A temporary minor mode to be activated specific to a buffer."
  :keymap (let ((settexter-mode-map (make-sparse-keymap)))
            (define-key settexter-mode-map (kbd "<f11>") 'settext-edit-vocab-line)
            (define-key settexter-mode-map (kbd "<f12>") 'settext-get-suggested-definitions)
            (define-key settexter-mode-map (kbd "<f10>") 'settext-transient)
            settexter-mode-map))

(provide 'settexter-mode)
