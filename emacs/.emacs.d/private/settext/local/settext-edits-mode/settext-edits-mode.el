;; settext-edits-mode.el
;; Temporary minor mode
;; Main use is to enable it only in specific buffers to achieve the goal of
;; buffer-specific keymaps

(define-minor-mode settext-edits-mode
  "A temporary minor mode to be activated specific to a buffer."
  :keymap (let ((settext-edits-mode-map (make-sparse-keymap)))
            (define-key settext-edits-mode-map (kbd "<f12>") 'settext-check-definitions)
              settext-edits-mode-map))

(provide 'settext-edits-mode)
