;; lsj2-defs-mode.el
;; Temporary minor mode
;; Main use is to enable it only in specific buffers to achieve the goal of
;; buffer-specific keymaps

(define-minor-mode lsj2-defs-mode
  "A temporary minor mode to be activated specific to a buffer."
  :keymap (let ((lsj2-defs-mode-map (make-sparse-keymap)))
            (evil-define-key 'normal lsj2-defs-mode-map "a" 'lsj-send-to-list)
            (evil-define-key 'normal lsj2-defs-mode-map "n" 'lsj-next-entry)
            (evil-define-key 'normal lsj2-defs-mode-map "f" 'lsj-find-definition)
              lsj2-defs-mode-map))

(provide 'lsj2-defs-mode)
