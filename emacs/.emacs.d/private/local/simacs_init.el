(setq inhibit-startup-message t)

(if (display-graphic-p)
    (progn 
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)))

(menu-bar-mode -1)
;;(electric-pair-mode 1)
;; how to have no bells?
(setq visual-bell t)
(setq ring-bell-function 'ignore)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)
;; disable line numbers for these modes:
(dolist (mode '(org-mode-hook
                 term-mode-hook
                 shell-mode-hook
                 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
    :diminish
    :bind (
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(use-package doom-modeline
    :init (doom-modeline-mode 1))

(use-package doom-themes
    :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.5))

(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

(use-package counsel
    :bind (("M-x" . counsel-M-x)))

(defun sp/evil-hook ()
  (dolist (mode '(dashboard-mode
                  ))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump t)
    :hook (evil-mode . sp/evil-hook)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    ;; Use visual line motions
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line))


(use-package evil-escape
  :init
  (evil-escape-mode 1)
  :config
  (setq-default evil-escape-key-sequence "fd"))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer sp/leader-keys
    :keymaps '(normal insert visual emacs dashboard)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(sp/leader-keys
 "1" '(winum-select-window-1 :which-key "win 1")
 "2" '(winum-select-window-2 :which-key "win 2")
 "f" '(:ignore t :which-key "files")
 "ff" '(counsel-find-file :which-key "find file")
 "fr" '(counsel-recentf :which-key "find recent")
 "fs" '(save-buffer :which-key "save")
 "b" '(:ignore t :which-key "buffers")
 "bb" '(counsel-switch-buffer :which-key "switch")
 "bd" '(kill-buffer-and-window :which-key "delete")
 "SPC" '(counsel-M-x :which-key "M-x")
 "q" '(:ignore t :which-key "quit")
 "qa" '(evil-quit-all :which-key "quit all")
 "qq" '(evil-quit :which-key "quit")
 "k" '(:ignore t :which-key "lisp")
 "ke" '(eval-last-sexp :which-key "evaluate")
 "ks" '(sp-forward-sexp :which-key "fwd sexp")
 "kS" '(sp-forward-symbol :which-key "fwd symbol")
 "c" '(:ignore t :which-key "code")
 "cc" '(comment-line :which-key "comment")
 "r" '(:ignore t :which-key "registers")
 "rl" '(evil-show-registers :which-key "list")
 "p" '(:ignore t :which-key "projects")
 "pf" '(projectile-find-file :which-key "find")
 "s" '(:ignore t :which-key "search")
 "sp" '(swiper :which-key "swiper")
 "ss" '(avy-goto-char-2 :which-key "char2")
 "sl" '(avy-goto-line :which-key "line")
 "w" '(:ignore t :which-key "windows")
 "wv" '(evil-window-vsplit :which-key "vsplit")
 "ws" '(evil-window-split :which-key "split")
 "wh" '(evil-window-left :which-key "go left")
 "wj" '(evil-window-down :which-key "go down")
 "wk" '(evil-window-up :which-key "go up")
 "wl" '(evil-window-right :which-key "go right")
 "wo" '(delete-other-windows :which-key "only")
 "wd" '(sp/delete-chosen-window :which-key "delete")
 "z" '(hydra-zoom/body :which-key "zoom")
 "t" '(hydra-toggles/body :which-key "toggles")
 "TAB" '(evil-buffer :which-key "last buffer"))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  ;; (projects . 5)
			  ;; (agenda . 5)	;
			  ;; (registers . 5)
			  )))

(use-package projectile
  :defer
  :config
  (projectile-mode +1))

(use-package evil-snipe
  :defer
  :config
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-repeat-scope 'visible)
  (setq evil-snipe-spillover-scope 'visible)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package avy)

;;(require 'smartparens-config)
;;(smartparens-global-mode 1)

(use-package hydra)

(defhydra hydra-zoom (:color pink
			     :hint nil)
"
 _j_: in      _k_: out      _q_: quit
" 

  ("j" text-scale-increase)
  ("k" text-scale-decrease)
  ("q" (message "Done") :exit t :color blue))

(defhydra hydra-toggles nil
  "toggles"
  ("f" auto-fill-mode "fill")
  ("t" toggle-truncate-line "truncate")
  ("q" nil "cancel"))

(use-package winum
  :defer
  :init
  (winum-mode 1))

(defun sp/delete-chosen-window()
  "Enter a window no to delete."
  (interactive)
  (let ((chosen-win (read-from-minibuffer
		     "Enter window no.: "
		     "")))
  (if (equal chosen-win "")
      (message "No window entered. Cancelling...")
    ;; (message (format "Chosen window %s" chosen-win))
      (let ((current-prefix-arg (concat "-" chosen-win)))
	(call-interactively 'winum-select-window-by-number)
	))))

(use-package company
  :defer
  :bind (
	:map company-active-map
           ("C-j" . #'company-select-next)
           ("C-k" . #'company-select-previous)) 
  :init
  (global-company-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package python-mode)

(use-package gnu-elpa-keyring-update)

;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Don't edit below here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(spinner gnu-elpa-keyring-update lsp-mode python-mode winum which-key use-package rainbow-delimiters projectile page-break-lines ivy-rich hydra general evil-snipe evil-escape doom-themes doom-modeline dashboard counsel company avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 120 :width normal)))))
