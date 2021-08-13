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

(setq inhibit-startup-message t)

(if (display-graphic-p)
    (progn 
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)))

(menu-bar-mode -1)
;; how to have no bells?
(setq visual-bell t)
(setq ring-bell-function 'ignore)

(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)
;; disable line numbers for these modes:
(dolist (mode '(org-mode-hook
		term-mode-hook
		treemacs-mode-hook
		shell-mode-hook
		eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-dracula t)
  :custom
  (setq doom-themes-enable bold t
	doom-themes-enable-italic t)
  (doom-themes-org-config))

(custom-set-faces '(default ((t
			      (:family "Source Code Pro"
				       :foundry "ADBO" :slant normal
				       :weight normal :height 120
				       :width normal)))))
(global-prettify-symbols-mode t)

(defun sp/make-link-to-private-shared-folder ()
  "Make a symlink in the emacs home directory to a folder
    in dotfiles."
  (let ((private (concat user-emacs-directory "private")))
    (if (not (file-exists-p private))
      (make-symbolic-link "/home/simon/.dotfiles/emacs/.emacs.d/private/" private)
      (message "Link to private already exists..."))))

(sp/make-link-to-private-shared-folder)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 10)
			  (bookmarks . 5)
			  (projects . 5)
			  ;; (agenda . 5)	;
			  ;; (registers . 5)
			  )))

;; (defun sp/evil-hook ()
;;   (dolist (mode '(dashboard-mode
;; 		  ))
;;     (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-tree)
  ;; :hook (evil-mode . sp/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'help-mode 'normal)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'rustic-popup-mode 'emacs))


(use-package evil-escape
  :init
  (evil-escape-mode 1)
  :config
  (setq-default evil-escape-key-sequence "fd"
		evil-escape-delay 0.2))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package smartparens
  :init
  (require 'smartparens-config))

(use-package avy)

(use-package evil-nerd-commenter
  :init
  (evil-define-key 'normal 'global
    "gcc" 'evilnc-comment-or-uncomment-lines
    "gcp" 'evilnc-copy-and-comment-lines))

(use-package expand-region)

(use-package popup-kill-ring
  :bind (("M-y" . popup-kill-ring)))

(use-package undo-tree
  :custom
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode 1))

(use-package winum
  :defer
  :init
  (winum-mode 1)
  :bind
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6))


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

(use-package winner
  :after evil
  :config
  (winner-mode))

(setq tab-bar-new-tab-choice "*scratch*"
      tab-bar-show nil)

(use-package perspective
  :init
  (persp-mode)
  (setq persp-state-default-file "~/.simacs_dir/layouts/layouts"))

(fset 'insert-line-and-paste-clipboard
      [?O escape ?m ?A ?\" ?* ?P ?0 ?\' ?A])

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
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :defer
  :bind (
	 :map company-active-map
	 ("C-j" . #'company-select-next)
	 ("C-k" . #'company-select-previous)
	 ("<tab>" . #'yas-expand)) 
  :init
  (global-company-mode 1)
  :custom
  (company-transformers '(company-sort-prefer-same-case-prefix)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :init
  ;; (setq-default yas-snippet-dirs '("~/.dotfiles/emacs/.emacs.d/private/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package hydra
  :defer)

(defhydra hydra-parens (:color pink
			       :hint nil)
  "
^Navigate sexp^         ^Change sexp^              
^^^^^^^^--------------------------------------------------
_n_: next    _u_: up      _s_: slurp        _d_: kill      
_p_: prev    _v_: down    _S_: bkwd slurp   _D_: bkwd kill
_f_: fwd     _e_: end     _b_: barf         
_c_: back    _E_: eval    _B_: bkwd barf    _q_: quit            
"

  ("n" sp-next-sexp)
  ("p" sp-previous-sexp)
  ("f" sp-forward-sexp)
  ("c" sp-backward-sexp)
  ("e" sp-end-of-sexp)
  ("E" eval-last-sexp)
  ("u" sp-up-sexp)
  ("v" sp-down-sexp)
  ("d" sp-kill-sexp)
  ("D" sp-backward-kill-sexp)
  ("s" sp-forward-slurp-sexp)
  ("S" sp-backward-slurp-sexp)
  ("b" sp-forward-barf-sexp)
  ("B" sp-backward-barf-sexp)
  ("q" (message "Done") :exit t :color blue))

(defhydra hydra-zoom nil
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" (message "Done") :exit t :color blue))

(defhydra hydra-toggles nil
  "toggles"
  ("f" auto-fill-mode "fill")
  ("t" toggle-truncate-line "truncate")
  ("w" whitespace-mode "whitespace")
  ("T" counsel-load-theme "theme")
  ("q" nil "cancel"))

(defhydra sp/hydra-org-headings (:color pink
					:hint nil)
  "
      _h_: promote    _j_: move down    _k_: move up    _l_: demote    _q_: quit" 
  ("h" org-metaleft)
  ("j" org-metadown)
  ("k" org-metaup)
  ("l" org-metaright)
  ("q" (message "Done") :exit t :color blue))

(defhydra hydra-smerge nil
  "smerge commands"
  ("a" smerge-keep-all "keep all")
  ("u" smerge-keep-upper "keep upper")
  ("l" smerge-keep-lower "keep lower")
  ("n" smerge-next "next conflict")
  ("p" smerge-prev "prev conflict")
  ("q" (message "Done") :exit t :color blue))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer sp/leader-keys
    :keymaps '(normal insert visual emacs dashboard)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun sp/open-init ()
  "Open init.el for simacs."
  (interactive)
  (find-file "~/.simacs_dir/simacs.org"))

(defun sp/open-journal ()
  "Open journal.org for simacs."
  (interactive)
  (find-file "~/Documents/org/journal.org"))

(defun sp/open-tasks ()
  "Open tasks.org for simacs."
  (interactive)
  (find-file "~/Documents/org/tasks.org"))

(defun sp/open-with-tasks-and-capture ()
  "Open tasks.org and org-capture for simacs.

This is mainly intended to be used from the command line as a startup convenience."
  (interactive)
  (find-file "~/Documents/org/tasks.org")
  (org-capture))

(sp/leader-keys
  "1" '(winum-select-window-1 :which-key "win 1")
  "2" '(winum-select-window-2 :which-key "win 2")
  "SPC" '(counsel-M-x :which-key "M-x")
  ":" '(eval-expression :which-key "M-:")
  "TAB" '(evil-buffer :which-key "last buffer")
  "u" '(universal-argument :which-key "c-u")
  "`" '(org-capture :which-key "org capture")
  "a" '(:ignore t :which-key "apps")
  "ad" '(dired :which-key "dired")
  "aj" '(dired-jump :which-key "dired-jump")
  "at" '(vterm :which-key "terminal")
  "au" '(undo-tree-visualize :which-key "undo-tree")
  "am"  '(:ignore t :which-key "media")
  "amp" '(emms-pause :which-key "play / pause")
  "amf" '(emms-play-file :which-key "play file")
  "ax" '(org-capture :which-key "org capture")
  "b" '(:ignore t :which-key "buffers")
  "bb" '(persp-counsel-switch-buffer :which-key "switch")
  "bd" '(kill-buffer-and-window :which-key "delete")
  "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "scratch")
  "bh" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "dashboard")
  "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages")
  "c" '(:ignore t :which-key "code")
  "cc" '(comment-line :which-key "comment")
  "e" '(:ignore t :which-key "eww")
  "ee" '(eww :which-key "run eww")
  "eb" '(eww-list-bookmarks :which-key "list bookmarks")
  "eB" '(eww-add-bookmark :which-key "add bookmark")
  "f" '(:ignore t :which-key "files")
  "fed" '(sp/open-init :which-key "edit init.el")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "find recent")
  "fs" '(save-buffer :which-key "save")
  "fw" '(write-file :which-key "save as")
  "ft" '(treemacs :which-key "treemacs")
  "g" '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "status")
  "gm" '(hydra-smerge/body :which-key "(s)merge")
  "h" '(:ignore t :which-key "help")
  "q" '(:ignore t :which-key "quit")
  "qa" '(evil-quit-all :which-key "quit all")
  "qq" '(evil-quit :which-key "quit")
  "qe" '(kill-emacs :which-key "kill emacs")
  "j" '(:ignore t :which-key "jump")
  "jo" '(sp/dired-jump-dir :which-key "open common")
  "jj" '(sp/open-journal :which-key "journal.org")
  "jt" '(sp/open-tasks :which-key "tasks.org")
  "k" '(:ignore t :which-key "lisp")
  "kk" '(hydra-parens/body :which-key "hydra")
  "ke" '(sp-end-of-sexp :which-key "end")
  "kE" '(eval-last-sexp :which-key "evaluate")
  "ks" '(sp-forward-slurp-sexp :which-key "forward slurp")
  "kS" '(sp-backward-slurp-sexp :which-key "backward slurp")
  "kb" '(sp-forward-barf-sexp :which-key "forward barf")
  "kB" '(sp-backward-barf-sexp :which-key "backward barf")
  "kw" '(:ignore t :which-key "wrap")
  "kwr" '(sp-rewrap-sexp :which-key "rewrap")
  "kw{" '(sp-wrap-curly :which-key "curly")
  "kw(" '(sp-wrap-round :which-key "round")
  "kw[" '(sp-wrap-square :which-key "square")
  "kwu" '(sp-unwrap-sexp :which-key "unwrap next")
  "kwU" '(sp-backward-unwrap-sexp :which-key "unwrap prev")
  "l" '(:ignore t :which-key "layouts")
  "la" '(persp-add-buffer :which-key "add buffer")
  "lA" '(persp-set-buffer :which-key "add buf excl")
  "lb" '(persp-ivy-switch-buffer :which-key "switch buf")
  "lc" '(persp-kill :which-key "close layout")
  "lk" '(persp-remove-buffer :which-key "remove buffer")
  "ll" '(persp-switch-last :which-key "last layout")
  "lr" '(persp-rename :which-key "rename layout")
  "ls" '(persp-switch :which-key "switch layout")
  "ln" '(persp-next :which-key "next layout")
  "lp" '(persp-prev :which-key "prev layout")
  "l C-s" '(persp-state-save :which-key "save layout")
  "l C-l" '(persp-state-load :which-key "load layout")
  "o" '(:ignore t :which-key "org")
  "oa" '(org-agenda :which-key "agenda")
  "ob" '(:ignore t :which-key "babel")
  "obt" '(org-babel-tangle :which-key "tangle")
  "oi" '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "link")
  "oit" '(:ignore t :which-key "timestamp")
  "oitt" '(org-time-stamp-inactive :which-key "inactive")
  "oita" '(org-time-stamp :which-key "active")
  "oj" '(counsel-org-goto :which-key "jump")
  "oh" '(sp/hydra-org-headings/body :which-key "headings")
  "oc" '(:ignore t :which-key "checkbox")
  "occ" '(sp/org-insert-checkbox :which-key "insert")
  "oct" '(org-toggle-checkbox :which-key "toggle")
  "och" '(org-toggle-checkbox-half :which-key "toggle half")
  "ot" '(org-todo :which-key "todo")
  "or" '(org-refile :which-key "refile")
  "on" '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
  "oo" '(org-open-at-point :which-key "open/follow")
  "oe" '(org-export-dispatch :which-key "export")
  "p" '(projectile-command-map :which-key "projects")
  "r" '(:ignore t :which-key "registers")
  "rl" '(evil-show-registers :which-key "list")
  "rp" '(insert-line-and-paste-clipboard :which-key "insert line paste")
  "s" '(:ignore t :which-key "search")
  "sp" '(swiper :which-key "swiper")
  "ss" '(avy-goto-char-2 :which-key "char2")
  "sl" '(avy-goto-line :which-key "line")
  "t" '(:ignore t :which-key "tabs")
  "tn" '(tab-bar-new-tab :which-key "new")
  "tc" '(tab-bar-close-tab :which-key "close")
  "tt" '(tab-bar-switch-to-tab :which-key "switch")
  "v" '(:ignore t :which-key "region")
  "vv" '(er/expand-region :which-key "expand")
  "v(" '(er/mark-outside-pairs :which-key "outside pairs")
  "v)" '(er/mark-inside-pairs :which-key "inside pairs")
  "v\"" '(er/mark-outside-quotes :which-key "outside quotes")
  "vq" '(er/mark-inside-quotes :which-key "inside quotes")
  "vd" '(er/mark-defun :which-key "function")
  "vc" '(er/mark-comment :which-key "comment")
  "ve" '(er/mark-email :which-key "email")
  "vu" '(er/mark-url :which-key "url")
  "w" '(:ignore t :which-key "windows")
  "wv" '(evil-window-vsplit :which-key "vsplit")
  "ws" '(evil-window-split :which-key "split")
  "wh" '(evil-window-left :which-key "go left")
  "wj" '(evil-window-down :which-key "go down")
  "wk" '(evil-window-up :which-key "go up")
  "wl" '(evil-window-right :which-key "go right")
  "wo" '(delete-other-windows :which-key "only")
  "wd" '(sp/delete-chosen-window :which-key "delete")
  "wu" '(winner-undo :which-key "winner undo")
  "wU" '(winner-redo :which-key "winner redo")
  "wF" '(make-frame :which-key "new frame")
  "z" '(hydra-zoom/body :which-key "zoom")
  "T" '(hydra-toggles/body :which-key "toggles"))

(define-key evil-normal-state-map (kbd "s") 'avy-goto-char-timer)
(general-nmap "SPC h" (general-simulate-key "C-h"))

(use-package dired
    :ensure nil
    :commands (dired dired-jump)
    :custom ((dired-listing-switches "-agho --group-directories-first"))
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer
      (kbd "SPC") nil))

  (use-package dired-single)

  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" 'dired-hide-dotfiles-mode))

(defvar sp-common-dirs
  `((?h . "/home/simon/")
    (?d . "/home/simon/Documents/")
    (?o . "/home/simon/Downloads/")
    (?r . "/home/simon/Documents/org/")
    (?f . "/home/simon/.dotfiles/")
    (?e . ,user-emacs-directory)
    (?c . "/home/simon/.config/")
    (?b . "/home/simon/.local/usr/bin/")
    (?j . "/home/simon/Projects")
    (?y . "/home/simon/Projects/python/"))
  "An alist of common-dirs to facilitate quick navigation.")

(defun sp/dired-jump-dir(char)
  "Jump to a directory in my common directories list."
  (interactive "c[h]ome, [d]ocs, d[o]wnloads, [e]macs, o[r]g, dot[f]iles, [c]onfig, .[b]in, pro[j]ects, p[y]thon")
  (dired-jump nil (alist-get char sp-common-dirs)))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup-files"))))

;; Dump custom-set variable to a disposable file.
(setq custom-file (concat user-emacs-directory "custom-set-variables-data.el"))

(use-package openwith
  :config
  (setq openwith-associations
	(list
	  (list (openwith-make-extension-regexp
		'("mpg" "mpeg" "mp3" "mp4"
                  "m4a"
		  "avi" "wmv" "wav" "mov" "flv"
		  "ogm" "ogg" "mkv"))
		"mpv"
		'(file))
	  (list (openwith-make-extension-regexp
		'("xbm" "pbm" "pgm" "ppm" "pnm"
		  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
		  "sxiv"
		  '(file))
	  (list (openwith-make-extension-regexp
		'("pdf"))
		"mupdf"
		'(file)))))

(use-package vterm
  :commands vterm)

(use-package magit
  :commands (magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (add-hook 'with-editor-mode-hook #'evil-insert-state))

(setq vc-follow-symlinks t)

(use-package git-gutter
  :hook ((text-mode . git-gutter-mode)
	 (prog-mode . git-gutter-mode)))

(use-package flycheck)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  :custom ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init 
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(add-hook 'prog-mode-hook #'electric-pair-mode)

(use-package pyvenv
  :defer)

(use-package lsp-pyright
  :defer)

(defun sp/setup-python-lsp ()
  (require 'pyvenv)
  (pyvenv-mode 1)
  (require 'lsp-pyright)
  ;; (fset 'lsp-format-buffer 'yapfify-buffer)
  ;; (fset 'lsp-format-region 'yapfify-region)
  (lsp-deferred) ;; or lsp
  (require 'yapfify)
  (push '(company-capf :with company-yasnippet) company-backends)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "= =" '(yapfify-buffer :which-key "format buffer")
   "= r" '(yapfify-region-or-buffer :which-key "format region")))

(use-package python-mode
  :defer
  :mode "\\.py\\'"
  :hook
  (python-mode . sp/setup-python-lsp)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt --no-banner")
  (python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
  (python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
  (python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (dap-python-executable "python")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(defun sp/setup-rust-lsp ()
  (lsp-deferred))

(use-package rustic
  :defer
  :hook
  (rustic-mode . sp/setup-rust-lsp))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.css\\'"   . web-mode)
	 ("\\.jsx?\\'"  . web-mode)
	 ("\\.tsx?\\'"  . web-mode)
	 ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2) ; HTML
  (setq web-mode-css-indent-offset 2)    ; CSS
  (setq web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package dap-mode
  :defer
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :which-key "debugger"))
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
	 :request "launch"
	 :name "LLDB::Run"
	 :gdbpath "rust-lldb"
	 :target nil
	 :cwd nil))

  (dap-register-debug-template
   "Rust::GDB Run Configuration"
   (list :type "gdb"
	 :request "launch"
	 :name "GDB::Run"
	 :gdbpath "rust-gdb"
	 :environment-variables '(("KEY" . "VALUE"))
	 :target nil
	 :cwd nil)))

(evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
(evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

(evil-define-key '(normal insert visual) org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key '(normal insert visual) org-mode-map (kbd "M-l") 'org-metaright)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-ellipsis " ▾")

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-agenda-files
      '("~/Documents/org/tasks.org"
	"~/Documents/org/ideas.org"
	"~/Documents/org/journal.org"
	))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)")
	(sequence "WAITING(w@/!)" "SOMEDAY(s!)" "PROJ(p!)" "|" "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-tag-alist
      '((:startgroup)
        ; Put mutually exclusive tags here
        (:endgroup)
        ("@errand" . ?e)
        ("@home" . ?h)
        ("@garage" . ?g)
        ("@work" . ?w)
        ("@family" . ?f)
        ("@note" . ?n)
        ("@fun" . ?F)
        ("@urgent" . ?u)
        ("@computing" . ?c)
        ("@idea" . ?i)))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	("n" "Next Tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))

	("W" "Work Tasks" tags-todo "+work-email")

	;; Low-effort next actions
	("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	 ((org-agenda-overriding-header "Low Effort Tasks")
	  (org-agenda-max-todos 20)
	  (org-agenda-files org-agenda-files)))

	("w" "Workflow Status"
	 ((todo "WAIT"
		((org-agenda-overriding-header "Waiting on External")
		 (org-agenda-files org-agenda-files)))
	  (todo "REVIEW"
		((org-agenda-overriding-header "In Review")
		 (org-agenda-files org-agenda-files)))
	  (todo "PLAN"
		((org-agenda-overriding-header "In Planning")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "BACKLOG"
		((org-agenda-overriding-header "Project Backlog")
		 (org-agenda-todo-list-sublevels nil)
		 (org-agenda-files org-agenda-files)))
	  (todo "READY"
		((org-agenda-overriding-header "Ready for Work")
		 (org-agenda-files org-agenda-files)))
	  (todo "ACTIVE"
		((org-agenda-overriding-header "Active Projects")
		 (org-agenda-files org-agenda-files)))
	  (todo "COMPLETED"
		((org-agenda-overriding-header "Completed Projects")
		 (org-agenda-files org-agenda-files)))
	  (todo "CANC"
		((org-agenda-overriding-header "Cancelled Projects")
		 (org-agenda-files org-agenda-files)))))))

(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 1))
	("journal.org" . (:maxlevel . 3))
	("archive.org" . (:maxlevel . 1))))
;; save org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-capture-templates
      '(("t" "Tasks / Projects / Appointments")
	("tt" "Task" entry (file+olp "~/Documents/org/tasks.org" "To organise")
	 "* TODO  %^{Title}\n  :LOGBOOK:\n  - Created: %U\n   :END:\n  :SUBTASKS:\n  - [ ]  %?\n  :END:\n  %a\n  %i" :empty-lines 1)
	("ta" "Appointment" entry (file+olp "~/Documents/org/tasks.org" "Appointments")
	 "* TODO  %^{Title} %?\n  :LOGBOOK:\n  - Created: %U\n   :END:\n  :SUBTASKS:\n  - [ ]  \n  :END:\n  %a\n  %i" :empty-lines 1)
	("j" "Journal Entries")
	("jj" "Journal" entry (file+olp+datetree "~/Documents/org/journal.org")
	 "\n* %<%I:%M %p> - Journal :journal:\n**  %?\n\n" :empty-lines 1)

	("b" "Book log")
	("br" "Read" entry (file+headline "~/Documents/org/Books.org" "2021")
	 ;; "| %^{Title} | %^{Author} | %^{Pages} | %^{Started} |  |  | %^{Notes} |" :kill-buffer t)
	 "* %^{Title}\n:PROPERTIES:\n:Title: %\\1\n:Author: %^{Author}\n:Pages: ?\n:Started: %U\n:Finished: ?\n:Sessions: ?\n:Notes: %^{Notes} %?\n:END:"
	 :kill-buffer t)
	("m" "Metrics Capture")
	("mw" "Weight" table-line (file+headline "~/Documents/org/metrics.org" "Weight")
	 "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
	("mg" "Guitar" table-line (file+headline "~/Documents/org/metrics.org" "Guitar")
	 "| %U | %^{Time spent (m)} | %^{Notes} |" :kill-buffer t)
	("mp" "Piano" table-line (file+headline "~/Documents/org/metrics.org" "Piano")
	 "| %U | %^{Time spent (m)} | %^{Notes} |" :kill-buffer t)
	("mr" "Reading" table-line (file+headline "~/Documents/org/metrics.org" "Reading")
	 "| %U | %^{Book} | %^{Time spent (m)} | %^{Notes} |" :kill-buffer t)))

(use-package ob-rust)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (rust . t)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))

(require 'org-src)
(add-to-list 'org-src-lang-modes '("rust" . "rustic"))

(defun sp/org-insert-checkbox ()
  "Convenience function to insert checkbox in org mode."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-toggle-checkbox)))

(defun sp/org-toggle-checkbox-half ()
  "Convenience function to insert checkbox in org mode."
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-toggle-checkbox)))

(use-package mpv)

(use-package emms
  :commands emms
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (setq emms-source-file-default-directory "~/Music/"))
