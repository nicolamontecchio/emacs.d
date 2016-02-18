(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; mac: switch meta/hyper key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; VISUAL ASPECT
(setq inhibit-splash-screen t)                                  ;; no splash screen
(tool-bar-mode -1)                                              ;; no toolbar
(scroll-bar-mode -1)                                            ;; no scroll bar
(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)                               ;; disable bell
(set-fringe-mode 0)                                             ;; (also here)
(show-paren-mode 1)                                             ;; show matching parents
(add-hook 'before-save-hook 'delete-trailing-whitespace)        ;; delete trailing whitespaces on save
(column-number-mode)                                            ;; display column number in command buf.
(add-hook 'prog-mode-hook 'subword-mode)                        ;; camel-case kill-word
(display-time-mode 1)
(wrap-region-global-mode)
(global-linum-mode t)                                           ;; line numbering on
(setq linum-format "%d ")					;; adjust line number column size
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'prog-mode-hook #'hs-minor-mode)                      ;; hs-minor-mode for all programming modes

;; GLOBAL KEY BINDINGS
(global-set-key (kbd "C-M-/")   'toggle-comment-region)                 ;; comment code - custom function below
(global-set-key (kbd "C-M-j")	'direx-project:jump-to-project-root)    ;; tree view of project directory
(global-set-key (kbd "M-k")     'kill-this-buffer)                      ;; kill current buffer
(global-set-key (kbd "C-M-i")   'company-complete)                      ;; autocomplete w/ company mode
(global-set-key (kbd "C-;")     'scroll-down-line)
(global-set-key (kbd "C-'")     'scroll-up-line)
(global-set-key (kbd "C-x o")   'switch-window)                         ;; for when there are more than 2 windows
(global-set-key (kbd "C-=")     'er/expand-region)                      ;; expand region

(global-set-key (kbd "M-j")	'avy-goto-word-1)                       ;; avy-jump (on word)
(global-set-key (kbd "M-[")	'avy-goto-char)				;; avy-jump (on any char)
(global-set-key (kbd "M-]")	'avy-goto-char-2)                       ;; avy-jump (on any two chars)
(global-set-key (kbd "C-M-y")   'toggle-truncate-lines)                 ;; switch on-off word wrap
(global-set-key (kbd "C-M-t")   'hs-toggle-hiding)                      ;; hide/show block
(global-set-key (kbd "M-x")     'smex)                                  ;; nicer M-x

;; multiple cursors key bindings
(global-set-key (kbd "C->")		'mc/mark-next-like-this)
(global-set-key (kbd "C-<")		'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-l")		'mc/edit-lines)
(global-set-key (kbd "C-c C-<")		'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<mouse-1>")	'mc/add-cursor-on-click)
(global-set-key (kbd "C-c C-l")		'recompile)

;; custom comment toggle function
(defun toggle-comment-region ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; full screen mode
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; graphic-only theme options
(setq pretty-themes '(atom-dark material heroku greymatters))
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(defun cycle-pretty-themes ()
  (interactive)
  (load-theme (car pretty-themes) t)
  (setq pretty-themes (append (cdr pretty-themes) (list (car pretty-themes)))))

;; graphic-only keybindings
(if (display-graphic-p)
    (progn
      ;; default win size
      (add-to-list 'default-frame-alist '(height . 58))
      (add-to-list 'default-frame-alist '(width . 120))
      ;; key-bindings for window resizing/navigation
      (global-set-key (kbd "H-<return>")	'toggle-fullscreen)             ;; toggle full screen
      (global-set-key (kbd "H-M-<left>")	'shrink-window-horizontally)	;; shrink window horizontally
      (global-set-key (kbd "H-M-<right>")	'enlarge-window-horizontally)	;; enlarge window horizontally
      (global-set-key (kbd "H-M-<down>")	'shrink-window)			;; shrink window vertically
      (global-set-key (kbd "H-M-<up>")		'enlarge-window)		;; enlarge window vertically
      (global-set-key [H-left]			'windmove-left)                 ;; move to left windnow
      (global-set-key [H-right]			'windmove-right)                ;; move to right window
      (global-set-key [H-up]			'windmove-up)                   ;; move to upper window
      (global-set-key [H-down]			'windmove-down)                 ;; move to lower window
      ;; other key bindings
      (global-set-key (kbd "H-M-s")	'magit-status)          ;; git status
      (global-set-key (kbd "H-M-o")	'projectile-find-file)  ;; open file in projectile mode
      (global-set-key (kbd "H-M-t")	'hs-toggle-hiding)      ;; toggle show/hide block
      (global-set-key (kbd "H-a")	'mark-whole-buffer)     ;; select all
      (global-set-key (kbd "H-M-b")	'browse-url-at-point)   ;; open url under cursor in chrome
      (global-set-key (kbd "H-M-l")	'cycle-pretty-themes)
      ;; other
      (setq linum-format "%3d ")				;; adjust line number column size
      (custom-set-faces
       '(default ((t (:inherit nil :weight medium :height 130 :width normal :family "source code pro")))))
      (cycle-pretty-themes)))

;; global-behavior
(global-auto-revert-mode 1)				;; automatically reload files when changed
(ido-mode 1)						;; IDO
(define-key global-map (kbd "RET") 'newline-and-indent) ;; auto indentation


;; yasnippet helper functions
(setq
 yas-already-loaded nil)
(defun yas-reload-if-necessary ()
  (progn
    (yas-minor-mode)
    (when (not yas-already-loaded)
      (progn
	(yas-reload-all)
	(setq yas-already-loaded 1)))))

;; C/C++
(setq c-default-style "linux" c-basic-offset 2)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'company-mode)
(defun override-cc-cl ()
       (local-set-key (kbd "C-c C-l") 'recompile))
(add-hook 'c-mode-common-hook 'override-cc-cl)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; cron mode
(add-to-list 'auto-mode-alist '("\\.cron$" . crontab-mode))

;; octave/matlab
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(setq hi2-show-indentations nil)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; rust
(setq rust-indent-offset 2)

;; LISPs
(add-hook 'emacs-lisp-mode-hook                   'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook  'paredit-mode)
(add-hook 'scheme-mode-hook                       'paredit-mode)
(add-hook 'clojure-mode-hook                      'paredit-mode)
(add-hook 'clojure-mode-hook                      'company-mode)
(add-hook 'scheme-mode-hook                      'hs-minor-mode)
(setq scheme-program-name "/usr/local/bin/guile")

;; javascript
(setq js-indent-level 2)

;; json pretty print
(defun jj ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "jq ." (current-buffer) t)))

;; avro schemas (json syntax)
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))

;; ipython
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--pylab"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(add-hook 'python-mode-hook 'yas-reload-if-necessary)
(add-hook 'python-mode-hook
	  (lambda ()
	    (progn
	      (company-mode)
	      (set (make-local-variable 'company-backends)
		   '((company-yasnippet company-dabbrev-code))))))


;; web-mode
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ngt?\\'" . web-mode))

;; scala
(defun ss ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "scalariform --stdin" (current-buffer) t)))

;; emacs server
(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))
(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; julia
(setq inferior-julia-program-name "~/bin/julia")

;; miscellanea
(setq tramp-use-ssh-controlmaster-options nil)							;; fix tramp without breaking ghc-mod
(put 'temporary-file-directory 'standard-value '((file-name-as-directory "~/tmp/tramp")))	;; tramp path
(put 'erase-buffer 'disabled nil)
(defun fci ()
  (interactive)
  (fci-mode)
  (setq fci-rule-column 80))
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'wgrep)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :weight medium :height 130 :width normal :family "source code pro")))))
