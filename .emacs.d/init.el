;; init.el -- Emacs configuration entry point

;; Startup Performance
;; ===================

;; Reduce the frequency of garbage collection
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections"
		     (format "%.2f seconds"
			     (float-time (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Package Management
;; ==================

;; straight.el
;; -----------

;; https://github.com/raxod502/straight.el
;; Declarative and reproducible Emacs package manager

;; This snippet installs straight if it's not already installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
;; -----------

;; https://github.com/jwiegley/use-package
;; A macro for loading and configuring packages

(straight-use-package 'use-package)

;; Configure use-package to install packages with straight
(setq straight-use-package-by-default t)

;; Tidy .emacs.d directory
;; =======================

;; Keep transient files outside of the dotfiles repo directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;; Use no-littering to automatically set common paths to the user-emacs-directory
(use-package no-littering)

;; Default Coding System
;; =====================

(set-default-coding-systems 'utf-8)

;; Keybindings
;; ===========

;; macOS Settings
;; --------------

;; Configure the left Option key to behave as the Meta key
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

(setq mac-command-modifier 'super)

;; which-key
;; ---------

;; https://github.com/justbur/emacs-which-key
;; Displays available keybindings in a popup

(use-package which-key
  :diminish which-key-mode
  
  :config
  (which-key-mode)
  
  :custom
  (which-key-idle-delay 0.3))

;; General Configuration
;; =====================

;; User Interface
;; --------------

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; Disable visible scroll bar
(tool-bar-mode -1) ;; Disable the toolbar
(tooltip-mode -1) ;; Disable tooltips
(set-fringe-mode 10) ;; Add horizontal frame padding

(setq visible-bell t) ;; Flash the window when something goes wrong

(setq use-dialog-box nil) ;; Disable dialog box prompts

;; Enable line numbers
(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable line numbers for some modes derived from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Don't warn for following symlinks
(setq vc-follow-symlinks t)

;; Don't warn when advice is added for functions
(setq ad-redefinition-action 'accept)

;; Theme
;; -----

;; DOOM Themes
;; https://github.com/hlissner/emacs-doom-themes

(use-package doom-themes)
(setq doom-one-brighter-comments t)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)

;; Mode Line
;; ---------

;; Diminish
;; https://github.com/myrjola/diminish.el
;; Diminished modes are minor modes with no modeline display

(use-package diminish)

;; DOOM Modeline
;; https://github.com/seagle0128/doom-modeline
;; A fancy and fast mode-line inspired by minimalism design

;; NOTE: You must run (all-the-icons-install-fonts) one time after installing this package

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

;; Minions
;; https://github.com/tarsius/minions
;; A minor-mode menu for the mode line

(use-package minions
  :config
  (minions-mode 1))

;; Workspaces
;; ----------

;; perspective
;; https://github.com/nex3/perspective-el
;; Provides multiple named workspaces, similar to multiple desktops in window managers

;; TODO Test this with Ivy
(use-package perspective
  :config
  ;; Running persp-mode twice resets the perspectives list
  (unless (equal persp-mode t)
    (persp-mode)))

;; Auto-Saving
;; -----------

;; super-save
;; https://github.com/bbatsov/super-save
;; Save Emacs buffers when they lose focus 

(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode 1)
  :custom
  (super-save-auto-save-when-idle t))

;; Auto-Reverting Changed Files
;; ----------------------------

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Highlight Matching Braces
;; -------------------------

;; https://www.emacswiki.org/emacs/ShowParenMode

(setq show-paren-delay 0)
(set-face-foreground 'show-paren-match (face-background 'highlight))
(show-paren-mode 1)

;; Editing Configuration
;; =====================

;; Tab Widths
;; ----------

(setq-default tab-width 2)

;; Use spaces instead of tabs
;; --------------------------

(setq-default indent-tabs-mode nil)

;; Automatically clean whitespace
;; ------------------------------

;; ws-butler
;; https://github.com/lewang/ws-butler
;; Unobtrusively trim extraneous white-space *ONLY* in lines edited

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Completions
;; ===========

;; Ivy
;; ---

(setq enable-recursive-minibuffers t)

;; Preserve minibuffer history
;; ---------------------------

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; Vertico
;; -------

(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main")
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu")
  :config
  (corfu-global-mode))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
