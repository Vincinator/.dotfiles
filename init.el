;; ==== Packages

;; Melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(require 'use-package)
(add-to-list 'load-path "~/.emacs.d/vince")

;; == evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; == Helm
(require 'setup-helm)
(require 'setup-helm-gtags)

(setq helm-exit-idle-delay 0)

;; == projectile

(require 'projectile)
(define-key projectile-mode-map (kbd "M-p") #'projectile-command-map)
(setq projectile-indexing-method 'turbo-alien)

(projectile-global-mode)

(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(helm-projectile-on)

;; == CEDET

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

;; == function-args
(fa-config-default)


;; == NeoTree
(require 'neotree)
(add-hook 'neo-after-create-hook
	  (lambda (&rest _) (display-line-numbers-mode -1)))

;; == speedbar sr
(require 'sr-speedbar)


;; == shell-pop
(require 'shell-pop)


;; == ace-window

(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; ==== Startup Config
(setq inhibit-startup-screen t)

(global-display-line-numbers-mode)


(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))


;; ==== Keybindings


;; == Window resize keys
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; == NeoTree
(global-set-key [f8] 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; == windmove (shift+arrowkey to switch window)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key [(super j)]  'windmove-down)
(global-set-key [(super k)]  'windmove-up)
(global-set-key [(super h)]  'windmove-left)
(global-set-key [(super l)]  'windmove-right)


;; == Magit
(require 'evil-magit)


;; == Sr-Speedbar
(define-key speedbar-mode-map (kbd "TAB") 'speedbar-edit-line)

;; ==== Style
(set-face-attribute 'default nil :height 180)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; NeoTree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("d3cf4dfbff156bdd42c874c48b35cd86a204934ee6ddb3462ab9d9ea4d5a0775" "e03d2f414fb109f3920752b10b92323697174f49d577da9e69979edbb147a921" default)))
 '(package-selected-packages
   (quote
    (magit ace-window helm-fuzzy-find shell-pop projectile-speedbar sr-speedbar function-args ggtags 0blayout sublimity zenburn-theme helm-gtags helm-projectile use-package helm-ack helm linum-relative evil)))
 '(semantic-mode t)
 '(shell-pop-default-directory "/home/vincent/")
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/zsh")
 '(shell-pop-universal-key "C-1")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30))

