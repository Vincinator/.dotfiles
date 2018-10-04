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

;; == NeoTree
(require 'neotree)


;; ==== Keybindings

;; == NeoTree
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
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


;; ==== Style
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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
    (sublimity zenburn-theme helm-gtags helm-projectile use-package helm-ack helm linum-relative evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

