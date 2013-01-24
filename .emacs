(require 'package)
(package-initialize)

(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(show-paren-mode 1)

;; (icy-mode 1)

;; Mac/homebrew only?
(setq-default ispell-program-name "/usr/local/bin/aspell")

;; Point it straight at homebrew-installed Exuberant ctags
(setq path-to-ctags "/usr/local/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file"
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name 
           (directory-file-name dir-name)))
  )

;; minor modes used in *any* major mode
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (highlight-80+-mode)
	    ))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.[Mm]arkdown" . markdown-mode) 
	    (cons '("\\.md" . markdown-mode) 
		  auto-mode-alist
		  )
	    )
      )

;; requires coffee-mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
          (lambda () (set 'tab-width 2)))

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Requires js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq js2-basic-offset 2)))

(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-local-dictionary "en"))
