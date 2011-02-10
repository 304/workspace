; включаем буфер обмена emacs в буфер операционной системы
(setq x-select-enable-clipboard t)

; делаем чтоб при открытии emacs'а не было мусора, а был открыт только один буфер *scratch*
(setq inhibit-splash-screen t)

; показывать номер строки и столбца
(column-number-mode t)

; скролл бар убрать
(scroll-bar-mode nil)

; 
(setq windmove-default-keybindings 1)


(windmove-default-keybindings 'meta)
;(desktop-save-mode t)

; краткие выбор варианта
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key [?\C-z] 'shell)
(global-set-key [?\C-x ?\C-g] 'goto-line)

(global-set-key [f5] 'php-mode)
(global-set-key [f6] 'html-mode)
(global-set-key [f7] 'javascript-mode)
(global-set-key [f8] 'sql-mode)
(global-set-key [f9] 'css-mode)



(setq browse-url-browser-function 'w3m-browse-url)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
    (require 'yasnippet) ;; not yasnippet-bundle
    (yas/initialize)
    (yas/load-directory "/usr/share/emacs/site-lisp/yas/snippets")



(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
 
;; This tab override shouldn't be necessary given ido's default 
;; configuration, but minibuffer-complete otherwise dominates the 
;; tab binding because of my custom tab-completion-everywhere 
;; configuration.
(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-complete)))




(require 'sdcv)
(setq sdcv-dictionary-simple-list        ;; a simple dictionary list
	  '("dictd_www.mova.org_slovnyk_en-ru")
)
(global-set-key (kbd "C-c C-d") 'sdcv-search-input+)


(setq org-todo-keywords
    '((sequence "TODO" "WAITING" "FAIL" "|" "PROCESS" "|" "DONE")))


     (setq org-todo-keyword-faces
           '(("TODO" . (:foreground "red" :weight bold))
			 ("FAIL" . (:foreground "red" :weight bold))
			 ("PROCESS" . (:foreground "yellow" :weight bold))))


(prefer-coding-system 'cp1251)
(prefer-coding-system 'utf-8-unix)

(require 'w3m-load)


(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

; включить номера строк
(global-linum-mode 1)

; настройки tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-default-user "java")

(add-to-list 'tramp-default-proxies-alist
                  '("\\.ru\\'" "\\`root\\'" "/ssh:%h:"))

;; ssh для хоста
(defun random.ru ()
  (interactive)
  (find-file "/su:random.ru:/")
)


(require 'color-theme)
(require 'color-theme-tango)
;;
(if window-system
    (color-theme-tango))

;; просмотр каталогов в одном буфере
(add-hook 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "^")
     (lambda () (interactive) (find-alternate-file "..")))
 ))
(put 'dired-find-alternate-file 'disabled nil)

; python mode
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))


(require 'php-mode)
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(tool-bar-mode nil)

;; Display the time
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

; автовыравнивание
(define-key php-mode-map (kbd "RET") 'newline-and-indent) 


; подсветка скобок
(setq show-paren-mode t)

; табы выключены
(setq indent-tab-mode nil)

; кол-во отступов
(setq c-basic-offset 4)

(setq c-default-style "bsd")

; нет отступа во вложенных выражениях
(c-set-offset 'substatement-open  0)

(defun backward-kill-word-or-kill-region (arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning)
                   (region-end))
    (backward-kill-word arg)))

;; Like everywhere else, C-w kills word or region.
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word-or-kill-region)

; переключалка буферов по ctrl+tab
(global-set-key [(control tab)] 'ido-switch-buffer)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(default-input-method "russian-computer")
 '(org-agenda-files (quote ("~/TODO.org")))
 '(org-time-stamp-custom-formats (quote ("<%d.%m.%y>" . "<%m/%d/%y %a %H:%M>")))
 '(php-mode-speedbar-open nil))
  
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "red" :slant italic :weight bold))))
 '(font-lock-string-face ((t (:foreground "yellow"))))
 '(org-level-2 ((t (:foreground "khaki" :weight extra-bold))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "pink" :weight ultra-bold))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "orange" :weight extra-bold))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "lightgreen" :weight extra-bold)))))
