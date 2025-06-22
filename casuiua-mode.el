;;; casuiua-mode.el --- major mode for uiua  -*- lexical-binding: t; -*-

; * DONE: Create mode
; * DONE: Create map
; ** DONE Open repl
; ** DONE data to repl
; * DONE: hook up lsp to mode
; ** DONE Syntax highlighting 
; ** DONE inlay hints and reporting
; * TODO: Add command that opens repl and uiua watch on the side (75/25 on height)
; * TODO: defgroup
; * TODO: add M-; casuiua-comment-region

;;; START casuiua-mode CONFIG
(defgroup casuiua-group nil
  "Casenc's configuration uiua through casuiua-mode."
  :prefix "casuiua-"
  :link '(url-link "https://github.com/amatgil/casuiua-mode"))
(defcustom casuiua-uiua-cli-cmd "uiua"
  "Command that starts up uiua"
  :type 'string
  :group 'casuiua-group)

(defun casuiua-open-repl ()
  (interactive)
  (let* ((buffer (get-buffer))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    (unless proc-alive ;; recreate if dead
      (message "Starting up repl")
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "Uiua Repl" buffer
               casuiua-uiua-cli-cmd nil '("repl"))))
    (when buffer
      (pop-to-buffer buffer))))


(defun casuiua-open-watch ()
  (interactive)
  (let* ((buffer (get-buffer-create "*uiua-watch*"))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    (unless proc-alive ;; recreate if dead
      (with-current-buffer buffer
        (message "Starting up watch window")
        (apply 'make-comint-in-buffer "Uiua Watch" buffer
               casuiua-uiua-cli-cmd nil '("watch"))))
    (when buffer
      (pop-to-buffer buffer))))


(defvar-keymap casuiua-mode-map
  :parent prog-mode-map
  "C-c C-r" #'casuiua-open-repl
  "C-c C-w" #'casuiua-open-watch
  "C-c C-c" #'casuiua-send-region-to-repl)

(define-derived-mode casuiua-mode prog-mode "CasUiua" "Major mode for Uiua")

;; Faces
(defface casuiua-noadic-function-face   '((t (:foreground "#f38ba8"))) "Noadic function"   :group 'casuiua-group)
(defface casuiua-monadic-function-face  '((t (:foreground "#a6e3a1"))) "Monadic function"  :group 'casuiua-group)
(defface casuiua-dyadic-function-face   '((t (:foreground "#89b4fa"))) "Dyadic function"   :group 'casuiua-group)
(defface casuiua-triadic-function-face  '((t (:foreground "#f5c2e7"))) "Triadic function"  :group 'casuiua-group)
(defface casuiua-tetradic-function-face '((t (:foreground "#f2cdcd"))) "Tetradic function" :group 'casuiua-group)
(defface casuiua-monadic-modifier-face  '((t (:foreground "#f9e2af"))) "Monadic modifier"  :group 'casuiua-group)
(defface casuiua-dyadic-modifier-face   '((t (:foreground "#cba6f7"))) "Dyadic modifier"   :group 'casuiua-group)
(defface casuiua-triadic-modifier-face  '((t (:foreground "#94e2d5"))) "Triadic modifier"  :group 'casuiua-group)
(defface casuiua-uiua-comment-face      '((t (:foreground "#313244"))) "Uiua comment"      :group 'casuiua-group)
(defface casuiua-uiua-number-face       '((t (:foreground "#fab387"))) "Uiua number"       :group 'casuiua-group)
(defface casuiua-uiua-module-face       '((t (:foreground "#6c7086"))) "Uiua module"       :group 'casuiua-group)
;;; END casuiua-mode CONFIG


;;; START LSP CONFIGURATION
;; (See https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/)

(when (require 'lsp-mode nil 'noerror)
  (progn
    ;; "casuiua-mode" corresponds to language "uiua"
    (add-to-list 'lsp-language-id-configuration '(casuiua-mode . "uiua"))
    (lsp-register-client (make-lsp-client
                          :new-connection (lsp-stdio-connection `(,casuiua-uiua-cli-cmd "lsp"))
                          :activation-fn (lsp-activate-on "uiua")
                          :server-id 'uiua-lsp))
    (setq lsp-semantic-token-faces
          '(("noadic_function" . casuiua-noadic-function-face)
            ("monadic_function" . casuiua-monadic-function-face)
            ("dyadic_function" . casuiua-dyadic-function-face)
            ("triadic_function" . casuiua-triadic-function-face)
            ("tetradic_function" . casuiua-tetradic-function-face)
            ("monadic_modifier" . casuiua-monadic-modifier-face)
            ("dyadic_modifier" . casuiua-dyadic-modifier-face)
            ("triadic_modifier" . casuiua-triadic-modifier-face)
            ("comment" . casuiua-uiua-comment-face)
            ("uiua_module" . casuiua-uiua-module-face)
            ("uiua_number" . casuiua-uiua-number-face)))
    (add-hook 'casuiua-mode-hook
              (lambda ()
                (lsp)
                (lsp-semantic-tokens-mode  ; this must be reran in the buffer?
                 )))))

;;; END LSP CONFIGURATION

(provide 'casuiua-mode)
