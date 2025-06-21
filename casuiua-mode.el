;;; casuiua-mode.el --- major mode for uiua  -*- lexical-binding: t; -*-

; * DONE: Create mode
; * DONE: Create map
; ** DONE Open repl
; ** DONE data to repl
; * DONE: hook up lsp to mode
; ** TODO Syntax highlighting 
; ** DONE inlay hints and reporting
; * TODO: Add command that opens repl and uiua watch on the side (75/25 on height)
; * TODO: defgroup

;;; START casuiua-mode CONFIG
(defcustom casuiua-uiua-cli-cmd "uiua"
  "Command that starts up uiua"
  :type 'string)

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

(defun casuiua-send-region-to-repl (region-start region-end)
  (interactive "r")
  (if (use-region-p)
      (let* ((buffer (get-buffer-create "*uiua-repl*"))
             (proc-alive (comint-check-proc buffer))
             (process (get-buffer-process buffer)))
        (if (and proc-alive buffer)
            (progn
              (comint-send-region process region-start region-end)
              (comint-send-string process "\n"))
          (message "No repl running")))
    (message "No region active")))


(defvar-keymap casuiua-mode-map
  :parent prog-mode-map
  "C-c C-r" #'casuiua-open-repl
  "C-c C-w" #'casuiua-open-watch
  "C-c C-c" #'casuiua-send-region-to-repl)

(define-derived-mode casuiua-mode prog-mode "CasUiua" "Major mode for Uiua")

;; We should not need treesitter, lsp should give highlighting automatically
;; (when (require 'tree-sitter nil 'noerror)
;;   (add-hook 'casuiua-mode-hook #'tree-sitter-mode)
;;   (add-hook 'casuiua-mode-hook #'tree-sitter-hl-mode))


;;; END casuiua-mode CONFIG

;; Faces
(defface casuiua-noadic-function-face   '((t (:foreground "#ed5e6a"))) "Noadic function")
(defface casuiua-monadic-function-face  '((t (:foreground "#95d16a"))) "Monadic function")
(defface casuiua-dyadic-function-face   '((t (:foreground "#54b0fc"))) "Dyadic function")
(defface casuiua-triadic-function-face  '((t (:foreground "#8078f1"))) "Triadic function")
(defface casuiua-tetradic-function-face '((t (:foreground "#f576d8"))) "Tetradic function")
(defface casuiua-monadic-modifier-face  '((t (:foreground "#f0c36f"))) "Monadic modifier")
(defface casuiua-dyadic-modifier-face   '((t (:foreground "#cc6be9"))) "Dyadic modifier")
(defface casuiua-triadic-modifier-face  '((t (:foreground "#F5A9B8"))) "Triadic modifier")
(defface casuiua-uiua-number-face       '((t (:foreground "#eeaa55"))) "Uiua number")
(defface casuiua-uiua-module-face       '((t (:foreground "#d7be8c"))) "Uiua module")


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
          '(("noadic_function" . casuiua-uiua-noadic-function-face)
            ("monadic_function" . casuiua-uiua-monadic-function-face)
            ("dyadic_function" . casuiua-uiua-dyadic-function-face)
            ("triadic_function" . casuiua-uiua-triadic-function-face)
            ("tetradic_function" . casuiua-uiua-tetradic-function-face)
            ("monadic_modifier" . casuiua-uiua-monadic-modifier-face)
            ("dyadic_modifier" . casuiua-uiua-dyadic-modifier-face)
            ("triadic_modifier" . casuiua-uiua-triadic-modifier-face)
            ("uiua_module" . casuiua-uiua-module-face)))
    ;; (add-hook 'casuiua-mode-hook
    ;;           (lambda ()
    ;;             (lsp)
    ;;             (lsp-semantic-tokens-mode  ; this must be reran in the buffer?
    ;;              )))))
    ))



;; (See https://gist.github.com/magistau/4c12ab54663f911e6a7560a807b63f1c)
;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(uiua-ts-mode . ("uiua" "lsp")))
;;   (setq-default eglot-workspace-configuration
;;                 '(:uiua.inlayHints.values :json-false
;;                   :uiua.inlayHints.bindingSignatureHints t
;;                   :uiua.inlayHints.inlineSignatureHints t
;;                   :uiua.inlayHints.inlineHintMinLength 3))
;;   :hook ((prog-mode . eglot-ensure)))
;; (use-package eglot-semtok
;;   :config
;;   (setq eglot-semtok-faces
;; 	(append
;; 	 '("uiua_number" (("" "" (:foreground "#ea5")))
;; 	   "noadic_function" (("" "" (:foreground "#ed5e6a")))
;; 	   "monadic_function" (("" "" (:foreground "#95d16a")))
;; 	   "dyadic_function" (("" "" (:foreground "#54b0fc")))
;; 	   "triadic_function" (("" "" (:foreground "#8078f1")))
;; 	   "tetradic_function" (("" "" (:foreground "#f576d8")))
;; 	   "monadic_modifier" (("" "" (:foreground "#f0c36f")))
;; 	   "dyadic_modifier" (("" "" (:foreground "#cc6be9")))
;; 	   "triadic_modifier" (("" "" (:foreground "#F5A9B8")))
;; 	   "uiua_module" (("" "" (:foreground "#d7be8c"))))
;; 	 eglot-semtok-faces))
;;   :hook ((eglot-connect . eglot-semtok-on-connected)
;; 	 (uiua-ts-mode . eglot-semtok-font-lock-init)))


;;; END LSP CONFIGURATION

(provide 'casuiua-mode)

;; (lsp-request "textDocument/semanticTokens/full"
;;              `(:textDocument ,(lsp--text-document-identifier)))


;; (lsp-request "textDocument/semanticTokens/didOpen"
;;              `(:textDocument ,(lsp--text-document-identifier)))


                                        ; tokenTypes: [comment parameter uiua_number uiua_string stack_function noadic_function monadic_function dyadic_function triadic_function tetradic_function monadic_modifier dyadic_modifier triadic_modifier uiua_module comment parameter number lifetime none property string method none none type keyword none namespace]


;; (setq lsp-enable-semantic-highlighting t) ;; this might be redundant, but try it
;; (setq lsp-font-lock-enable t)             ;; MUST be t
;; (setq lsp-log-io t)
;; (defun my-lsp-fix-semantic-token-legend (orig-fn &rest args)
;;   "Fix duplicated tokenTypes in semanticTokensProvider legend."
;;   (let ((caps (apply orig-fn args)))
;;     (when-let* ((provider (gethash "semanticTokensProvider" caps))
;;                 (legend (gethash "legend" provider))
;;                 (types (gethash "tokenTypes" legend)))
;;       ;; Remove duplicates but keep order
;;       (puthash "tokenTypes" (vconcat (delete-dups (append types nil))) legend))
;;     caps))

;(advice-add 'lsp--server-capabilities :around #'my-lsp-fix-semantic-token-legend)

;; (advice-add 'lsp-semantic-tokens--refresh :before
;;             (lambda (&rest _args) (message "lsp-semantic-tokens--refresh triggered")))
;; (member #'lsp-semantic-tokens--on-change after-change-functions)
;; (add-hook 'after-change-functions #'lsp-semantic-tokens--on-change nil t)

