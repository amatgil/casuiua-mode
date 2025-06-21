;;; casuiua-mode.el --- major mode for uiua  -*- lexical-binding: t; -*-

; * DONE: Create mode
; * DONE: Create map
; ** DONE Open repl
; ** DONE data to repl
; * DONE: hook up lsp to mode
; ** TODO Syntax highlighting 
; ** DONE inlay hints and reporting
; * TODO: Add command that opens repl and uiua watch on the side (75/25 on height)

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
                          :server-id 'uiua-lsp))))

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
