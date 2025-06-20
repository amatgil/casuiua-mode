;;; casuiua-mode.el --- major mode for uiua  -*- lexical-binding: t; -*-

; TODO: Create mode
; TODO: Create map
;    - Send data to repl
; TODO: hook up lsp to mode
; TODO: syntax highlighting using said lsp
; TODO: Add command that opens repl and uiua watch on the side (75/25 on height) .. somehow

(defvar casuiua-uiua-cli-cmd "uiua")
(defvar casuiua-uiua-cli-args '("repl"))

(defun casuiua-open-repl ()
  (interactive)
  (comint-run casuiua-uiua-cli-cmd casuiua-uiua-cli-args))


(defvar-keymap casuiua-mode-map
  :parent prog-mode-map
  :doc "Keymap for casuiua"
  "C-c C-r" #'casuiua-open-repl)

(define-derived-mode casuiua-mode prog-mode "CasUiua" "Major mode for Uiua")
