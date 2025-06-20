;;; casuiua-mode.el --- major mode for uiua  -*- lexical-binding: t; -*-

; TODO: Create mode
; TODO: Create map
;    - DONE Open repl
;    - Send data to repl
; TODO: hook up lsp to mode
; TODO: syntax highlighting using said lsp
; TODO: Add command that opens repl and uiua watch on the side (75/25 on height) .. somehow

(defcustom casuiua-uiua-cli-cmd "uiua")

(defun casuiua-open-repl ()
  (interactive)
  (let* ((buffer (get-buffer-create "*uiua-repl*"))
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
  :doc "Keymap for casuiua"
  "C-c C-r" #'casuiua-open-repl
  "C-c C-w" #'casuiua-open-watch)

(define-derived-mode casuiua-mode prog-mode "CasUiua" "Major mode for Uiua")
