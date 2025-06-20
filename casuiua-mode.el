;;; casuiua-mode.el --- major mode for uiua  -*- lexical-binding: t; -*-

; * DONE: Create mode
; * DONE: Create map
; ** DONE Open repl
; ** DONE data to repl
; * TODO: hook up lsp to mode
; ** TODO Syntax highlighting 
; ** TODO inlay hints and reporting
; * TODO: Add command that opens repl and uiua watch on the side (75/25 on height) .. somehow

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
        (if buffer
            (progn
              (comint-send-region process region-start region-end)
              (comint-send-string process "\n"))
          (message "No repl running")))
    (message "No region active")))


(defvar-keymap casuiua-mode-map
  :parent prog-mode-map
  "C-c C-r" #'casuiua-open-repl
  "C-c C-w" #'casuiua-open-watch)

(define-derived-mode casuiua-mode prog-mode "CasUiua" "Major mode for Uiua")

(provide 'casuiua-mode)
