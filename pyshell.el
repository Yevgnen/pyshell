;;; pyshell.el ---
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 1.0.0
;; Keywords: python
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Python interactive shell improvements.
;;
;; See documentation on https://github.com/Yevgnen/pyshell.

;;; Code:

(require 'python)

(defcustom pyshell-switch-shell-after-send nil
  "Whether switch to shell buffer after send."
  :type 'boolean)

(defcustom pyshell-set-pwd-before-send t
  "Whether set pwd to current directory before send."
  :type 'boolean)

(defun pyshell-pop-to-buffer-dwim (buffer &optional pop-to-buffer-function &rest args)
  (let ((window (get-buffer-window buffer 'all-frames))
        (pop-to-buffer-function (or pop-to-buffer-function #'pop-to-buffer)))
    (if (null window)
        (apply pop-to-buffer-function (push buffer args))
      (let ((frame (window-frame window)))
        (unless (eq frame (window-frame))
          (select-frame-set-input-focus frame))
        (select-window window)))))

;; Stuffs set int `python-mode' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pyshell-set-pwd-before-send ()
  (if pyshell-set-pwd-before-send
      (if-let* ((pwd (and (buffer-file-name)
                          (file-name-directory (buffer-file-name)))))
          (python-shell-send-string-no-output (format "cd %s" (shell-quote-argument pwd))))))

(defun pyshell-define-magic-variable-before-send ()
  (when (buffer-file-name)
    (python-shell-send-string-no-output
     (concat (format "__file__ = \"%s\"" (expand-file-name (buffer-file-name)))  "\n"))))

(defun pyshell-switch-to-shell-maybe ()
  (if pyshell-switch-shell-after-send
      (if-let* ((buffer (process-buffer (python-shell-get-process-or-error))))
          (unless (get-buffer-window buffer t)
            (python-shell-switch-to-shell)))))

;;;###autoload
(defun pyshell-send-region (orig-func &rest args)
  (pyshell-set-pwd-before-send)
  (pyshell-define-magic-variable-before-send)
  (apply orig-func args)
  (pyshell-switch-to-shell-maybe))

;;;###autoload
(defun pyshell-send-buffer (orig-func &rest args)
  (apply orig-func args))

;;;###autoload
(defun pyshell-send-dwim ()
  (interactive)
  (let ((start) (end))
    (save-excursion
      (progn (backward-paragraph)
             (setq start (point)))
      (progn (forward-paragraph)
             (setq end (point))))
    (pyshell-send-region start end)))

;;;###autoload
(defun pyshell-switch-to-shell (orig-func &rest orig-args)
  (if-let* ((proc (condition-case nil
                      (python-shell-get-process-or-error)
                    (error nil)))
            (proc-buf (process-buffer proc)))
      (pyshell-pop-to-buffer-dwim proc-buf
                                  (lambda (&rest args)
                                    (apply orig-func (cdr args)))
                                  orig-args)
    (run-python nil nil t)))

;; Stuffs set in `inferior-python-mode' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun pyshell-eval-example-or-interrupt ()
  "Better C-c C-c for `inferior-python-mode'."
  (interactive)
  (let ((line (thing-at-point 'line t))
        (regex "^[ ]*\\(>>>\\|\\.\\.\\.\\) ")
        (continue-regex "^[ ]*\\.\\.\\."))
    (if (and line (string-match regex line))
        (let ((lines))
          (save-excursion
            (while (string-match regex (thing-at-point 'line t))
              (previous-line))
            (next-line)
            (while (string-match regex (thing-at-point 'line t))
              (let ((line (string-trim (thing-at-point 'line t)))
                    (last-line (nth 0 lines)))
                (if (string-match continue-regex line)
                    (setf (nth 0 lines) (concat last-line (substring line 4)))
                  (push (substring line 4) lines)))
              (next-line)))
          (python-shell-send-string (concat (mapconcat #'identity (reverse lines) ";")
                                            "\n")
                                    (get-buffer-process (current-buffer)))
          (goto-char (point-max)))
      (call-interactively #'comint-interrupt-subjob))))

(defun pyshell-input-sender-hook ()
  "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
  (setq comint-input-sender
        (lambda (proc command)
          (cond
           ;; Check for clear command and execute it.
           ((string-match "^[ \t]*\\(clear\\|c\\)[ \t]*$" command)
            (comint-send-string proc "\n")
            (comint-clear-buffer))
           ((string-match "^[ \t]*clc[ \t]*$" command)
            (comint-send-string proc "%reset -f\n"))
           ((string-match "^plt\\.show[ \t]*()[ \t]*" command)
            (comint-send-string proc "plt.show(block=False)\n"))
           ;; Send other commands to the default handler.
           (t (comint-simple-send proc command))))))

;; Minor mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pyshell-python-enable ()
  (advice-add 'python-shell-send-region :around #'pyshell-send-region)
  (advice-add 'python-shell-send-buffer :around #'pyshell-send-buffer)
  (advice-add 'python-shell-switch-to-shell :around #'pyshell-switch-to-shell))

(defun pyshell-python-disable ()
  (advice-remove 'python-shell-send-region #'pyshell-send-region)
  (advice-remove 'python-shell-send-buffer #'pyshell-send-buffer)
  (advice-remove 'python-shell-switch-to-shell #'pyshell-switch-to-shell))

(defun pyshell-shell-enable ()
  (add-hook 'inferior-python-mode-hook #'pyshell-input-sender-hook))

(defun pyshell-shell-disable ()
  (remove-hook 'inferior-python-mode-hook #'pyshell-input-sender-hook))

(defun pyshell-enable ()
  (pyshell-python-enable)
  (pyshell-shell-enable))

(defun pyshell-disable ()
  (pyshell-python-disable)
  (pyshell-shell-disable))

;;;###autoload
(define-minor-mode pyshell-mode
  "Enhancements for `python-mode' and `inferior-python-mode'."
  :global t
  (if pyshell-mode
      (pyshell-enable)
    (pyshell-disable)))

(provide 'pyshell)

;;; pyshell.el ends here
