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
  (if-let* ((pwd (and (buffer-file-name)
                      (file-name-directory (buffer-file-name)))))
      (python-shell-send-string-no-output (format "cd %s" (shell-quote-argument pwd)))))

(defun pyshell-define-magic-variable-before-send ()
  (when (buffer-file-name)
    (python-shell-send-string-no-output
     (concat (format "__file__ = \"%s\"" (expand-file-name (buffer-file-name)))  "\n"))))

(defun pyshell-switch-to-shell-maybe ()
  (if-let* ((buffer (process-buffer (python-shell-get-process-or-error))))
      (unless (get-buffer-window buffer t)
        (python-shell-switch-to-shell))))

(defun pyshell-set-pydata-display ()
  (let* ((width (window-width (get-buffer-window (process-buffer (python-shell-get-process-or-error)) t)))
         (adj-width (- width 5))
         (code (mapconcat #'identity
                          `("try:"
                            ;; numpy
                            ,(format "    np.set_printoptions(precision=4, threshold=1000, linewidth=%d)" adj-width)
                            ;; pandas
                            "    pd.set_option('display.show_dimensions', True)"
                            ,(format "    pd.set_option('display.max_rows', %d)" (- (window-height) 8))
                            "    pd.set_option('expand_frame_repr', True)"
                            "    pd.set_option('large_repr', 'truncate')"
                            ,(format "    pd.set_option('display.width', %d)" adj-width)
                            "except NameError:"
                            "    pass")
                          "\n")))
    (python-shell-send-string-no-output code)))

;;;###autoload
(defun pyshell-send-region (orig-func &rest args)
  (pyshell-set-pwd-before-send)
  (pyshell-define-magic-variable-before-send)
  (pyshell-set-pydata-display)
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

;; Minor mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pyshell-python-enable ()
  (advice-add 'python-shell-send-region :around #'pyshell-send-region)
  (advice-add 'python-shell-send-buffer :around #'pyshell-send-buffer)
  (advice-add 'python-shell-switch-to-shell :around #'pyshell-switch-to-shell))

(defun pyshell-python-disable ()
  (advice-remove 'python-shell-send-region #'pyshell-send-region)
  (advice-remove 'python-shell-send-buffer #'pyshell-send-buffer)
  (advice-remove 'python-shell-switch-to-shell #'pyshell-switch-to-shell))

;;;###autoload
(define-minor-mode pyshell-mode
  "Enhancements for `python-mode' and `inferior-python-mode'."
  :global t
  (if pyshell-mode
      (pyshell-python-enable)
    (pyshell-python-disable)))

(provide 'pyshell)

;;; pyshell.el ends here
