;;; schroot-mode.el --- work transparently in a chroot -*- lexical-binding: t -*-

;; Copyright (C) 2017 Jacob Salzberg

;; Author: Jacob Salzberg <jsalzbergedu@yahoo.com>
;; Keywords: Programming
;; Version: 0.0.1
;; Package-Requires ((emacs "24.4") (cl-lib "somenumber"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; Inspired by this article http://gernotklingler.com/blog/use-chroot-jail-software-development/

;;; Code:
(require 'cl-lib)
(eval-when-compile (require 'cl))

(defvar schroot-mode-configs (list)
  "An alist that relates each project directory to a config.")

(defvar schroot-mode-files-loc (expand-file-name "./")
  "The location of `schroot_pass.sh` and `schroot_pass_bash.sh`, used to pass the configuration and command.")

(defun schroot-mode-pass-loc ()
  "The location of `schroot_pass.sh`"
  (expand-file-name "schroot_pass.sh" schroot-mode-files-loc))

(defun schroot-mode-pass-bash-loc ()
  "The location of `schroot_pass_bash.sh"
  (expand-file-name "schroot_pass_bash.sh" schroot-mode-files-loc))

(defun schroot-mode-add-dir-config (dir config)
  "A function that adds a directory where commands must be executed in a chroot as well as the schroot configuration file associated with that directory."
  (interactive "sDirectory where commands must be executed inside a chroot: \nsSchroot configuration name: ")
  (add-to-list 'schroot-mode-configs (cons dir config)))

(defun schroot-mode-project-dirs ()
  "Retrieves the list of project directories from the config alist."
  (mapcar 'car schroot-mode-configs))

(defun schroot-mode-dir-predicate-list ()
  "Helper function to generate a list that includes T for directores one is in and NIL for directories one is not in."
  (let ((location (or buffer-file-name default-directory)))
  (mapcar (lambda (dir) "Check if current DIR is in one of SCHROOT-MODE-PROJECT-DIRS"
	    (when (string-match (regexp-quote dir) location)
	      t))
	  (schroot-mode-project-dirs))))
  
(defun schroot-mode-dir-p ()
  "Helper function to determine whether one is in one of the SCHROOT-MODE-PROJECT-DIRS."
  ;; cl-remove-if-not #'identity shrinks the list to just `t`s, and car takes the front one, which is `t` if there is one and nil if there is not.
  (car (cl-remove-if-not #'identity (schroot-mode-dir-predicate-list))))

(defun schroot-mode-current-schroot-dir ()
  "A function that returns the schroot-mode project directory one is in."
  (alist-get 't (cl-mapcar #'cons (schroot-mode-dir-predicate-list) (schroot-mode-project-dirs))))

(defun schroot-mode-current-schroot-config ()
  "A function that returns the schroot configuration one has set for the project."
  (alist-get (schroot-mode-current-schroot-dir) schroot-mode-configs))

(define-minor-mode schroot-mode
  "A mode for working on projects inside a chroot using schroot."
  nil
  " schroot-mode"
  nil)



(defun schroot-mode-advised-make-process
    (orig-make-process &rest :name name :buffer buffer :command command :coding coding :noquery noquery :stop stop :connection-type connection-type :filter filter :sentinel sentinel :stderr stderr)
  "Advice for MAKE-PROCESS to use schroot with the appropriate config"
  ;; Because remove-advice was not working for my purpose, calling this with a name "-_schroot-mode_-" results in the original function being called
  (let ((schr-cfg (schroot-mode-current-schroot-config)))
    (if (schroot-mode-dir-p)
	(cond ((and (not (string-match "-_schroot-mode_-" (plist-get :name ':name)))
			 (not (and (string= (car (plist-get :name ':command)) "/bin/bash") (string= (cadr (plist-get :name ':command)) "-c"))))
	       (let ((original-command (plist-get :name ':command)))
		 (plist-put :name ':command (append (list (schroot-mode-pass-loc)) nil (list (schroot-mode-current-schroot-config)) nil original-command))
		 (apply orig-make-process :name)))
	      ((and (string= (car (plist-get :name ':command)) "/bin/bash") (string= (cadr (plist-get :name ':command)) "-c"))
	       (let ((original-command (cl-remove-if (lambda (s) "Match the `-c`s and `/bin/bash`s" (or (string= s "-c") (string= s "/bin/bash"))) (plist-get :name ':command))))
		 (plist-put :name ':command (append (list (schroot-mode-pass-bash-loc)) nil (list (schroot-mode-current-schroot-config)) nil original-command))
		 (apply orig-make-process :name))))
      (apply orig-make-process :name))))
  
(defun schroot-mode-strip-tag (string)
  "Helper function to strip the -_schroot-mode_- tag out of STRING"
  (substring string 0 (or (string-match "-_schroot-mode_-" string)
			  (string-match "-_schroot-mode-async-shell_-" string)
			  (string-match "-_schroot-mode-shell_-" string))))

(defun schroot-mode-advised-call-process
    (orig-call-process program &optional infile destination display &rest args)
  "Advice for CALL-PROCESS to use schroot with the appropriate config."
  ;; Use host ls in the case of dired, and use the "-_schroot-mode_-" trick to avoid double double dipping in schroot
  (when (not (cond ((and (string= program "ls") (car (cl-remove-if-not (lambda (s) (string= s "--dired")) args)))  
		    (apply orig-call-process program infile destination display args))
		   ((car (cl-remove-if-not (lambda (s) "Test for the SCHROOT-MODE tag" (string-match "-_schroot-mode_-" s)) args))
		    (let ((original-args (cl-remove-if (lambda (s) "Remove string if empty" (string= "" s)) (mapcar 'schroot-mode-strip-tag args))))
		      (apply orig-call-process program infile destination display original-args)))
		   ((and (string= program shell-file-name) (car (cl-remove-if-not (lambda (s) (string= s "-c")) args)) (schroot-mode-dir-p))
		    (let ((stripped-args (cl-remove-if (lambda (s) "Match the `-c`s" (string= "-c" s)) args)))
		      (let ((outgoing-args (append (list (schroot-mode-current-schroot-config)) stripped-args)))
			(apply orig-call-process (schroot-mode-pass-bash-loc) infile destination display outgoing-args))))
		   ((schroot-mode-dir-p)
		    (apply orig-call-process (schroot-mode-pass-loc) infile destination display (append (list (schroot-mode-current-schroot-config)) nil (list program) nil args nil)))))
    (apply orig-call-process program infile destination display args)))

(defun schroot-mode-advised-start-process (orig-start-process name buffer program &rest program-args)
  "Advice for START-PROCESS to use schroot with the appropriate config"
  (if (string-match "-_schroot-mode_-" name)
      (let ((schr-name (schroot-mode-strip-tag name)) (args (cl-remove-if (lambda (s) "Match the `-c`s" (string= "-c" s)) program-args)))
	(apply #'make-process :name name :buffer buffer (list :command (append (list (schroot-mode-pass-bash-loc)) nil args nil))))
    (let ((schr-name (concat name "-_schroot-mode_-")))
      (if (schroot-mode-dir-p)
	  ;; First, remove advice for make-process, which is called by start-process
	  (if program-args
	      (apply orig-start-process schr-name buffer (schroot-mode-pass-loc) (schroot-mode-current-schroot-config) program program-args)
	    (apply orig-start-process schr-name buffer (schroot-mode-pass-loc) (schroot-mode-current-schroot-config) program nil))
	(apply orig-start-process schr-name buffer program program-args)))))

(defun schroot-mode-build-shell-command (command)
  "Helper function to build a shell command for the schroot"
  ;; Oddly enough, this one doesn't double dip in schroot when calling call-process.
  ;; If that behavior changes (it should, as it should be using the advised call-process)
  ;; just concat "-_schroot-mode_-" to activate the shim.
  (concat (shell-quote-argument (schroot-mode-pass-loc)) " "
	  (shell-quote-argument (schroot-mode-current-schroot-config)) " "
	  (shell-quote-argument command)))

(defun schroot-mode-advised-shell-command (orig-shell-command command &optional output-buffer error-buffer)
  "Advice for SHELL-COMMAND to use schroot with the appropriate config."
  ;; The tag for directly acessing shell command is -_schroot-mode-shell_-
  (if (and (not (string-match "-_schroot-mode-shell_-" command))
	   (schroot-mode-dir-p))
      (apply orig-shell-command (schroot-mode-build-shell-command command) output-buffer error-buffer nil)
    (if (string-match "-_schroot-mode-shell_-" command)
	(let ((original-command (schroot-mode-strip-tag (schroot-mode-strip-tag command))))
	  (apply orig-shell-command original-command output-buffer error-buffer nil))
      (apply orig-shell-command command output-buffer error-buffer nil))))

;; I could not get async-shell-command to work. If anyone wants to give it a go, have at it.

;; This is where it stops getting transparent.
(defun schroot-mode-compile (command &optional comint)
  "A function designed to replace the functionality of `compile`.
Hopefully a temporary solution that will last until `compile`s bugginess in a chroot is figured out."
  (get-buffer-create "*schroot-compilation*")
  (interactive (list (compilation-read-command (eval compile-command))))
  (if comint
      (progn
	(make-process :name "compilation" :buffer (get-buffer "*schroot-compilation*") :command (list "/bin/bash" "-c" command))
	(shell "*schroot-compilation*"))
    (make-process :name "compilation" :buffer (get-buffer "*schroot-compilation*") :command (list "/bin/bash" "-c" command)))
  (pop-to-buffer (get-buffer "*schroot-compilation*")))

(defun schroot-mode-addvice ()
  "Adds the advice for make-process to use schroot."
  (interactive)
  (advice-add 'call-process :around #'schroot-mode-advised-call-process)
  (advice-add 'make-process :around #'schroot-mode-advised-make-process)
  (advice-add 'start-process :around #'schroot-mode-advised-start-process)
  (advice-add 'shell-command :around #'schroot-mode-advised-shell-command))

(defun schroot-mode-remove-advice ()
  "Removes the advice for make-process to use schroot."
  (interactive)
  (advice-remove 'call-process #'schroot-mode-advised-call-process)
  (advice-remove 'make-process #'schroot-mode-advised-make-process)
  (advice-remove 'start-process #'schroot-mode-advised-start-process)
  (advice-remove 'shell-command #'schroot-mode-advised-shell-command)
  (advice-remove 'compilation-start #'schroot-mode-advised-compilation-start)
  ;(advice-remove 'start-process-shell-command #'schroot-mode-advised-start-process-shell-command))
  )

(defun schroot-mode-togg-cond ()
  "Toggles schroot mode for SCHROOT-MODE-PROJECT-DIRS"
  ;; Term mode freezes in schroot-mode
  (if (and (schroot-mode-dir-p) (not (string= major-mode "term-mode")))
      (schroot-mode 1)
    (schroot-mode -1)
    (schroot-mode-remove-advice)))

(define-globalized-minor-mode schroot-mode-global-mode
  schroot-mode
  schroot-mode-togg-cond)

(add-hook 'schroot-mode-hook 'schroot-mode-addvice)

(provide 'schroot-mode)
;;; schroot-mode.el ends here
