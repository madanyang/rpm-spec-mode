;;; 
;;; osc-spec-mode.el --- RPM spec file editing commands for Emacs
;;;  Copyright (C) 2012 	   Togan Muftuoglu <toganm@opensuse.org>

;;; Commentary:

;; This mode is used for editing spec files used for building RPM packages.
;; Put this in your .emacs file to enable autoloading of rpm-spec-mode,
;; and auto-recognition of ".spec" files:
;;
;;  (autoload 'osc-spec-mode "osc-spec-mode.el" "RPM spec mode." t)
;;  (setq auto-mode-alist (append '(("\\.spec" . osc-spec-mode))
;;                                auto-mode-alist))
;;------------------------------------------------------------
;;

;;; Code:

(require 'cl)

(defconst osc-spec-mode-version "0.0.1" "Version of `osc-spec-mode'.")

(defun osc-br-split ()
  "Split a BuildRequires: line like this \"BuildRequires:  foo bar baz\" into
BuildRequires:  foo
BuildRequires:  bar
BuildRequires:  baz"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (string-equal "BuildRequires" (word-at-point))
        (progn (beginning-of-line)
               (forward-word)
               (forward-word)
               (backward-char)
               (let ((here (point-marker)))
                 (progn (end-of-line)
                        (let ((there (point-marker)))
                          (progn (goto-char here)
                                 (let ((from " ")
                                       (to "\nBuildRequires:  "))
                                   (replace-string from to nil here there)))))))
      (message "You can only run osc-br-split on a line that starts with BuildRequires:"))))

(defun mark-line ()
  (interactive)
  (beginning-of-line)
  (set-mark (point-marker))
  (end-of-line))

(defun mark-to-end ()
  (interactive)
  (set-mark (point-marker))
  (end-of-line))

(defun osc-pwd ()
  "Returns the current working directory, sans parent directories similar to pwd"
  (file-name-nondirectory (directory-file-name (expand-file-name default-directory))))



(defun osc-view-readonly (filename)
  "view a file readonly"
  (let* ((buffer-name (format "*%s*" filename)))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name))
    (let* ((output-buffer (get-buffer-create buffer-name))
           (outwin (display-buffer output-buffer nil t)))
      (save-selected-window
        (select-window outwin)
        (insert-file-contents filename)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun osc-view-changelog ()
  "something like \"less foo.changes\""  
  (interactive)

  (let ((filename (format "%s.changes" (osc-pwd))))
    (osc-view-readonly filename)))


(defun osc-view-spec ()
  "Open the .spec file in the current working directory in read-only mode."
  (interactive)

  ;;"something like \"less foo.spec\""
  (let ((filename (format "%s.spec" (osc-pwd))))
    (osc-view-readonly filename)))


(defun osc-ldconfig ()
  "Replace `%run_ldconfig', which is deprecated, with direct calls to `/sbin/ldconfig'."
  (interactive)          
  (let ((old-pnt (point-marker)))
    (beginning-of-buffer)
    (replace-string "%run_ldconfig" "/sbin/ldconfig")
    (goto-char old-pnt)))


(defun osc-lang-files ()
  "Insert typical lang subpackage text."
  (interactive)
  (insert (format "%%files lang -f %%{name}.lang\n")))

(defun osc-fdupes ()
  "Insert typical %fdupes text."
  (interactive)
  (insert (format "%%fdupes %buildroot\n")))


(defun ab-open-rej ()
  "When a patch fails to apply, it will leave behind files with
the suffix `.rej'. To use this command effectively, open the file
that was not successfully patched, and then run this command,
which will split the window and open the `.rej' file in one
window, and the unpatched or partially unpatched file in the
other."
  (interactive)
  (let* ((this-buffer (buffer-file-name))
         (rej-buffer (concat this-buffer ".rej")))
    (delete-other-windows)
    (split-window)
    (find-file rej-buffer)
    (revert-buffer t t)))

(defun osc-about-osc-spec-mode (&optional arg)
  "About `osc-spec-mode'."
  (interactive "p")
  (message
   (concat "osc-spec-mode version "
           osc-spec-mode-version
           " by Togan Muftuoglu <toganm@opensuse.org> version")))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.spec\\(\\.in\\)?$" . osc-spec-mode))

(provide 'osc-spec-mode)
