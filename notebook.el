;;; notebook.el --- Notebook mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/notebook-mode
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org-mode, babel, notebook

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;;
;;
;;; News:
;;
;; Version 0.2
;; Moved buttons inside documents
;;
;; Version 0.1
;; First proof of concept with buttons in margin
;;
;;; Code 
(require 'org)
(require 'svg-lib)

(defvar notebook--active-tags nil)

(defun notebook--build-keywords (item)
  "Internal.  Build the list of keyword from ITEM."
  (let ((pattern  (format "\\(%s\\)" (car item)))
        (tag      (nth 0 (cdr item)))
        (callback (nth 1 (cdr item)))
        (help     (nth 2 (cdr item))))
    (when (and (symbolp tag) (fboundp tag))
      (setq tag `(,tag (match-string 0))))
    (setq tag ``(face nil
                 display ,,tag
                 ,@(if ,callback '(pointer hand))
                 ,@(if ,help `(help-echo ,,help))
                 ,@(if ,callback `(keymap (keymap (mouse-1  . ,,callback))))))
    `(,pattern 1 ,tag)))

(defun notebook-tag (tag face &optional inverse margin)
  (let* ((margin (or margin 0))
         (alignment (if margin 0.0 0.5)))
    (if inverse
        (svg-lib-tag tag nil
                     :padding 1 :margin margin :stroke 0  :radius 3
                     :font-weight 'semibold :alignment alignment
                     :foreground  (face-background face nil 'default)
                     :background  (face-foreground face nil 'default))
      (svg-lib-tag tag nil
                   :padding 1 :margin margin :stroke 2 :radius 3
                   :font-weight 'regular  :alignment alignment
                   :foreground  (face-foreground face nil 'default)
                   :background  (face-background face nil 'default)))))

(setq notebook-tags
      '(("^#\\+call:" .     ((notebook-tag "CALL" 'org-tag)
                             'notebook-call-at-point "Call function"))
        ("call_" .         ((notebook-tag "CALL" 'default nil 1)
                             'notebook-call-at-point "Call function"))
        ("^#\\+begin_src" . ((notebook-tag "RUN" 'org-tag t)
                             'notebook-run-at-point "Run code block"))
        ("|RUN|" .          ((notebook-tag "RUN" 'org-tag t)))
        ("|RUN ALL|" .      ((notebook-tag "RUN ALL" 'org-meta-line)
                             'notebook-run "Run all notebook code blocks"))
        ("|SETUP|" .        ((notebook-tag "SETUP" 'org-meta-line)
                             'notebook-setup "Setup notebook environment"))
        ("|EXPORT|" .       ((notebook-tag "EXPORT" 'org-meta-line)
                             'notebook-export-html "Export the notebook to HTML"))
        ("|CALL|" .         ((notebook-tag "CALL" 'org-meta-line)))
        ("|CALL|" .         ((notebook-tag "CALL" 'org-meta-line)))
        
        ("^#\\+end_src" .   ((notebook-tag "END" 'org-tag)))
        ("^#\\+caption:" .  ((notebook-tag "CAPTION" 'org-meta-line)))
        ("^#\\+name:" .     ((notebook-tag "NAME" 'org-meta-line)))
        ("^#\\+header:" .   ((notebook-tag "HEADER" 'org-meta-line)))
        ("^#\\+label:" .    ((notebook-tag "LABEL" 'org-meta-line)))
        ("^#\\+results:"  . ((notebook-tag "RESULTS" 'org-meta-line)))))


(defun notebook--remove-text-properties (oldfun start end props  &rest args)
  "This apply remove-text-properties with 'display removed from props"
  (apply oldfun start end (org-plist-delete props 'display) args))

(defun notebook--remove-text-properties-on (args)
  "This installs an advice around remove-text-properties"
  (advice-add 'remove-text-properties
              :around #'notebook--remove-text-properties))

(defun notebook--remove-text-properties-off (args)
  "This removes the advice around remove-text-properties"
  (advice-remove 'remove-text-properties
                 #'notebook--remove-text-properties))

(defun notebook-run-at-point ()
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-redisplay-inline-images))

(defun notebook-call-at-point ()
  (interactive)
  (org-ctrl-c-ctrl-c))

(defun notebook-setup ()
  (interactive)
  (setq org-cite-csl-styles-dir ".")
  (setq org-babel-python-command "/opt/anaconda3/bin/python")
  (require 'ob-python)
  (require 'oc-csl))

(defun notebook-run ()
  (interactive)
  (org-babel-execute-buffer))

(defun notebook-export-html ()
  (interactive)
  (org-html-export-to-html))

(defun notebook-mode-on ()
  "Activate SVG tag mode."

  (add-to-list 'font-lock-extra-managed-props 'display)
  (setq font-lock-keywords-case-fold-search t)
  (setq org-image-actual-width `( ,(truncate (* (frame-pixel-width) 0.85))))
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (org-redisplay-inline-images)
  (org-indent-mode)
  (org-hide-block-all)

  ;; Remove any active tags
  (when notebook--active-tags
    (font-lock-remove-keywords nil
          (mapcar #'notebook--build-keywords notebook--active-tags)))

  ;; Install keyword tags
  (when notebook-tags
    (font-lock-add-keywords nil
                            (mapcar #'notebook--build-keywords notebook-tags)))
  (setq notebook--active-tags (copy-sequence notebook-tags))

  ;; Install advices on remove-text-properties (before & after). This
  ;; is a hack to prevent org mode from removing SVG tags that use the
  ;; 'display property
  (advice-add 'org-fontify-meta-lines-and-blocks
            :before #'notebook--remove-text-properties-on)
  (advice-add 'org-fontify-meta-lines-and-blocks
              :after #'notebook--remove-text-properties-off)

  ;; Redisplay everything to show tags
  (font-lock-flush))

(defun notebook-mode-off ()
  "Deactivate SVG tag mode."

  (when notebook--active-tags
    (font-lock-remove-keywords nil
               (mapcar #'notebook--build-keywords notebook--active-tags)))
  (setq notebook--active-tags nil)

  ;; Remove advices on remove-text-properties (before & after)
  (advice-remove 'org-fontify-meta-lines-and-blocks
                 #'notebook--remove-text-properties-on)
  (advice-remove 'org-fontify-meta-lines-and-blocks
                 #'notebook--remove-text-properties-off)

  ;; Redisplay everything to hide tags
  (font-lock-flush))


(define-minor-mode notebook-mode
  "Minor mode for graphical tag as rounded box."
  :group 'notebook
  (if notebook-mode
      (notebook-mode-on)
    (notebook-mode-off)))

(define-globalized-minor-mode
   global-notebook-mode notebook-mode notebook-mode-on)

(provide 'notebook)
;;; notebook.el ends here
