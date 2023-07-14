;;; notebook.el --- Notebook mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/notebook-mode
;; Version: 0.3
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
;; A minor mode to populate an org document with various SVG tags and
;; buttons.
;;
;;
;;; News:
;;
;; Version 0.3
;; Use of svg-tag-mode to implement buttons
;;
;; Version 0.2
;; Moved buttons inside documents
;;
;; Version 0.1
;; First proof of concept with buttons in margin
;;
;;; Code:
(require 'org)
(require 'svg-tag-mode)

(setq svg-tag-tags
      '(
        ;; Inline code
        ;; --------------------------------------------------------------------
        ("^#\\+call:" .     ((lambda (tag) (svg-tag-make "CALL"
                                           :face 'org-meta-line))
                             (lambda () (interactive) (notebook-call-at-point)) "Call function"))
        ("call_" .         ((lambda (tag) (svg-tag-make "CALL"
                                          :face 'default
                                          :margin 1
                                          :alignment 0))
                            (lambda () (interactive) (notebook-call-at-point)) "Call function"))
        ("src_" .          ((lambda (tag) (svg-tag-make "CALL"
                                          :face 'default
                                          :margin 1
                                          :alignment 0))
                             (lambda () (interactive) (notebook-call-at-point)) "Execute code"))

        ;; Code blocks
        ;; --------------------------------------------------------------------
        ("^#\\+begin_src\\( [a-zA-Z\-]+\\)" .  ((lambda (tag)
                                                  (svg-tag-make (upcase tag)
                                                                :face 'org-meta-line
                                                                :crop-left t))))
        ("^#\\+begin_src" . ((lambda (tag) (svg-tag-make "RUN"
                                           :face 'org-meta-line
                                           :inverse t
                                           :crop-right t))
                             (lambda () (interactive) (notebook-run-at-point)) "Run code block"))
        ("^#\\+end_src" .    ((lambda (tag) (svg-tag-make "END"
                                            :face 'org-meta-line))))

        
        ;; Export blocks
        ;; --------------------------------------------------------------------
        ("^#\\+begin_export" . ((lambda (tag) (svg-tag-make "EXPORT"
                                              :face 'org-meta-line
                                              :inverse t
                                              :alignment 0
                                              :crop-right t))))
        ("^#\\+begin_export\\( [a-zA-Z\-]+\\)" .  ((lambda (tag)
                                                     (svg-tag-make (upcase tag)
                                                                   :face 'org-meta-line
                                                                   :crop-left t))))
        ("^#\\+end_export" . ((lambda (tag) (svg-tag-make "END"
                                            :face 'org-meta-line))))

        ;; :noexport: tag
        ;; --------------------------------------------------------------------
        ("\\(:no\\)export:" .    ((lambda (tag) (svg-tag-make "NO"
                                                :face 'org-meta-line
                                                :inverse t
                                                :crop-right t))))
        (":no\\(export:\\)" .    ((lambda (tag) (svg-tag-make "EXPORT"
                                                :face 'org-meta-line
                                                :crop-left t))))

        ;; Miscellaneous keywords
        ;; --------------------------------------------------------------------
        ("|RUN|" .          ((lambda (tag) (svg-tag-make "RUN"
                                           :face 'org-meta-line
                                           :inverse t))))
        ("|RUN ALL|" .       ((lambda (tag) (svg-tag-make "RUN ALL"
                                            :face 'org-meta-line))
                             (lambda () (interactive) (notebook-run)) "Run all notebook code blocks"))
        ("|SETUP|" .         ((lambda (tag) (svg-tag-make "SETUP"
                                            :face 'org-meta-line))
                             (lambda () (interactive) (notebook-setup)) "Setup notebook environment"))
        ("|EXPORT|" .        ((lambda (tag) (svg-tag-make "EXPORT"
                                            :face 'org-meta-line))
                             (lambda () (interactive) (notebook-export-html)) "Export the notebook to HTML"))
        ("|CALL|" .          ((lambda (tag) (svg-tag-make "CALL"
                                            :face 'org-meta-line))))

        
        ;; References
        ;; --------------------------------------------------------------------
        ("\\(\\[cite:@[A-Za-z]+:\\)" .
         ((lambda (tag) (svg-tag-make (upcase tag)
                                      :face 'nano-default
                                      :inverse t
                                      :beg 7 :end -1
                                      :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9a-z]+\\]\\)" .
         ((lambda (tag) (svg-tag-make (upcase tag)
                                      :face 'nano-default
                                      :end -1
                                      :crop-left t))))

        ;; Miscellaneous properties
        ;; --------------------------------------------------------------------
        ("^#\\+caption:" .   ((lambda (tag) (svg-tag-make "CAPTION"
                                            :face 'org-meta-line))))
        ("^#\\+latex:" .     ((lambda (tag) (svg-tag-make "LATEX"
                                            :face 'org-meta-line))))
        ("^#\\+html:" .      ((lambda (tag) (svg-tag-make "HTML"
                                            :face 'org-meta-line))))
        ("^#\\+name:" .      ((lambda (tag) (svg-tag-make "NAME"
                                            :face 'org-meta-line))))
        ("^#\\+header:" .    ((lambda (tag) (svg-tag-make "HEADER"
                                            :face 'org-meta-line))))
        ("^#\\+label:" .     ((lambda (tag) (svg-tag-make "LABEL"
                                            :face 'org-meta-line))))
        ("^#\\+results:"  .  ((lambda (tag) (svg-tag-make "RESULTS"
                                            :face 'org-meta-line))))))


(defun notebook-run-at-point ()
  "Update notebook rendering at point."
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-redisplay-inline-images))

(defun notebook-call-at-point ()
  (interactive)
  (org-ctrl-c-ctrl-c))

(defun notebook-setup ()
  "Notebook mode setup function."
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
  "Activate notebook mode."

  (add-to-list 'font-lock-extra-managed-props 'display)
  (setq font-lock-keywords-case-fold-search t)
  (setq org-image-actual-width `( ,(truncate (* (frame-pixel-width) 0.85))))
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-with-inline-images t)
  (org-redisplay-inline-images)
  (org-indent-mode)
  (org-hide-block-all)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (svg-tag-mode 1))

(defun notebook-mode-off ()
  "Deactivate notebook mode."

  (svg-tag-mode -1)
  (remove-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

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
