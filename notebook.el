;;; notebook.el --- Notebook mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/notebook-mode
;; Version: 0.1
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

;;; Code:
(require 'org)
(require 'svg-lib)


(defvar notebook--tags
  `(:run   ,(svg-lib-tag "RUN" nil
                         :padding 1 :margin 0 :stroke 2
                         :radius 0 :font-weight 'semibold
                         :foreground  (face-background 'default)
                         :background  (face-foreground 'default))
    :clear ,(svg-lib-tag "CLEAR" nil
                         :padding 1 :margin 0 :stroke 2
                         :radius 0 :font-weight 'semibold
                         :foreground  (face-background 'default)
                         :background  (face-foreground 'default))
    :export ,(svg-lib-tag "EXPORT" nil
                         :padding 1 :margin 0 :stroke 2
                         :radius 0 :font-weight 'semibold
                         :foreground  (face-background 'default)
                         :background  (face-foreground 'default))
    :run-0 ,(svg-lib-tag "RUN" nil
                        :padding 1 :margin 2 :stroke 2
                        :radius 0 :font-weight 'normal
                        :foreground  (face-foreground 'font-lock-comment-face nil t)
                        :background  (face-background 'font-lock-comment-face nil 'default))
    :run-1 ,(svg-lib-tag "RUN" nil
                        :padding 1 :margin 2 :stroke 0
                        :radius 0 :font-weight 'normal
                        :foreground  (face-background 'warning nil 'default)
                        :background  (face-foreground 'warning nil 'default))
    :run-2 ,(svg-lib-tag "RUN" nil
                        :padding 1 :margin 2 :stroke 0
                        :radius 0 :font-weight 'semibold
                        :foreground  (face-background 'default)
                        :background  (face-foreground 'default))
    :out-0 ,(svg-lib-tag "???" nil
                        :padding 1 :margin 2 :stroke 2
                        :radius 0 :font-weight 'normal
                        :foreground  (face-foreground 'font-lock-comment-face nil t)
                        :background  (face-background 'font-lock-comment-face nil 'default))
    :out-1 ,(svg-lib-tag "???" nil
                        :padding 1 :margin 2 :stroke 2
                        :radius 0 :font-weight 'normal
                        :foreground  (face-foreground 'font-lock-comment-face nil t)
                        :background  (face-background 'font-lock-comment-face nil 'default))
    :out-2 ,(svg-lib-tag "OUT" nil
                        :padding 1 :margin 2 :stroke 2
                        :radius 0 :font-weight 'normal
                        :foreground  (face-foreground 'default)
                        :background  (face-background 'default))))


(defun notebook--regular-tag (tag function help)
  "Return a string displaying svg TAG"
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] function)
    (propertize " " 'local-map map 
                    'help-echo help
                    'display (plist-get notebook--tags tag))))

(defun notebook--margin-tag (tag)
  "Return a string displaying svg TAG to be inserted in the left margin"
  (propertize " " 'display `(((margin left-margin)
                              ,(plist-get notebook--tags tag)))))

(defun notebook--tag-block (&optional tag)
  "Add TAG to block at point"  
  (let* ((element (org-element-at-point))
         (beg (org-element-property :begin element))
         (overlay (progn (remove-overlays beg (+ beg 1))
                         (make-overlay beg (+ beg )))))
    (when tag
        (overlay-put overlay 'notebook-tag t)
        (overlay-put overlay 'before-string (notebook--margin-tag tag)))))

(defun notebook--tag-src-block (&optional tag)
  (notebook--tag-block tag))

(defun notebook--tag-src-blocks (&optional tag)
  "Tag all source blocks with TAG"
  (org-babel-map-executables nil
    (notebook--tag-src-block tag)))

(defun notebook--tag-out-block (&optional tag)
  "Add TAG to block at point"  
  (let* ((location (org-babel-where-is-src-block-result))
   	     (case-fold-search t))
      (when location
        (save-excursion
          (goto-char location)
          (notebook--tag-block tag)))))

(defun notebook--tag-out-blocks (&optional tag)
  "Tag all result blocks with TAG"
  (org-babel-map-executables nil
    (notebook--tag-out-block tag)))



(defun notebook--decorate-headers ()
  ""
  (let* ((face `(:inherit region :extend t))
         (p1 (progn (forward-line -1) (point)))
         (p2 (progn (forward-line +1) (point)))
         (p3 (progn (forward-line +1) (point)))
         (p4 (progn (forward-line +1) (point)))
         (height (truncate (* (face-attribute 'default :height) .5)))
         (region-overlay  (make-overlay p1 p4)))

    (overlay-put (make-overlay p1 p2)
                 'face `(:extend t :height ,height))
    (overlay-put (make-overlay p3 p4)
                 'face `(:extend t :height ,height))
    (overlay-put region-overlay 'face face)
    (overlay-put region-overlay 'line-prefix
     (concat 
      (propertize " " 
      'display `((margin right-margin)
                ,(propertize "      " 'face face)))
      (propertize " " 
           'display `((margin left-margin)
             ,(propertize "     " 'face face)))))

    (overlay-put region-overlay 'wrap-prefix
     (concat 
      (propertize " " 
      'display `((margin right-margin)
                ,(propertize " " 'face face)))
      (propertize " " 
           'display `((margin left-margin)
             ,(propertize " " 'face face)))))))


(defun notebook--execute-src-block-before (&optional arg info params)
  (let* ((org-babel-current-src-block-location
	      (or org-babel-current-src-block-location
	          (nth 5 info)
	          (org-babel-where-is-src-block-head))))
    (save-excursion
      (goto-char org-babel-current-src-block-location)
      (notebook--tag-src-block :run-1)
      (notebook--tag-out-block :out-1)))
  (redisplay t))

(defun notebook--execute-src-block-after (&optional arg info params)
  (let* ((org-babel-current-src-block-location
	      (or org-babel-current-src-block-location
	          (nth 5 info)
	          (org-babel-where-is-src-block-head))))
    (save-excursion
      (goto-char org-babel-current-src-block-location)
      (notebook--tag-src-block :run-2)
      (notebook--tag-out-block :out-2)
      (org-redisplay-inline-images)))
  (redisplay t))

(defun notebook--remove-result-before (&optional info keep-keyword)
  (notebook--tag-src-block :run-0)
  (let ((location (org-babel-where-is-src-block-result nil info))
	    (case-fold-search t))
    (when location
      (save-excursion
        (goto-char location)
        (notebook--tag-block)))))


(defun notebook--activate ()

  ;; Document layout
  (setq org-startup-with-inline-images t)
  (org-mode)
  (org-indent-mode)
  (org-hide-block-all)
  (set-frame-size nil 86 48)
  (set-window-margins nil 5 5)
  (setq line-spacing 0)
  (setq org-image-actual-width `( ,(truncate (* (frame-pixel-width) 0.75))))
  (set-frame-parameter (selected-frame) 'internal-border-width 1)
  (set-face-attribute 'internal-border (selected-frame)
                      :background (face-foreground 'default))
  (set-face-attribute 'internal-border t
                      :background (face-background 'default))
  (face-remap-add-relative 'header-line
     `(:foreground ,(face-background 'default)
       :background ,(face-foreground 'default)))
  (face-remap-add-relative 'header-line 
       :box `(:line-width 8
              :color ,(face-foreground 'default)
              :style nil))
  
  ;; Header and mode line
  (setq mode-line-format nil)
  (setq header-line-format
        (concat " " (propertize "GNU Emacs"
                                'face `(:inherit 'bold
                                        :foreground ,(face-background 'default)))
                " " (propertize "—" 'face 'font-lock-comment-face)
                " " (propertize "Notebook" 
                                'face `(:foreground ,(face-background 'default)))
                (propertize " " 'display `(space :align-to (- right 8)))
                    (notebook--regular-tag :run
                                           'notebook-run-all
                                           "Run all source cells")
                " " (notebook--regular-tag :clear
                                           'notebook-clear-all
                                           "Clear all result cells")))

  ;; Top level header decorations
  (org-map-entries 'notebook--decorate-headers "LEVEL=1")

  ;; Tag sources blocks
  (notebook--tag-src-blocks :run-0)

  ;; Tag result blocks
  (notebook--tag-out-blocks :out-0)

  ;; Install margin click handler
  (local-set-key [left-margin mouse-1] #'notebook--margin-click)

  ;; Install babel advices
  (advice-add 'org-babel-execute-src-block :before
              #'notebook--execute-src-block-before)
  (advice-add 'org-babel-execute-src-block :after
              #'notebook--execute-src-block-after)
  (advice-add 'org-babel-remove-result :before
              #'notebook--remove-result-before))
  

(defun notebook--deactivate ()
  ;; Remove babel advices
  (advice-remove 'org-babel-execute-src-block 
                 #'notebook--execute-src-block-before)
  (advice-remove 'org-babel-execute-src-block
                 #'notebook--execute-src-block-after)
  (advice-remove 'org-babel-remove-result
                 #'notebook--remove-result-before))

(defun notebook--margin-click (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (forward-line)
  (org-ctrl-c-ctrl-c))

(defun notebook-run-all ()
  (interactive)
  (org-babel-execute-buffer))

(defun notebook-clear-all ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'notebook-tag t)
  (org-babel-map-src-blocks nil (org-babel-remove-result)))

(defun notebook-clear-below-point ()
  (interactive)
  (org-babel-map-src-blocks nil (org-babel-remove-result)))

(define-minor-mode notebook-mode
  "Toggle notebook mode"
  :group 'org-mode
  :global t
  :init-value nil

  (if notebook-mode
      (notebook--activate)
    (notebook--deactivate)))
