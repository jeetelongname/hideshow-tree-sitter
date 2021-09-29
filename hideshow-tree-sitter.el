;;; hideshow-tree-sitter.el --- add tree sitter based code folding  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeetaditya Chatterjee

;; Author: Jeetaditya Chatterjee <jeetelongname@gmail.com>
;; Keywords: languages, faces, tree-sitter
;;; Commentary:
;;
;;; Code:
(require 'tree-sitter)
(require 'hideshow)
(require 'seq)
(require 'cl-lib)

(defgroup hideshow-tree-sitter nil
  "Tree sitter based code folding leveraging hideshow."
  :group 'tree-sitter
  :prefix "hideshow-tree-sitter-")

(defconst hideshow-tree-sitter-language-alist
  '((ruby-mode . "ruby"))
  "All of the langauges mode supports.") ;; TODO: Fix doc string

(defconst hideshow-tree-sitter--query-dir
  (let ((lib-dir (file-name-directory (locate-library "hideshow-tree-sitter.el"))))
    (file-name-as-directory (concat lib-dir "queries"))))

(defun hideshow-tree-sitter--get-nodes ()
  "Get all the folable nodes in a buffer."
  (let* ((tree (tsc-root-node tree-sitter-tree))
         (lang (cdr (assoc major-mode hideshow-tree-sitter-language-alist)))
         (query-string (with-temp-buffer
                         (insert-file-contents
                          (concat hideshow-tree-sitter--query-dir
                                  lang "/folds.scm"))
                         (buffer-string)))
         (query-object (tsc-make-query tree-sitter-language query-string))
         (captures (tsc-query-captures query-object tree #'tsc--buffer-substring-no-properties))
         (nodes (seq-map #'cdr captures)))
    nodes))

(defun hideshow-tree-sitter--denestify-captures (ranges location)
  "Takes a list of RANGES and a LOCATION will return the range that contain the current point."

  (cl-labels ((inbetween (range)
                         (< (car range) location (cdr range)))
              (go (range loc urange)
                  (cond ((= (length range) 1) range)
                        ((null range)
                         (if (inbetween (car urange))
                             urange
                           nil))
                        (t (go
                            (seq-filter #'inbetween (cdr range))
                            loc urange)))))
    (go ranges location ranges)))



(defun hideshow-tree-sitter--get-range ()
  "Get the range of the node under point."
  (let* ((loc (point))
         (byte-loc (position-bytes loc))
         (nodes (hideshow-tree-sitter--get-nodes))
         (ranges-bytepos (seq-map #'tsc-node-byte-range nodes))
         (range (hideshow-tree-sitter--denestify-captures ranges-bytepos byte-loc)))
    (car range)))

(defun hideshow-tree-sitter-hide-block ()
  "Hide a block.")

(defun hideshow-tree-sitter-show-block ()
  "Unhide (show) a block.")

(defun hideshow-tree-sitter-hide-all ()
  "Hide all blocks.")

(defun hideshow-tree-sitter-show-all ()
  "Unhide (show) all blocks.")

(defun hideshow-tree-sitter-toggle-hiding (&optional e)
  "Toggle hiding/showing of a block.
See `hideshow-tree-sitter-hide-block' and `hideshow-tree-sitter-show-block'.
Argument E should be the event that triggered this action."
  (interactive)
  (hs-life-goes-on
   (posn-set-point (event-end e))
   (if (hs-already-hidden-p)
       (hideshow-tree-sitter-show-block)
     (hideshow-tree-sitter-hide-block))))

(provide 'hideshow-tree-sitter)
;;; hideshow-tree-sitter.el ends here
