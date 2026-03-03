;;; playground.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 William Boulanger
;;
;; Author: William Boulanger <willbou2@gmail.com>
;; Maintainer: William Boulanger <willbou2@gmail.com>
;; Created: January 23, 2025
;; Modified: January 23, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dash)
(require 's)

(require 'url)
(require 'json)
(require 'base64)

;;; --------------------------- ai-images settings ---------------------------
(defgroup ai-images nil
  "Generate images from various sources with a prompt."
  :group 'text
  :prefix "ai-images-")

(defcustom ai-images-default-api 'together
  "The default api to use"
  :type '(choice (const :tag "together.ai" together)
          (const :tag "test" test)))

(defcustom ai-images-default-width 800
  "The default width of the generated images"
  :type '(integer 1))

(defcustom ai-images-default-height 800
  "The default height of the generated images"
  :type '(integer 1))

(defcustom ai-images-average-width 200
  "The tiling algorithm will try to respect this value."
  :type '(integer 1))

;;; --------------------- ai-images-together settings ---------------------
(defgroup ai-images-together nil
  "Generate images using together.ai with a prompt."
  :group 'ai-images
  :prefix "ai-images-together-")

(defcustom ai-images-together-api-key nil
  "API key used with together.ai"
  :type 'string)
(defun ai-images-together--api-key ()
  (if ai-images-together-api-key ai-images-together-api-key
    (error "Please set an API key for together.ai")))

(defcustom ai-images-together-default-model "black-forest-labs/FLUX.1-dev"
  "Model used by together.ai (Please refer to Together's documentation)")

(defcustom ai-images-together-default-steps 28
  "Number of steps used in inference")

(cl-defun ai-images-together--generate-batch (number prompt width height &key model steps)
  (let* ((json-data `(("model" . ,(or model ai-images-together-default-model))
                      ("prompt" . ,prompt)
                      ("width" . ,width)
                      ("height" . ,height)
                      ("steps" . ,(or steps ai-images-together-default-steps))
                      ("n" . ,number)
                      ("response_format" . "b64_json")))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (ai-images-together--api-key)))
            ("Content-Type" . "application/json")))
         (url-request-data (json-encode json-data))
         (response (url-retrieve-synchronously "https://api.together.xyz/v1/images/generations")))
    (with-current-buffer response
      (goto-char url-http-end-of-headers)
      (let* ((json-response (json-read))
             (data (cdr (assoc 'data json-response))))
        (--map (base64-decode-string (cdr (assoc 'b64_json it))) data)))))

(defun ai-images-together--generate (number &rest args)
  (let* ((number-full-batches (truncate (/ number 4)))
         (number-remaining-images (% number 4))
         (images '()))
    (dotimes (i number-full-batches)
      (push (apply #'ai-images-together--generate-batch 4 args) images))
    (push (apply #'ai-images-together--generate-batch number-remaining-images args) images)
    (-flatten images)))

(defun ai-images-test--generate (number width height)
  (let (images '())
    (dotimes (i number)
      (let* ((red (random 256))
             (green (random 256))
             (blue (random 256))
             (pixels-length (* width height 3))
             (header (format "P6\n%d %d\n255\n" width height))
             (pixels (make-string pixels-length 0)))
        (cl-loop for p from 0 to (- pixels-length 1) by 3 do
                 (aset pixels p red)
                 (aset pixels (+ p 1) green)
                 (aset pixels (+ p 2) blue))
        (push (concat header pixels) images)))
    images))

(defun ai-images-generate (number)
  (interactive "p")
  (let* ((number (or number 1))
         (buffer-char-width (-(window-width) 20))
         (buffer-width (string-pixel-width (make-string buffer-char-width ? )))
         (wanted-images-per-row (truncate (/ buffer-width ai-images-average-width)))
         (max-width (truncate (/ buffer-width wanted-images-per-row)))
         (real-width (min max-width ai-images-default-width))
         (images-per-row (truncate (/ buffer-width real-width)))
         (raw-images (ai-images-together--generate-batch
                      number "kitchen knives" ai-images-default-width ai-images-default-height))
         (emacs-images (--map (create-image it nil t :max-width max-width) raw-images)))
    (save-excursion
      (cl-loop for ei in emacs-images
               for i from 0 do
               (when (= (% i images-per-row) 0)
                 (end-of-line)
                 (newline))
               (insert-image ei)))))

(provide 'ai-images)

;;; playground.el ends here
