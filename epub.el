;;; -*- lexical-binding: t -*-
(require 'shr)
(require 'arc-mode)

(defun epub-mode ()
  (interactive)
  (let* ((archive buffer-file-name)
         (name (file-name-sans-extension (file-name-nondirectory archive)))
         (toc (format "Table of Contents (%s)" name)))
    (unless epub--debug
      (kill-buffer))
    (epub--show-toc archive toc)))

(defvar epub--debug nil)

(defconst epub-render-buffer "*epub*"
  "Buffer to display rendered pages.")

(defconst epub-toc-buffer "*epub-toc*"
  "Default buffer to display table of contents.")

(defvar epub--archive-cache nil
  "Contains xml and page content cache as alist in format
((ARCHIVE-PATH . ((CONTENT-PATH . VALUE)
                  ...))
 ...)")

(defsubst cadr-safe (x)
  "Return the safe car of the cdr of X."
  (car-safe (cdr-safe x)))

(defsubst cddr-safe (x)
  "Return the safe cdr of the cdr of X."
  (cdr-safe (cdr-safe x)))

(defsubst caddr-safe (x)
  "Return the safe car of the cdr of the cdr of X."
  (car-safe (cdr-safe (cdr-safe x))))

(defun epub--show-toc (archive &optional buffer)
  "Assuming ARCHIVE is a valid epub file, switch to BUFFER and render table of contents there."
  (let* ((rootfile (epub--locate-rootfile archive))
	 (dom (epub--archive-get-xml archive rootfile))
	 (manifest-idx (epub--manifest-idx dom))
	 (spine-toc-idx (epub--spine-toc-idx dom manifest-idx))
         (ncx-file (epub--locate-ncx dom rootfile))
         (ncx-path (file-name-directory (or ncx-file "")))
         (ncx (epub--archive-get-xml archive ncx-file))
         (title (caddr-safe (epub--xml-node ncx 'docTitle 'text)))
         (navmap (epub--xml-node ncx 'navMap))
         (buf (get-buffer-create (or buffer epub-toc-buffer)))
         start)
    (pop-to-buffer-same-window buf)
    (indented-text-mode)
    (erase-buffer)
    (insert title "\n\nTable of contents\n\n")
    (epub--insert-navmap navmap archive ncx-path spine-toc-idx)
    (insert "\n\n")
    (if epub--debug (epub--insert-xml archive ncx-file))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))


(defun epub--spine-toc-idx (dom manifest-idx)
  "Create table-of-contents hashtable, with key as page href, 
and value as pair (previous-page-href . next-page-href)"
  (when epub--debug
    (message "epub.el: Building href navigation index"))
  (let* ((spine-root (epub--xml-node dom 'spine))
	 (toc-id (epub--xml-prop spine-root 'toc))
	 (spine-items (cddr-safe spine-root))
	 (hashtable (make-hash-table :test #'equal)))
    (cl-loop for items = spine-items then (cdr items)
	     for prev-item = nil then item-id
	     for item = (car items)
	     for item-id = (epub--xml-prop item 'idref)
	     until (null items) 
	     if (not (string= item-id toc-id)) ;; skip toc
	     do (puthash (gethash item-id manifest-idx)                                            ;; this href
			 (cons (gethash prev-item manifest-idx)                                    ;; prev href
			       (gethash (epub--xml-prop (cadr-safe items) 'idref) manifest-idx))   ;; next href
			 hashtable))
    hashtable))

(defun epub--manifest-idx (dom)
  "Create manifest hash table with key being id, value being href"
  ;;(message "epub.el: Building manifest index")
  (let ((manifest (cddr-safe (epub--xml-node dom 'manifest)))
	(new-idx (make-hash-table :test #'equal)))
    (loop for item in (cdr manifest)
	  do (puthash (epub--xml-prop item 'id) (epub--xml-prop item 'href) new-idx))
    new-idx))

  
(defun epub--insert-xml (archive name &optional no-pretty-print)
  (let ((start (point-marker)))
    (when epub--debug
      (message "epub.el: Extracting %S, looking for %S" archive name))
    (archive-zip-extract archive name)
    (decode-coding-inserted-region start (point) name)
    (unless no-pretty-print
      (epub--pretty-print-xml start (point)))))

;;; BIG UGLY HACK - REDEFINING shr-image-fetched
(eval-after-load "shr"
  '(progn
     (defvar shr-image-fetched-original
       (symbol-function 'shr-image-fetched))

     (defun shr-image-fetched (status buffer start end &optional flags)
       ;; For some reason shr-image-fetched expects to be in the beginnig of
       ;; the buffer containing image, and isn't. So let's move it manually
       (goto-char (point-min))
       (funcall shr-image-fetched-original
                status buffer start end flags))))

(defun epub--create-navpoint-handler (archive ncx-path navpoint-content spine-toc-idx)
  "Create handler for navigation buttons"
  (let* ((node-path (concat ncx-path navpoint-content))
	 (split-node (split-string node-path "#"))
         (arc-path (car split-node))
         (html-path (cadr split-node)))
    (lambda (_)
      (switch-to-buffer (get-buffer-create epub-render-buffer))
      ;;todo figure out a reliable way to display human-readable name
      (let ((dom (epub--archive-get-dom archive arc-path)))
        (erase-buffer)
        (shr-insert-document dom)
	(let ((prev-next (gethash navpoint-content spine-toc-idx)))
	  ;; insert navigation buttons
	  (when (car prev-next)
	    (insert "\n")
	    (insert-text-button
	     "Previous page"
	     'action (epub--create-navpoint-handler archive ncx-path (car prev-next) spine-toc-idx)
	     'follow-link t))
	  (when (cdr prev-next)
	    (insert "\n")
	    (insert-text-button
	     "Next page"
	     'action (epub--create-navpoint-handler archive ncx-path (cdr prev-next) spine-toc-idx)
	     'follow-link t)))
        (goto-char (point-min)))
      ;;  (let ((rendered (epub--archive-get-rendered-page archive arc-path)))
      ;;    (erase-buffer)
      ;;    (insert rendered)
      ;;    (goto-char (point-min)))
      )))

(defun epub--insert-navpoint (navpoint ncx-path archive spine-toc-idx &optional ident-str)
  "Inserts navigation button in TOC buffer"
  (let ((navpoint-content (epub--xml-prop (epub--xml-node navpoint 'content) 'src))
        (text (caddr-safe (epub--xml-node navpoint 'navLabel 'text))) 
        (point-start (point)))
    (when ident-str (insert ident-str)) 
    (insert text)
    (make-text-button
     point-start (point)
     'action (epub--create-navpoint-handler archive ncx-path navpoint-content spine-toc-idx)
     'follow-link t)
    (insert "\n")))

(defun epub--insert-navmap (navmap archive ncx-path spine-toc-idx &optional ident-str)
  "Inserts navigation buttons in TOC buffer"
  (cl-loop for navpoint in navmap
           when (epub--xml-node navpoint 'navLabel 'text)
           do
           (epub--insert-navpoint navpoint ncx-path archive spine-toc-idx ident-str)
           (epub--insert-navmap navpoint archive ncx-path spine-toc-idx (concat ident-str "  "))))

(defun epub--pretty-print-xml (&optional begin end)
  (interactive (and (use-region-p) (list (region-beginning) (region-end))))
  (save-excursion
      (nxml-mode)
      (goto-char (or begin (point-min)))
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region (or begin (point-min)) (or end (point-max)))))

(defun epub--locate-rootfile (archive)
  (let* ((container (epub--archive-get-xml archive "META-INF/container.xml"))
         (rootfile (epub--xml-node container 'rootfiles 'rootfile))
         (full-path (epub--xml-prop rootfile 'full-path)))
    (unless (stringp full-path)
      (error "Unable to locate epub document root file"))
    full-path))

(defun epub--locate-ncx (dom rootfile)
  "Locate and return relative path for navigation control file"
  (let* ((manifest (epub--xml-node dom 'manifest))
         (ncx
          (cl-loop for item in (cddr-safe manifest)
                   when (and
                         (string= "application/x-dtbncx+xml"
                                  (epub--xml-prop item 'media-type))
                         (string-prefix-p "ncx"
                                          (epub--xml-prop item 'id) t))
                   return item))
         (href (epub--xml-prop ncx 'href)))
    (unless (stringp href)
      (error "Error locating ncx in epub document manifest"))
    (epub--href-relative href rootfile)))

(defun epub--xml-node (item &rest keys)
  (unless (> (length keys) 0)
    (error "Insufficient number of keys"))
  (let ((node item))
    (cl-loop for key in keys
             if (assq key (cddr-safe node))
             do (setq node it)
             else return nil
             finally return node)))

(defun epub--xml-prop (item key)
  (cdr-safe (assq key (cadr-safe item))))

(defun epub--href-relative (name &optional relative-to)
  (concat (or (file-name-directory (or relative-to "")) "") name))

(defun epub--fill-cache (archive name value)
  (push (cons name value)
        (cdr (assoc archive epub--archive-cache)))
  value)

(defun epub--get-cached(archive name &optional cache-fill-func)
  (let ((arc-cache (cdr (assoc archive epub--archive-cache))))
    (unless arc-cache
      (push (cons archive nil)
            epub--archive-cache))
    (let ((cache (cdr (assoc name arc-cache))))
      (when (and (not cache)
                 cache-fill-func)
        (setq cache (funcall cache-fill-func archive name)))
      cache)))

(defun epub--fill-dom-cache (archive name)
  (let ((dom-cache (with-temp-buffer
                     (archive-zip-extract archive name)
                     (epub--convert-links archive
                                          (libxml-parse-html-region (point-min)
                                                                    (point-max))
					  (file-name-directory name)))))
    (epub--fill-cache archive name dom-cache)))

(defun epub--archive-get-dom (archive name)
  (epub--get-cached archive
                    name
                    #'epub--fill-dom-cache))

(defun epub--map-alist (alist mapper)
  "Maps ALIST entries with MAPPER function. 
Mapper must accept a cons cell and return updated cons cell"
  (cond ((not (listp alist)) alist) ;; atom
        ((not (listp (cdr alist))) (funcall mapper alist)) ;; flat alist entry
        (t (mapcar (lambda (x) (epub--map-alist x mapper)) ;; nested alists
                   alist))))

(defun epub--extract-link (archive link)
  "From ARCHIVE, extract file at relative path LINK and return url for extracted"
  (let ((tmp-file (make-temp-file "epub-src"
                                  nil
                                  (file-name-extension link t))))
    (with-temp-file tmp-file
      (archive-zip-extract archive link)
      (goto-char (point-min))
      (concat "file://" tmp-file))))

(defun epub--convert-links (archive dom link-root)
  "From ARCHIVE, extract non-external links in DOM and replace them 
with links to temporary files. Returns updated DOM"
  (epub--map-alist dom
                   (lambda (x)
                     (if (and (or (eq (car x) 'src) (eq (car x) 'href))
                              (not (string-match "^[[:alpha:]]+[[:alnum:]+-.]*://"  ; assume URL is anything that starts with proper scheme name followed by ://
                                                 (cdr x))))
			 (cons 'src (epub--extract-link archive (concat link-root (cdr x))))
                       x))))

(defun epub--fill-render-cache (archive name)
  (let ((rendered
         (with-temp-buffer
           (when epub--debug
             (message "epub.el: Extracting %S, looking for %S" archive name))
           (archive-zip-extract archive name)
           (let ((dom
                  (epub--convert-links archive
                                       (libxml-parse-html-region (point-min)
                                                                 (point-max))
				       (file-name-directory name))))
             (erase-buffer)
             (shr-insert-document dom)
             (buffer-substring (point-min) (point-max))))))
    (epub--fill-cache archive name rendered)))

(defun epub--archive-get-rendered-page (archive name)
  (epub--get-cached archive
                    name
                    #'epub--fill-render-cache))

(defun epub--fill-xml-cache (archive name)
  (let ((xml-cache (with-temp-buffer
                     (epub--insert-xml archive name t)
                     (libxml-parse-xml-region (point-min) (point-max)))))
    (epub--fill-cache archive name xml-cache)))

(defun epub--archive-get-xml (archive name)
  (epub--get-cached archive
                    name
                    #'epub--fill-xml-cache))

(unless (fboundp 'libxml-parse-html-region)
  (error "epub.el requires Emacs to be compiled with libxml2"))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-mode))
(add-to-list 'auto-coding-alist '("\\.epub\\'" . no-conversion-multibyte))

(provide 'epub)
