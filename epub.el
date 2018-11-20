;;; -*- lexical-binding: t -*-
(require 'shr)
(require 'arc-mode)

(defun epub-mode ()
  "Major mode for viewing .epub files"
  (interactive)
  (let* ((archive buffer-file-name)
         (name (file-name-sans-extension (file-name-nondirectory archive)))
         (toc (format "Table of Contents (%s)" name)))
    (unless epub--debug
      (kill-buffer))
    (epub--show-toc archive toc)))

(defun epub-goto-page (context page)
  "Go to page in epub file."
  (interactive (list
	        (or (and (boundp 'epub--current-context) epub--current-context)
		    (let ((arc-name (completing-read "Which opened epub? "
						     epub--context-hash-table
						     nil
						     t)))
		      (gethash arc-name epub--context-hash-table)))
	        (string-to-number (read-from-minibuffer "Page number: " "1"))))
  (let ((page-href (gethash page
			    (epub-context-pagenum-idx context))))
    (unless page-href
      (error "Page not found"))
    (switch-to-buffer (get-buffer-create epub-render-buffer))
    (setq-local epub--current-context context)
    (epub--render-page page-href context)))

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

(defvar epub--context-hash-table (make-hash-table :test #'equal)
  "Hash table with key being full path to archive, value being associated `epub-context'")

(defvar-local epub--current-context nil
  "Buffer-local variable containing associated `epub-context'")

(defsubst cadr-safe (x)
  "Return the safe car of the cdr of X."
  (car-safe (cdr-safe x)))

(defsubst cddr-safe (x)
  "Return the safe cdr of the cdr of X."
  (cdr-safe (cdr-safe x)))

(defsubst caddr-safe (x)
  "Return the safe car of the cdr of the cdr of X."
  (car-safe (cdr-safe (cdr-safe x))))

(cl-defstruct epub-context
  archive
  ncx-path
  href-idx
  pagenum-idx)

(defun epub--show-toc (archive &optional buffer)
  "Assuming ARCHIVE is a valid epub file, switch to BUFFER and render table of contents there."
  (let* ((rootfile (epub--locate-rootfile archive))
	 (dom (epub--archive-get-xml archive rootfile))
	 (manifest-idx (epub--manifest-idx dom))
	 (spine-toc-indeces (epub--spine-toc-idx dom manifest-idx))
         (ncx-file (epub--locate-ncx dom rootfile))
         (ncx-path (file-name-directory (or ncx-file "")))
         (ncx (epub--archive-get-xml archive ncx-file))
         (title (caddr-safe (epub--xml-node ncx 'docTitle 'text)))
         (navmap (epub--xml-node ncx 'navMap))
         (buf (get-buffer-create (or buffer epub-toc-buffer)))
	 (context (make-epub-context
			:archive archive
			:ncx-path ncx-path
			:href-idx (car spine-toc-indeces)
			:pagenum-idx (cdr spine-toc-indeces))))
    (puthash archive context epub--context-hash-table)
    (pop-to-buffer-same-window buf)
    (indented-text-mode)
    (erase-buffer)
    (insert title "\n\nTable of contents\n\n")
    (epub--insert-navmap navmap context)
    (insert "\n\n")
    (if epub--debug (epub--insert-xml archive ncx-file))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq-local epub--current-context context)
    (setq buffer-read-only t)))

(defun epub--spine-toc-idx (dom manifest-idx)
  "Create table-of-contents hashtable, with key as page href, 
and value as pair (previous-page-href . next-page-href)"
  (when epub--debug
    (message "epub.el: Building href navigation indeces"))
  (let* ((spine-root (epub--xml-node dom 'spine))
	 (toc-id (epub--xml-prop spine-root 'toc))
	 (spine-items (cddr-safe spine-root))
	 (href-idx (make-hash-table :test #'equal))
	 (pagenum-idx (make-hash-table :test #'equal)))
    (cl-loop for items = spine-items then (cdr items)
	     for prev-href = nil then (gethash item-id manifest-idx)
	     for next-href = (gethash (epub--xml-prop (cadr-safe items) 'idref) manifest-idx)
	     for item-id = (epub--xml-prop (car items) 'idref)
	     for this-href = (gethash item-id manifest-idx)
	     for pagenum = 1 then (1+ pagenum)
	     until (null items) 
	     if (not (string= item-id toc-id)) ;; skip toc
	     do (progn
		  (puthash this-href (cons prev-href next-href)
			   href-idx)
		  (puthash this-href pagenum
			   pagenum-idx)
		  (puthash pagenum this-href
			   pagenum-idx)))
    (cons href-idx pagenum-idx)))

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

(defun epub--create-navpoint-handler (archive-context page-href)
  "Create handler for navigation buttons"
  (lambda (_)
    (switch-to-buffer (get-buffer-create epub-render-buffer))
    (setq-local epub--current-context archive-context)
    (epub--render-page page-href archive-context)))

(defun epub--render-page (page-href archive-context)
  "Renders page PAGE-HREF from ARCHIVE-CONTEXT"
  (view-mode 0)
  ;;todo figure out a reliable way to display human-readable name without creating new buffer
  (let* ((href-idx (epub-context-href-idx archive-context))
	(pagenum-idx (epub-context-pagenum-idx archive-context))
	(node-path (concat (epub-context-ncx-path archive-context) page-href))
	(split-node (split-string node-path "#"))
        (arc-path (car split-node))
	(prev-next (gethash page-href href-idx))
	(pagenum (gethash page-href pagenum-idx))
	(dom (epub--archive-get-dom (epub-context-archive archive-context) arc-path)))
    (erase-buffer)
    (insert (format "Page %d\n" pagenum))
    ;; insert navigation buttons
    (when (car prev-next)
      (insert-text-button
       "Previous page"
       'action (epub--create-navpoint-handler archive-context (car prev-next))
       'follow-link t)
      (insert "\n"))
    (when (cdr prev-next)
      (insert-text-button
       "Next page"
       'action (epub--create-navpoint-handler archive-context (cdr prev-next))
       'follow-link t)
      (insert "\n"))
    (shr-insert-document dom)
    (goto-char (point-min)))
  (view-mode 1))

  
(defun epub--insert-navpoint (navpoint archive-context &optional ident-str)
  "Inserts navigation button in TOC buffer"
  (let ((navpoint-content (epub--xml-prop (epub--xml-node navpoint 'content) 'src))
        (text (caddr-safe (epub--xml-node navpoint 'navLabel 'text))) 
        (point-start (point)))
    (when ident-str (insert ident-str)) 
    (insert text)
    (make-text-button
     point-start (point)
     'action (epub--create-navpoint-handler archive-context navpoint-content)
     'follow-link t)
    (insert "\n")))

(defun epub--insert-navmap (navmap context &optional ident-str)
  "Inserts navigation buttons in TOC buffer"
  (cl-loop for navpoint in navmap
           when (epub--xml-node navpoint 'navLabel 'text)
           do
           (epub--insert-navpoint navpoint context ident-str)
           (epub--insert-navmap navpoint context (concat ident-str "  "))))

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
  (let ((tmp-file (make-temp-file "epub-src" ;;todo delete temp file later
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
