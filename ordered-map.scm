(define-record-type ordered-map
  (%make-ordered-map entry-list entry-table)
  ordered-map?
  (entry-list omap:entry-list set-omap:entry-list!)
  (entry-table omap:entry-table))

(define (make-ordered-map)
  (%make-ordered-map #f (make-equal-hash-table)))

(define-record-type omap-entry
  (make-omap-entry key item next prev)
  omap-entry?
  (key omap-entry-key set-omap-entry-key!)
  (item omap-entry-item set-omap-entry-item!)
  (next omap-entry-next set-omap-entry-next!)
  (prev omap-entry-prev set-omap-entry-prev!))

(define (omap:fetch-entry omap key)
  (hash-table/get (omap:entry-table omap) key #f))

(define (omap:put! omap key datum)
  (let ((entry (omap:fetch-entry omap key)))
    (if entry 
	(set-omap-entry-item! entry datum)
	(omap:put-new-entry! omap key datum))))

(define (omap:put-new-entry! omap key datum)
  (let* ((head (omap:entry-list omap))
	 (new-entry (make-omap-entry key datum head #f)))
    (if head (set-omap-entry-prev! head new-entry))
    (set-omap:entry-list! omap new-entry)
    (hash-table/put! (omap:entry-table omap) key new-entry)))

(define (omap:get omap key default)
  (let ((entry (omap:fetch-entry omap key)))
    (if entry 
	(omap-entry-item entry)
	default)))

(define (omap:remove! omap key)
  (let ((entry (omap:fetch-entry omap key)))
    (if entry
	(omap:remove-entry! omap key entry))))

(define (omap:remove-entry! omap key entry)
  (hash-table/remove! (omap:entry-table omap) key)
  (let ((old-prev (omap-entry-prev entry))
	(old-next (omap-entry-next entry)))
    (if old-prev (set-omap-entry-next! old-prev old-next))
    (if old-next (set-omap-entry-prev! old-next old-prev))))

(define (omap:count omap)
  (hash-table/count (omap:entry-table omap)))

(define (omap:key-list omap)
  (reverse
   (let loop ((head (omap:entry-list omap)))
     (if head
	 (cons (omap-entry-key head)
	       (loop (omap-entry-next head)))
	 '()))))

(define (omap:for-each omap procedure)
  (let loop ((head (omap:entry-list omap)))
    (if head
	(begin (loop (omap-entry-next head))
	       (procedure (omap-entry-key head) (omap-entry-item head)))
	unspecific)))

