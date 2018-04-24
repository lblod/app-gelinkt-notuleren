(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")

(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "slave-mandaat-domain.lisp")
(read-domain-file "slave-organisatie-domain.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEMPLATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource template ()
  :class (s-prefix "ext:Template")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:matches :string-set ,(s-prefix "ext:templateMatches"))
                (:body :string ,(s-prefix "ext:templateContent"))
                (:contexts :uri-set ,(s-prefix "ext:activeInContext"))
                (:disabled-in-contexts :uri-set ,(s-prefix "ext:disabledInContext")))
  :resource-base (s-url "http://lblod.info/templates/")
  :features `(no-pagination-defaults)
  :on-path "templates")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DOCUMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource editor-document ()
  :class (s-prefix "ext:EditorDocument")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:content :string ,(s-prefix "ext:editorDocumentContent"))
                (:context :string ,(s-prefix "ext:editorDocumentContext"))
                (:created-on :datetime ,(s-prefix "pav:createdOn"))
                (:starred :boolean ,(s-prefix "tmp:starred"))
                (:origin :string ,(s-prefix "pav:providedBy"))) ;;de gemeente Niel

  :has-one `((editor-document-status :via ,(s-prefix "ext:editorDocumentStatus")
                                     :as "status")
             (editor-document        :via ,(s-prefix "pav:previousVersion")
                                     :as "previous-version")
             (editor-document :via ,(s-prefix "pav:previousVersion")
                              :inverse t
                              :as "next-version")
             (zitting :via ,(s-prefix "besluit:heeftNotulen")
                              :inverse t
                              :as "zitting"))
  :resource-base (s-url "http://lblod.info/editor-documents/")
  :features `(no-pagination-defaults)
  :on-path "editor-documents")

(define-resource editor-document-status ()
  :class (s-prefix "ext:EditorDocumentStatus")
  :properties `((:name :string ,(s-prefix "ext:EditorDocumentStatusName")))
  :resource-base (s-url "http://lblod.info/editor-document-statuses/")
  :features `(no-pagination-defaults)
  :on-path "editor-document-statuses")
