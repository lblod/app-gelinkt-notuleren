(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
;;(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* nil)

(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "slave-mandaat-domain.lisp")
(read-domain-file "slave-organisatie-domain.lisp")
(read-domain-file "generic-model-plugin-domain.lisp")
(read-domain-file "tasklist-domain.lisp")

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
                (:updated-on :datetime ,(s-prefix "pav:lastUpdateOn"))
                (:starred :boolean ,(s-prefix "tmp:starred"))
                (:origin :string ,(s-prefix "pav:providedBy"))) ;;de gemeente Niel
  :has-one `((editor-document-status :via ,(s-prefix "ext:editorDocumentStatus")
                                     :as "status")
             (editor-document :via ,(s-prefix "pav:previousVersion")
                              :as "previous-version")
             (editor-document :via ,(s-prefix "pav:previousVersion")
                              :inverse t
                              :as "next-version")
             (zitting :via ,(s-prefix "besluit:heeftNotulen")
                      :inverse t
                      :as "zitting")
             (document-container :via ,(s-prefix "pav:hasVersion")
                                 :inverse t
                                 :as "document-container"))
  :has-many `((tasklist-solution :via ,(s-prefix "ext:editorDocumentTasklistSolution")
                                 :as "tasklist-solutions"))
  :resource-base (s-url "http://lblod.info/editor-documents/")
  :features `(no-pagination-defaults)
  :on-path "editor-documents")

(define-resource document-container ()
  :class (s-prefix "ext:DocumentContainer")
  :has-many `((editor-document :via ,(s-prefix "pav:hasVersion")
                               :as "revisions"))
  :has-one `((editor-document :via ,(s-prefix "pav:hasCurrentVersion")
                              :as "current-version"))
  :resource-base (s-url "http://lblod.info/document-containers/")
  :on-path "document-containers")

(define-resource editor-document-status ()
  :class (s-prefix "ext:EditorDocumentStatus")
  :properties `((:name :string ,(s-prefix "ext:EditorDocumentStatusName")))
  :resource-base (s-url "http://lblod.info/editor-document-statuses/")
  :features `(no-pagination-defaults)
  :on-path "editor-document-statuses")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; USERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-resource gebruiker ()
  :class (s-prefix "foaf:Person")
  :resource-base (s-url "http://data.lblod.info/id/gebruiker/")
  :properties `((:voornaam :string ,(s-prefix "foaf:firstName"))
                (:achternaam :string ,(s-prefix "foaf:familyName"))
                (:rijksregister-nummer :string ,(s-prefix "dct:identifier")))
  :has-many `((account :via ,(s-prefix "foaf:account")
                       :as "account")
              (bestuurseenheid :via ,(s-prefix "foaf:member")
                              :as "bestuurseenheden"))
  :on-path "gebruikers")

(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :resource-base (s-url "http://data.lblod.info/id/account/")
  :properties `((:provider :via ,(s-prefix "foaf:accountServiceHomepage"))
                (:vo-id :via ,(s-prefix "dct:identifier")))
  :has-one `((gebruiker :via ,(s-prefix "foaf:account")
                         :inverse t
                         :as "gebruiker"))
  :on-path "accounts"
)
