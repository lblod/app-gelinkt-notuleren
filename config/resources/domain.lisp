(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
;;(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* nil)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#decimal"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#string"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#float"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#double"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#integer"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#int"
    (value object)
  (declare (ignore object))
  value)
(defun handle-relation-get-call (base-path id relation)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (let* ((resource (find-resource-by-path base-path))
               (link (find-resource-link-by-path resource relation)))
          (show-relation-call resource id link)))
    (incorrect-accept-header (condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (no-such-resource ()
      (respond-not-found))
    (no-such-property (condition)
      (let ((message
             (format nil "Could not find property (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))
    (cl-fuseki:sesame-exception (exception)
      (declare (ignore exception))
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Could not execute SPARQL query.")))))))
    (no-such-link (condition)
      (let ((message
             (format nil "Could not find link (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))))
(read-domain-file "slave-besluit-domain.lisp")
(read-domain-file "slave-mandaat-domain.lisp")
(read-domain-file "slave-organisatie-domain.lisp")
(read-domain-file "slave-leidinggevenden-domain.lisp")
(read-domain-file "slave-contact-domain.lisp")
(read-domain-file "generic-model-plugin-domain.lisp")
(read-domain-file "tasklist-domain.lisp")
(read-domain-file "master-editor.lisp")
(read-domain-file "master-mobiliteit.lisp")
(read-domain-file "reports.lisp")
(read-domain-file "master-log-domain.lisp")
(read-domain-file "master-jobs-domain.lisp")

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
(run-validations)

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:name :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "nfo:fileCreated")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download"))
  :resource-base (s-url "http://data.example.com/files/")
  :features `(include-uri)
  :on-path "files")
