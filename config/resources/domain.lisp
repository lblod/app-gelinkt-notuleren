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
(read-domain-file "master-editor.lisp")
(read-domain-file "master-blockchain.lisp")

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
