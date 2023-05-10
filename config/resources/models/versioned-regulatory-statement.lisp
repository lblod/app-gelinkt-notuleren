(define-resource versioned-regulatory-statement ()
  :class (s-prefix "ext:VersionedRegulatoryStatement")
  :properties `((:state :string ,(s-prefix "ext:stateString")))
  :has-many `((signed-resource :via ,(s-prefix "ext:signsRegulatoryStatement")
                               :inverse t
                               :as "signed-resources"))
  :has-one `((published-resource :via ,(s-prefix "ext:publishesRegulatoryStatement")
                                 :inverse t
                                 :as "published-resource")
             (versioned-behandeling :via ,(s-prefix "ext:hasVersionedRegulatoryStatement")
                                 :inverse t
                                 :as "versioned-behandeling")
             (editor-document :via ,(s-prefix "ext:regulatoryStatement")
                                         :as "regulatory-statement")
             (file :via ,(s-prefix "prov:generated")
                            :as "file"))
  :resource-base (s-url "http://data.lblod.info/prepublished-regulatory-statement/")
  :features '(include-uri)
  :on-path "versioned-regulatory-statements")