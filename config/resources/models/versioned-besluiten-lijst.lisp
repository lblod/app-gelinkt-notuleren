(define-resource versioned-besluiten-lijst ()
  :class (s-prefix "ext:VersionedBesluitenLijst")
  :properties `((:state :string ,(s-prefix "ext:stateString"))
                (:content :string ,(s-prefix "ext:content"))
                (:deleted :boolean ,(s-prefix "ext:deleted")))
  :has-many `((signed-resource :via ,(s-prefix "ext:signsBesluitenlijst")
                               :inverse t
                               :as "signed-resources"))
  :has-one `((published-resource :via ,(s-prefix "ext:publishesBesluitenlijst")
                                 :inverse t
                                 :as "published-resource")
             (editor-document :via ,(s-prefix "prov:wasDerivedFrom")
                              :as "editor-document")
             (zitting :via ,(s-prefix "besluit:heeftBesluitenlijst")
                                 :inverse t
                                 :as "zitting"))
  :resource-base (s-url "http://data.lblod.info/prepublished-besluiten-lijsten/")
  :features '(include-uri)
  :on-path "versioned-besluiten-lijsten")