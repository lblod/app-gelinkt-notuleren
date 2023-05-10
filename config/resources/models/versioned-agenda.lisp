(define-resource versioned-agenda ()
  :class (s-prefix "ext:VersionedAgenda")
  :properties `((:state :string ,(s-prefix "ext:stateString"))
                (:content :string ,(s-prefix "ext:content"))
                (:kind :string ,(s-prefix "ext:agendaKind"))
                (:deleted :boolean ,(s-prefix "ext:deleted")))
  :has-many `(
              (signed-resource :via ,(s-prefix "ext:signsAgenda")
                               :inverse t
                               :as "signed-resources")
              )
  :has-one `(
             (published-resource :via ,(s-prefix "ext:publishesAgenda")
                                 :inverse t
                                 :as "published-resource")
             (editor-document :via ,(s-prefix "prov:wasDerivedFrom")
                              :as "editor-document")
             (document-container :via ,(s-prefix "ext:hasVersionedAgenda")
                                 :inverse t
                                 :as "document-container")
             )
  :resource-base (s-url "http://data.lblod.info/prepublished-agendas/")
  :features '(include-uri)
  :on-path "versioned-agendas")