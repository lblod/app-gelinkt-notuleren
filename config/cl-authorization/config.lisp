;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

(add-delta-logger)
(add-delta-messenger "http://deltanotifier/")

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* nil)
(setf *backend* "http://virtuoso:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil)

;;;;;;;;;;;;;;;;;;;;;;;
;;; quad transformations
(in-package :quad-transformations)

(quad-transformations:define-quad-transformation (quad method)
  ;; make quad objects which have datatype in uuid specification just strings
  (if (and
       ;; predicate is uuid
       (string= (detect-quads::quad-term-uri (quad:predicate quad))
                "http://mu.semte.ch/vocabularies/core/uuid")
       ;; object has datatype
       (= (length (sparql-parser:match-submatches (quad:object quad))) 3))
      (let ((new-quad (quad:copy quad))) ; make new quad
        (setf (quad:object new-quad)
              (handle-update-unit::make-nested-match
               `(ebnf::|RDFLiteral| ,(first (sparql-parser:match-submatches (quad:object quad))))))
        ;; use the new quad
        (quad-transformations:update new-quad))
      ;; otherwise keep it
      (quad-transformations:keep)))

(quad-transformations:define-quad-transformation (quad method)
  ;; make quad objects which have datatype in uuid specification just strings
  (if (and
       ;; predicate is uuid
       (string= (detect-quads::quad-term-uri (quad:predicate quad))
                "http://mu.semte.ch/vocabularies/core/uuid")
       ;; object has datatype
       (= (length (sparql-parser:match-submatches (quad:object quad))) 3))
      (let ((new-quad (quad:copy quad))) ; make new quad
        (setf (quad:object new-quad)
              (handle-update-unit::make-nested-match
               `(ebnf::|RDFLiteral| ,(first (sparql-parser:match-submatches (quad:object quad))))))
        ;; use the new quad
        (quad-transformations:update new-quad))
      ;; otherwise keep it
      (quad-transformations:keep)))

;;;;;;;;;;;;;;;;;
;;; access rights
(in-package :acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")

(type-cache::add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session")

(define-prefixes
  :adms "http://www.w3.org/ns/adms#"
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :besluitvorming "http://data.vlaanderen.be/ns/besluitvorming#"
  :cogs "http://vocab.deri.ie/cogs#"
  :core "http://open-services.net/ns/core#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :signing "http://mu.semte.ch/vocabularies/ext/signing/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :foaf "http://xmlns.com/foaf/0.1/"
  :graphs "http://mu.semte.ch/graphs/"
  :leidinggevenden "http://data.lblod.info/vocabularies/leidinggevenden/"
  :locn "http://www.w3.org/ns/locn#"
  :mandaat "http://data.vlaanderen.be/ns/mandaat#"
  :mobiliteit "http://data.lblod.info/vocabularies/mobiliteit/"
  :mobiliteit "https://data.vlaanderen.be/ns/mobiliteit#"
  :mu "http://mu.semte.ch/vocabularies/core/"
  :musession "http://mu.semte.ch/vocabularies/session/"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :schema "http://schema.org/"
  :org "http://www.w3.org/ns/org#"
  :person "http://www.w3.org/ns/person#"
  :persoon "http://data.vlaanderen.be/ns/persoon#"
  :prov "http://www.w3.org/ns/prov#"
  :reporting "http://lblod.data.gift/vocabularies/reporting/"
  :rlog "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#"
  :rdfs "http://www.w3.org/2000/01/rdf-schema#"
  :tasks "http://redpencil.data.gift/vocabularies/tasks/"
  :dct "http://purl.org/dc/terms/")

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("leidinggevenden:Bestuursfunctie" -> _)
  ("leidinggevenden:Functionaris" -> _)
  ("leidinggevenden:FunctionarisStatusCode" -> _)
  ("mobiliteit:MaatregelConcept" -> _)
  ("mobiliteit:Verkeersbordcombinatie" -> _)
  ("mobiliteit:VerkeersbordconceptStatusCode" -> _)
  ("besluit:Artikel" -> _)
  ("besluit:Besluit" -> _)
  ("besluit:Bestuurseenheid" -> _)
  ("besluit:Bestuursorgaan" -> _)
  ("mandaat:Fractie" -> _)
  ("mandaat:Kandidatenlijst" -> _)
  ("mandaat:Mandaat" -> _)
  ("mandaat:Mandataris" -> _)
  ("mandaat:RechtstreekseVerkiezing" -> _)
  ("mandaat:Verkiezingsresultaat" -> _)
  ("ext:BeleidsdomeinCode" -> _)
  ("ext:BestuurseenheidClassificatieCode" -> _)
  ("ext:BestuursfunctieCode" -> _)
  ("ext:BestuursorgaanClassificatieCode" -> _)
  ("ext:EditorDocumentFolder" -> _)
  ("ext:EditorDocumentStatus" -> _)
  ("ext:GeslachtCode" -> _)
  ("ext:KandidatenlijstLijsttype" -> _)
  ("ext:MandatarisStatusCode" -> _)
  ("ext:SyncTask" -> _)
  ("ext:Task" -> _)
  ("ext:Tasklist" -> _)
  ("ext:Template" -> _)
  ("ext:VerkiezingsresultaatGevolgCode" -> _)
  ("ext:signing/BlockchainStatus" -> _)
  ("dct:PeriodOfTime" -> _)
  ("schema:ContactPoint" -> _)
  ("schema:PostalAddress" -> _)
  ("nfo:FileDataObject" -> _)
  ("rdfs:Class" -> _)
  ("rdfs:Property" -> _)
  ("skos:Concept" -> _)
  ("skos:ConceptScheme" -> _)
  ("locn:Address" -> _)
  ("org:Membership" -> _)
  ("org:Organization" -> _)
  ("org:Post" -> _)
  ("org:Role" -> _)
  ("org:Site" -> _)
  ("person:Person" -> _)
  ("prov:Location" -> _)
  ("foaf:Image" -> _)
  ("foaf:OnlineAccount" -> _)
  ("foaf:Person" -> _)
  ("mobiliteit:Verkeersbordcategorie" -> _)
  ("mobiliteit:Verkeersbordconcept" -> _)
  ("mobiliteit:VerkeersbordconceptStatus" -> _))

(define-graph sessions ("http://mu.semte.ch/graphs/sessions")
  ("musession:Session" -> _))

(supply-allowed-group "public")

(grant (read)
       :to-graph public
       :for-allowed-group "public")

(grant (read)
       :to-graph sessions
       :for-allowed-group "public")


;; ORGANIZATION HAS POSSIBLY DUPLICATE USER DATA says config.ex
(define-graph organization ("http://mu.semte.ch/graphs/organizations/")
  ("nfo:FileDataObject" -> _)
  ("ext:Attachment" -> _)
  ("besluitvorming:Agenda" -> _)
  ("ext:EditorDocument" -> _)
  ("besluit:Zitting" -> _)
  ("ext:Intermission" -> _)
  ("ext:AgendaPosition" -> _)
  ("ext:Agenda" -> _)
  ("besluit:Agendapunt" -> _)
  ("besluit:BehandelingVanAgendapunt" -> _)
  ("besluit:Stemming" -> _)
  ("ext:DocumentContainer" -> _)
  ("ext:VersionedAgenda" -> _)
  ("ext:VersionedNotulen" -> _)
  ("ext:VersionedBesluitenLijst" -> _)
  ("ext:VersionedBehandeling" -> _)
  ("ext:VersionedRegulatoryStatement" -> _)
  ("tasks:Task" -> _)
  ("ext:TaskSolution" -> _)
  ("ext:TasklistSolution" -> _)
  ("person:Person" -> _)
  ("persoon:Geboorte" -> _)
  ("adms:Identifier" -> _)
  ("signing:SignedResource" -> _)
  ("signing:PublishedResource" -> _)
  ("ext:PublishingLog" -> _))

(supply-allowed-group "org"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
         PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
         SELECT DISTINCT ?session_group WHERE {
           <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                        ext:sessionRole ?role.
           FILTER(?role in (\"GelinktNotuleren-lezer\",\"GelinktNotuleren-schrijver\", \"GelinktNotuleren-publiceerder\",  \"GelinktNotuleren-ondertekenaar\"))
         }"
  :parameters (list "session_group"))

(grant (read)
       :to-graph organization
       :for-allowed-group "org")

(define-graph organization-writable ("http://mu.semte.ch/graphs/organizations/")
  ("nfo:FileDataObject" -> _)
  ("ext:Attachment" -> _)
  ("ext:EditorDocument" -> _)
  ("besluit:Zitting" -> _)
  ("ext:Intermission" -> _)
  ("ext:AgendaPosition" -> _)
  ("ext:Agenda" -> _)
  ("besluit:Agendapunt" -> _)
  ("besluit:BehandelingVanAgendapunt" -> _)
  ("besluit:Stemming" -> _)
  ("ext:DocumentContainer" -> _)
  ("ext:TaskSolution" -> _)
  ("ext:TasklistSolution" -> _)
  ("tasks:Task" -> _)
  ("person:Person" -> _)
  ("persoon:Geboorte" -> _)
  ("adms:Identifier" -> _)
  ("ext:PublishingLog" -> _))

(supply-allowed-group "org-wf"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
         PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
         SELECT DISTINCT ?session_group WHERE {
           <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                        ext:sessionRole \"GelinktNotuleren-schrijver\".
         }"
  :parameters (list "session_group"))

(grant (write)
       :to-graph organization-writable
       :for-allowed-group "org-wf")

(define-graph organization-publication ("http://mu.semte.ch/graphs/organizations/")
  ("besluitvorming:Agenda"
   -> _
   <- _)
  ("ext:VersionedAgenda"
   -> _
   <- _)
  ("ext:VersionedNotulen"
   -> _
   <- _)
  ("ext:VersionedBesluitenLijst"
   -> _
   <- _)
  ("ext:VersionedBehandeling"
   -> _
   <- _)
  ("ext:VersionedRegulatoryStatement"
   -> _
   <- _)
  ("signing:PublishedResource"
   -> _
   <- _)
  ("tasks:Task"
   -> _
   <- _)
  ("ext:DocumentContainer"
   -> _
   <- _)) ; needed to update status on publishing decision/notulen

(supply-allowed-group "org-publish"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
         PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
         SELECT DISTINCT ?session_group WHERE {
           <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                        ext:sessionRole \"GelinktNotuleren-publiceerder\".
         }"
  :parameters (list "session_group"))

(grant (write)
       :to-graph organization-publication
       :for-allowed-group "org-publish")

(define-graph organization-signing ("http://mu.semte.ch/graphs/organizations/")
  ("besluitvorming:Agenda" -> _ <- _)
  ("ext:VersionedAgenda" -> _ <- _)
  ("ext:VersionedNotulen" -> _ <- _)
  ("ext:VersionedBesluitenLijst" -> _ <- _)
  ("ext:VersionedBehandeling" -> _ <- _)
  ("signing:SignedResource" -> _ <- _)
  ("tasks:Task" -> _ <- _)
  ("ext:PublishingLog" -> _ <- _))

(supply-allowed-group "org-sign"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
         PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
         SELECT DISTINCT ?session_group WHERE {
           <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                        ext:sessionRole \"GelinktNotuleren-ondertekenaar\".
         }"
  :parameters (list "session_group"))

(grant (write)
       :to-graph organization-signing
       :for-allowed-group "org-sign")

(define-graph reports ("http://mu.semte.ch/graphs/reports")
  ; For logs, reports (dashboard and report service)
  ("rlog:Entry" -> _)
  ("rlog:Level" -> _)
  ("rlog:StatusCode" -> _)
  ("ext:LogSource" -> _)
  ("reporting:Report" -> _)
  ("cogs:Job" -> _)
  ("core:Error" -> _)
  ("nfo:DataContainer" -> _)
  ("nfo:FileDataObject" -> _)
  ("nfo:DataContainer" -> _))

(supply-allowed-group "org-reports"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
         PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
         PREFIX session: <http://mu.semte.ch/vocabularies/session/>
         SELECT DISTINCT * WHERE {
           <SESSION_ID> session:account/ext:sessionRole \"GelinktNotuleren-report-admin\".
         }")

(grant (write)
       :to-graph reports
       :for-allowed-group "org-reports")
