;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

(setf *delta-handlers* nil)
(add-delta-logger)
(add-delta-messenger "http://deltanotifier/")

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t)
(setf *log-incoming-requests* t)
(setf *backend* "http://triplestore:8890/sparql")

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

(defparameter *access-specifications* nil
  "All known ACCESS specifications.")

(defparameter *graphs* nil
  "All known GRAPH-SPECIFICATION instances.")

(defparameter *rights* nil
  "All known GRANT instances connecting ACCESS-SPECIFICATION to GRAPH.")

(define-prefixes 
  :adms "http://www.w3.org/ns/adms#"
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :besluitvorming "http://data.vlaanderen.be/ns/besluitvorming#"
  :cogs "http://vocab.deri.ie/cogs#"
  :dct "http://purl.org/dc/terms/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :foaf "http://xmlns.com/foaf/0.1/"
  :gn "http://data.lblod.info/vocabularies/gelinktnotuleren/"
  :installatie "https://wegenenverkeer.data.vlaanderen.be/ns/installatie#"
  :lblodlg "http://data.lblod.info/vocabularies/leidinggevenden/"
  :lblodmow "http://data.lblod.info/vocabularies/mobiliteit/"
  :lmb "http://lblod.data.gift/vocabularies/lmb/"
  :locn "http://www.w3.org/ns/locn#"
  :mandaat "http://data.vlaanderen.be/ns/mandaat#"
  :mobiliteit "https://data.vlaanderen.be/ns/mobiliteit#"
  :musession "http://mu.semte.ch/vocabularies/session/"
  :person "http://www.w3.org/ns/person#"
  :persoon "http://data.vlaanderen.be/ns/persoon#"
  :prov "http://www.w3.org/ns/prov#"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :onderdeel "https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#"
  :org "http://www.w3.org/ns/org#"
  :oslc "http://open-services.net/ns/core#"
  :rdfs "http://www.w3.org/2000/01/rdf-schema#"
  :reporting "http://lblod.data.gift/vocabularies/reporting/"
  :rlog "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/rlog#"
  :schema "http://schema.org/"
  :sign "http://mu.semte.ch/vocabularies/ext/signing/"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :task "http://redpencil.data.gift/vocabularies/tasks/"
  :svariables "https://lblod.data.gift/vocabularies/variables/"
)

; This incantation tells sparql-parser to treat any subject with a URI starting with a certain
; prefix as having a specific rdf:type
(type-cache::add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session")

(define-graph session-graph ("http://mu.semte.ch/graphs/sessions")
  ("musession:Session" -> _)
)

(define-graph public-graph ("http://mu.semte.ch/graphs/public")
  ("lblodlg:Bestuursfunctie" -> _)
  ("lblodlg:Functionaris" -> _)
  ("lblodlg:FunctionarisStatusCode" -> _)
  ("lblodmow:MaatregelConcept" -> _)
  ("lblodmow:Verkeersbordcombinatie" -> _)
  ("lblodmow:VerkeersbordconceptStatusCode" -> _)
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
  ("oslc:Error" -> _)
  ("ext:Tasklist" -> _)
  ("ext:Template" -> _)
  ("ext:VerkiezingsresultaatGevolgCode" -> _)
  ("sign:BlockchainStatus" -> _)
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
  ("mobiliteit:VerkeersbordconceptStatus" -> _)
  ("lmb:Bestuursperiode" -> _)
)

(define-graph org-read-graph ("http://mu.semte.ch/graphs/organizations/")
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
  ("task:Task" -> _)
  ("oslc:Error" -> _)
  ("ext:TaskSolution" -> _)
  ("ext:TasklistSolution" -> _)
  ("person:Person" -> _)
  ("persoon:Geboorte" -> _)
  ("adms:Identifier" -> _)
  ("sign:SignedResource" -> _)
  ("sign:PublishedResource" -> _)
  ("ext:PublishingLog" -> _)
  ("ext:Installatievergadering" -> _)
  ("ext:InstallatievergaderingSynchronizationStatus" -> _)
  ("gn:AangepasteStemming" -> _)
  ("ext:UserPreference" -> _)
)

(define-graph org-write-graph ("http://mu.semte.ch/graphs/organizations/")
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
  ("task:Task" -> _)
  ("oslc:Error" -> _)
  ("person:Person" -> _)
  ("persoon:Geboorte" -> _)
  ("adms:Identifier" -> _)
  ("ext:PublishingLog" -> _)
  ("ext:Installatievergadering" -> _)
  ("ext:InstallatievergaderingSynchronizationStatus" -> _)
  ("gn:AangepasteStemming" -> _)
  ("ext:UserPreference" -> _)
)

(define-graph org-pub-graph ("http://mu.semte.ch/graphs/organizations/")
  ("nfo:FileDataObject" -> _)
  ("besluitvorming:Agenda" -> _)
  ("ext:VersionedAgenda" -> _)
  ("ext:VersionedNotulen" -> _)
  ("ext:VersionedBesluitenLijst" -> _)
  ("ext:VersionedBehandeling" -> _)
  ("ext:VersionedRegulatoryStatement" -> _)
  ("sign:PublishedResource" -> _)
  ("task:Task" -> _)
  ("oslc:Error" -> _)
  ("ext:DocumentContainer" -> _) ; needed to update status on publishing decision/notulen
)

(define-graph org-sign-graph ("http://mu.semte.ch/graphs/organizations/")
  ("nfo:FileDataObject" -> _)
  ("besluitvorming:Agenda" -> _)
  ("ext:VersionedAgenda" -> _)
  ("ext:VersionedNotulen" -> _)
  ("ext:VersionedBesluitenLijst" -> _)
  ("ext:VersionedBehandeling" -> _)
  ("sign:SignedResource" -> _)
  ("task:Task" -> _)
  ("oslc:Error" -> _)
  ("ext:PublishingLog" -> _)
)

(define-graph lmb-public-graph ("http://mu.semte.ch/graphs/lmb-data-public")
  ("mandaat:Mandataris" -> _)
  ("mandaat:Fractie" -> _)
  ("org:Membership" -> _)
  ("person:Person" -> _)
)

(define-graph lmb-private-graph ("http://mu.semte.ch/graphs/lmb-data-private/")
  ("person:Person" -> _)
  ("persoon:Geboorte" -> _)
  ("adms:Identifier" -> _)
)

(define-graph awv-ldes-graph ("http://mu.semte.ch/graphs/awv/ldes")
  ("onderdeel:WordtAangeduidDoor" -> _)
  ("mobiliteit:VerkeersbordVerkeersteken" -> _)
  ("onderdeel:Realiseert" -> _)
  ("onderdeel:BevatMaatregelOntwerp" -> _)
  ("onderdeel:IsGebaseerdOp" -> _)
  ("svariables:VariableInstanceWithLiteralValue" -> _)
  ("onderdeel:HoortBij" -> _)
  ("onderdeel:HeeftVerkeersteken" -> _)
  ("installatie:AanzichtVerkeersbordopstelling" -> _)
  ("onderdeel:HeeftBetrokkene" -> _)
  ("mobiliteit:SignalisatieOntwerp" -> _)
  ("onderdeel:HeeftAanzicht" -> _)
  ("installatie:Verkeersbordopstelling" -> _)
  ("onderdeel:HeeftOntwerp" -> _)
  ("onderdeel:RetroreflecterendVerkeersbord" -> _)
  ("onderdeel:BevatVerkeersteken" -> _)
  ("onderdeel:IsOntwerpVan" -> _)
  ("mobiliteit:MobiliteitsmaatregelOntwerp" -> _)
  ("mobiliteit:OntwerpVerkeersteken" -> _)
  ("onderdeel:RetroreflecterendeFolie" -> _)
  ("onderdeel:Bevestiging" -> _)
  ("mobiliteit:AanvullendReglementOntwerp" -> _)
  ("onderdeel:HeeftWaardeVoor" -> _)
)

(define-graph error-log-graph ("http://mu.semte.ch/graphs/login-error-logs")
  ("rlog:Entry" -> _)
)

(define-graph reports-graph ("http://mu.semte.ch/graphs/reports")
  ; For logs, reports (dashboard and report service)
  ("rlog:Entry" -> _)
  ("rlog:Level" -> _)
  ("rlog:StatusCode" -> _)
  ("ext:LogSource" -> _)
  ("reporting:Report" -> _)
  ("cogs:Job" -> _)
  ("oslc:Error" -> _)
  ("nfo:DataContainer" -> _)
  ("nfo:FileDataObject" -> _)
  ("nfo:DataContainer" -> _)
)

(supply-allowed-group "org-read"
  :parameters ("session_group")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole ?role.
             FILTER(?role in (\"GelinktNotuleren-lezer\",\"GelinktNotuleren-schrijver\", \"GelinktNotuleren-publiceerder\",  \"GelinktNotuleren-ondertekenaar\"))
          }"
)

; Same query as org-read but without the session_group param
(supply-allowed-group "read"
  :parameters ()
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT * WHERE {
            <SESSION_ID> ext:sessionRole ?role.
             FILTER(?role in (\"GelinktNotuleren-lezer\",\"GelinktNotuleren-schrijver\", \"GelinktNotuleren-publiceerder\",  \"GelinktNotuleren-ondertekenaar\"))
          }"
)

(supply-allowed-group "org-write"
  :parameters ("session_group")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole \"GelinktNotuleren-schrijver\".
          }"
)

; Same query as org-write but without the session_group param
(supply-allowed-group "write"
  :parameters ()
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT * WHERE {
            <SESSION_ID> ext:sessionRole \"GelinktNotuleren-schrijver\".
          }"
)

(supply-allowed-group "org-pub"
  :parameters ("session_group")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole \"GelinktNotuleren-publiceerder\".
          }"
)

(supply-allowed-group "org-sign"
  :parameters ("session_group")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole \"GelinktNotuleren-ondertekenaar\".
          }"
)

(supply-allowed-group "reports"
  :parameters ()
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          SELECT DISTINCT * WHERE {
            <SESSION_ID> session:account/ext:sessionRole \"GelinktNotuleren-report-admin\".
          }"
)

(grant (read)
       :to-graph session-graph
       :for-allowed-group "read")

(grant (read)
       :to-graph org-read-graph
       :for-allowed-group "org-read")

(grant (read)
       :to-graph lmb-private-graph
       :for-allowed-group "org-read")

(grant (write)
       :to-graph org-write-graph
       :for-allowed-group "org-write")

(grant (write)
       :to-graph org-pub-graph
       :for-allowed-group "org-pub")

(grant (write)
       :to-graph org-sign-graph
       :for-allowed-group "org-sign")

(grant (read)
       :to-graph reports-graph
       :for-allowed-group "reports")

(grant (read)
       :to-graph error-log-graph
       :for-allowed-group "reports")

(grant (read)
       :to-graph awv-ldes-graph
       :for-allowed-group "write")

(supply-allowed-group "public")

(grant (read)
       :to-graph public-graph
       :for-allowed-group "public")

(grant (read)
       :to-graph lmb-public-graph
       :for-allowed-group "public")
       
(grant (write)
       :to-graph error-log-graph
       :for-allowed-group "public")

(in-package :support)
;; setting the string-max-size to nil assures us that strings will never be converted to files
(defparameter *string-max-size* nil
  "Maximum size of a string before it gets converted.")
