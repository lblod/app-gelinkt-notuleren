## Linking published decisions to Aanvullend Reglement designs

It is intended that after an Aanvullend Reglement (AR) design is inserted into a decision, and that
decision is published, that the document contains sufficient information to determine which design
has been published.
The intent is to allow the VKS design tool to indicate which designs have been used in a
publication, and so have been 'approved'.

The relationships between resources follow the [Besluit Mobiliteit
model](https://data.test-vlaanderen.be/doc/applicatieprofiel/besluit-mobiliteit/ontwerpstandaard/toolchain4).

### Method

In order to illustrate how these published AR designs as well as the `Mobiliteitsmaatregel` designs
and the 'instantiated' (given a URI) `Mobiliteitsmaatregel`s can be extracted from the data, we
assume the step of parsing the published RDFa documents and inserting the extracted information into
a triplestore.
We therefore provide the following SPARQL query to be run against this data:

```sparql
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX eli: <http://data.europa.eu/eli/ontology#>
PREFIX mobiliteit: <https://data.vlaanderen.be/ns/mobiliteit#>
PREFIX onderdeel: <https://wegenenverkeer.data.vlaanderen.be/ns/onderdeel#>
PREFIX implementatieelement: <https://wegenenverkeer.data.vlaanderen.be/ns/implementatieelement#>

SELECT (?arOnt AS ?AanvullendReglementOntwerp) (?mOnt AS ?MobiliteitsmaatregelOntwerp) (?mmr AS ?Mobiliteitsmaatregel) WHERE {
  ?sub a besluit:Besluit ;
       # Note: the Besluit Mobiliteit model uses dct:isPartOf in the other direction for this, but
       #   we use eli:has_part as per the Besluit Publicatie model
       eli:has_part ?art .
  ?art a besluit:Artikel ;
       mobiliteit:heeftMobiliteitsmaatregel ?mmr .
  ?mmr a mobiliteit:Mobiliteitsmaatregel ;
       mobiliteit:isGebaseerdOpMaatregelOntwerp ?mOnt .
  ?mOnt a mobiliteit:MobiliteitsmaatregelOntwerp;
        ^implementatieelement:RelatieObject.doel / implementatieelement:RelatieObject.bron ?arOnt .
  ?arOnt a mobiliteit:AanvullendReglementOntwerp .
}
```
