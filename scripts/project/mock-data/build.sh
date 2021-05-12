#!/usr/bin/env ruby
require 'bundler/inline'
require 'yaml'
require 'securerandom'
$stdout.sync = true
puts
puts "GN factory for mock data."

print "installing dependencies..."
gemfile do
  source 'https://rubygems.org'
  gem 'linkeddata'
end
puts "done"

class String
  def sparql_escape
    '"""' + self.gsub(/[\\"]/) { |s| '\\' + s } + '"""'
  end
end

STATUS_GEAGENDEERD_URI="http://mu.semte.ch/application/concept-schemes/082b89ce4a8e42678689e9b35c6479d7"
STATUS_CONCEPT_URI="http://mu.semte.ch/application/concepts/a1974d071e6a47b69b85313ebdcef9f7"
AGENDAPOINTS_FOLDER="http://mu.semte.ch/application/editor-document-folder/ae5feaed-7b70-4533-9417-10fbbc480a4c"

class EditorDocument
  attr_reader :status, :publisher
  def initialize(agendapoint, publisher, status)
    @status = status
    @agendapoint = agendapoint
    @publisher = publisher
  end

  def container_uuid
    @container_uuid ||= SecureRandom.uuid
  end

  def document_uuid
    @document_uuid ||= SecureRandom.uuid
  end

  def to_ntriples
    now = DateTime.now.to_s
    context = "{\"vocab\":\"http://data.vlaanderen.be/ns/besluit#\",\"prefix\":{\"eli\":\"http://data.europa.eu/eli/ontology#\",\"prov\":\"http://www.w3.org/ns/prov#\",\"mandaat\":\"http://data.vlaanderen.be/ns/mandaat#\",\"besluit\":\"http://data.vlaanderen.be/ns/besluit#\",\"ext\":\"http://mu.semte.ch/vocabularies/ext/\",\"person\":\"http://www.w3.org/ns/person#\",\"persoon\":\"http://data.vlaanderen.be/ns/persoon#\",\"dateplugin\":\"http://say.data.gift/manipulators/insertion/\",\"besluittype\":\"https://data.vlaanderen.be/id/concept/BesluitType/\",\"dct\":\"http://purl.org/dc/terms/\",\"mobiliteit\":\"https://data.vlaanderen.be/ns/mobiliteit#\",\"lblodmow\":\"http://data.lblod.info/vocabularies/mobiliteit/\"}}"
<<EOF
         <http://mockdata.example.com/containers/#{container_uuid}> a <http://mu.semte.ch/vocabularies/ext/DocumentContainer>;
                        <http://mu.semte.ch/vocabularies/ext/editorDocumentStatus>	<#{status}> ;
                        <http://mu.semte.ch/vocabularies/ext/editorDocumentFolder>	<#{AGENDAPOINTS_FOLDER}> ;
                      	<http://purl.org/pav/hasCurrentVersion> <http://mockdata.example.com/editor-documents/#{document_uuid}>;
                        <http://purl.org/pav/hasVersion> <http://mockdata.example.com/editor-documents/#{document_uuid}>;
                        <http://mu.semte.ch/vocabularies/core/uuid>	"#{container_uuid}";
                        <http://purl.org/dc/terms/publisher>	<#{publisher}> .
         <http://mockdata.example.com/editor-documents/#{document_uuid}> a <http://mu.semte.ch/vocabularies/ext/EditorDocument>;
                                                                      	<http://mu.semte.ch/vocabularies/ext/editorDocumentContext>	#{context.sparql_escape} ;
                                                                        <http://purl.org/dc/terms/title>	"#{@agendapoint["title"]} - generated #{DateTime.now.strftime("%Y-%m-%d %H %M")}" ;
                                                                      	<http://mu.semte.ch/vocabularies/ext/editorDocumentContent> #{content.sparql_escape};
                                                                        <http://mu.semte.ch/vocabularies/core/uuid>	"#{document_uuid}" ;
                                                                        <http://purl.org/pav/createdOn> "#{now}"^^xsd:dateTime;
                                                                        <http://purl.org/pav/lastUpdateOn> "#{now}"^^xsd:dateTime.
EOF
  end

  def articles_as_html
    articles = @agendapoint.dig("decision", "articles") || []
    output = []
    articles.each_with_index do |item, index|
      output << <<EOF
    <div property="eli:has_part" resource="http://mockdata.example.com/articles/#{SecureRandom.uuid}}" typeof="besluit:Artikel">
      <div property="eli:number" datatype="xsd:string">Artikel #{index+1}</div>
      <span style="display:none;" property="eli:language" resource="http://publications.europa.eu/resource/authority/language/NLD" typeof="skos:Concept">&nbsp;</span>
      <div property="prov:value" datatype="xsd:string">
          #{item}
      </div>
    </div>
EOF
    end
    output.join("\n")
  end

  def authority_as_html
    authority = @agendapoint.dig("decision", "motivation", "authority") || []
    if authority.length > 0
      <<EOF
 <h5>Bevoegdheid</h5>
    <ul class="bullet-list">
    #{authority.map{ |c| "<li>#{c}</li>" }.join("\n")}
    </ul>
    <br>
EOF
    else
      ""
    end
  end
  def citations_as_html
    citations = @agendapoint.dig("decision", "motivation", "citations") || []
    if citations.length > 0
      <<EOF
    <h5>Juridische context</h5>
    <ul class="bullet-list">
    #{citations.map{ |c| "<li>#{c}</li>" }.join("\n")}
    </ul>
EOF
    else
      ""
    end
  end

  def context_as_html
    context = @agendapoint.dig("decision", "motivation", "context") || []
    if context.length > 0
      <<EOF
      <h5>Feitelijke context en argumentatie</h5>
    <ul class="bullet-list">
      #{context.map{ |c| "<li>#{c}</li>" }.join("\n")}
    </ul>
EOF
    else
      ""
    end
  end

  def decision_as_html
    if @agendapoint["decision"]
      <<EOF
    <div property="prov:generated" resource="http://mockdata.example.com/decisions/#{SecureRandom.uuid}" typeof="besluit:Besluit">
      <h4 class="h4" property="eli:title" datatype="xsd:string">#{@agendapoint["decision"]["title"]}</h4>
      <div property="besluit:motivering" lang="nl">
       #{authority_as_html}
       <br>
       #{citations_as_html}
       <br>
       #{context_as_html}
      </div>
      <h5>Beslissing</h5>
      <div property="prov:value" datatype="xsd:string">
        #{articles_as_html}
      </div>
  </div>
EOF
    else
      ""
    end
  end
  def content
    <<EOF
      #{@agendapoint["freetext"]}
      #{decision_as_html}
EOF
  end
end

def generate(target_graph)
  print "loading data templates..."
  agendapoints = YAML.load_file("./agendapoints.yml")
  puts "loaded #{agendapoints.length} agendapoints"
  print "generating documents..."
  agendapoints.each do |ap|
    doc = EditorDocument.new(ap, "http://data.lblod.info/id/bestuurseenheden/974816591f269bb7d74aa1720922651529f3d3b2a787f5c60b73e5a0384950a4", STATUS_CONCEPT_URI)

    begin
      data_query = %(
    INSERT DATA {
      GRAPH <#{target_graph}> {
        #{doc.to_ntriples}
      }
    }
   )
      result = client.query(data_query)
    rescue Exception => e
      puts e
    end
    puts doc.container_uuid
  end
  puts "done"
end


def until_valid(question, options = nil, &block)
  if options
    puts question
    options.each do |option|
      puts "[#{option[:id]}] #{option[:name]}"
    end
    print "please specify your choice: "
  else
    print "#{question}: "
  end
  STDOUT.flush
  klass_input = STDIN.gets.chomp
  verified = block.call(klass_input)
  if verified
    return klass_input
  else
    puts "invalid input"
    until_valid(question, options, &block)
  end
end

def client
  @client ||= SPARQL::Client.new("http://virtuoso:8890/sparql")
end

def fetch_unit_uris(name)
  client.query("SELECT distinct ?uri WHERE { ?uri a <http://data.vlaanderen.be/ns/besluit#Bestuurseenheid>; <http://www.w3.org/2004/02/skos/core#prefLabel> #{name.sparql_escape}}")
end

def fetch_graph_and_type(unit)
  client.query(%(
PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
select distinct ?type ?graph WHERE  {
<#{unit}> a besluit:Bestuurseenheid; mu:uuid ?uuid;
besluit:classificatie/skos:prefLabel ?type.
    BIND(IRI(CONCAT("http://mu.semte.ch/graphs/organizations/", ?uuid)) as ?graph)
} LIMIT 100
))[0]
end

name = until_valid("Name of administrative unit to generate data for") do |input|
    fetch_unit_uris(input).length > 0
end

units = fetch_unit_uris(name)
graphs = []
units.each do |unit|
  graphs << fetch_graph_and_type(unit[:uri])
end

if (units.length > 1)
  puts "found 2 administrative units matching your query"
  graphs.each_with_index do |result, index|
    puts "#{index} #{result[:graph]} #{result[:type]}"
  end
  pref_index = until_valid("Select unit") do |input|
    input.to_i >= 0 && input.to_i < graphs.length
  end
  target_graph=graphs[pref_index.to_i][:graph]
else
  target_graph=units[0][:graph]
end
puts "generating data in #{target_graph}"
generate(target_graph)
puts "data generated, don't forget to restart cache and resources"
