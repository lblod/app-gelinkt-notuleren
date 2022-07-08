#!/usr/bin/env ruby
require 'bundler/inline'
require 'yaml'
require 'securerandom'
require 'date'

$stdout.sync = true
print "installing dependencies..."
gemfile do
  source 'https://rubygems.org'
  gem 'bcrypt'
end
puts "done"

salt = ""
begin
  File.open("/project/docker-compose.override.yml") do |file|
    config = YAML.load(file)
    salt = config.dig("services", "dashboard-login", "environment", "MU_APPLICATION_SALT")
    throw "invalid salt" unless salt
   end
rescue
  puts "Tried to locate login salt in docker-compose.override.yml, but couldn't find it."
  puts "Please make sure a salt is configured:"
  puts <<EOF
  login:
    restart: "no"
    environment:
      MU_APPLICATION_SALT: "#{SecureRandom.hex}"
EOF
end

puts "this script will generate a new user account"
print "username: "
STDOUT.flush
account_name = STDIN.gets.chomp

print "password: "
STDOUT.flush
password = STDIN.gets.chomp

account_salt = SecureRandom.hex
person_uuid = SecureRandom.uuid
account_uuid = SecureRandom.uuid
domain = "mow.data.gift"
hashed_password = BCrypt::Password.create(password + salt + account_salt)
now = DateTime.now.xmlschema
query = %(
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX people: <http://#{domain}/people/>
PREFIX accounts: <http://#{domain}/accounts/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX mu:      <http://mu.semte.ch/vocabularies/core/>
PREFIX ext:      <http://mu.semte.ch/vocabularies/ext/>
PREFIX account: <http://mu.semte.ch/vocabularies/account/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
INSERT DATA {
   GRAPH <http://mu.semte.ch/graphs/users> {
     people:#{person_uuid} a foaf:Person ;
                   foaf:name "#{account_name}";
                   foaf:account accounts:#{account_uuid};
                   mu:uuid "#{person_uuid}";
                   dcterms:created "#{now}"^^xsd:datetime;
                   dcterms:modified "#{now}"^^xsd:datetime.
     accounts:#{account_uuid} a foaf:OnlineAccount;
                                                   foaf:accountName "#{account_name}";
                                                   mu:uuid "#{account_uuid}";
                                                   account:password """#{hashed_password}""";
                                                   account:salt "#{account_salt}";
                                                   account:status <http://mu.semte.ch/vocabularies/account/status/active>;
                                                   dcterms:created "#{now}"^^xsd:datetime;
                                                   ext:sessionRole "GelinktNotuleren-report-admin";
                                                   dcterms:modified "#{now}"^^xsd:datetime.
}}
)

timestamp = DateTime.now.strftime("%Y%m%d%H%M%L")
filename = "#{timestamp}-create-user-#{account_name.gsub(" ", "-")}.sparql"
File.write("/project/config/migrations/#{filename}", query)
puts "generated migration #{filename} for #{account_name.inspect} : #{password.inspect}"

