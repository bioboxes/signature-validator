Feature: Validate a biobox signature
  In order for developers to validate biobox input files
  The validate-biobox-signature can be used to generate a schema
  So that the developer can test the biobox.yml file

  Scenario: Parsing a simple signature
    When I run the bash command:
      """
      ${BINARY}
      """
    Then the stderr should not contain anything
     And the exit status should be 0
     And the stdout should be valid YAML
     And the YAML document should have the entries:
       | key                  | value                                   |
       | $schema              | http://json-schema.org/draft-04/schema# |
       | additionalProperties | false                                   |
       | type                 | object                                  |
     And the YAML document entry "properties.version" should have the entries:
       | key     | value      |
       | type    | string     |
       | pattern | ^0.9.\\d+$ |
