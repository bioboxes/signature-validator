Feature: Validate a biobox signature
  In order for developers to validate biobox input files
  The validate-biobox-signature can be used to generate a schema
  So that the developer can test the biobox.yml file

  Scenario Outline: Using different command line parameters
    When I run the bash command:
      """
      ${BINARY} <signature> "Fastq A -> Fastq A" <schema> <type>
      """
    Then the stderr should not contain anything
     And the exit status should be 0

    Examples:
      | signature   | schema   | type   |
      | --signature | --schema | input  |
      | --signature | --schema | output |
      | -s          | -e       | input  |

  Scenario: An invalid schema is specified
    When I run the bash command:
      """
      ${BINARY} --signature "Fastq A -> Fastq A" --schema=error
      """
    Then the stdout should not contain anything
    Then the stderr should contain:
      """
      Error: unknown schema type "error"
      """

  Scenario: Parsing a simple signature
    When I run the bash command:
      """
      ${BINARY} --signature "Fastq A -> Fastq A" --schema=output
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
