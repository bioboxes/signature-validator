Feature: Validate different biobox signatures

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



  Scenario Outline: Parsing different biobox signature terms
    When I run the bash command:
      """
      ${BINARY} --signature "[<term> A] -> <term> A" --schema=input
      """
    Then the stderr should not contain anything
     And the exit status should be 0
     And the stdout should be valid YAML

    Examples:
      | term        |
      | Fasta       |
      | Fastq       |
      | Insert_size |



  Scenario Outline: Parsing a simple signature
    When I run the bash command:
      """
      ${BINARY} --signature "Fastq A -> Fastq A" --schema=<schema>
      """
    Then the stderr should not contain anything
     And the exit status should be 0
     And the stdout should be valid YAML
     And the YAML document should have the key-values:
       | key                  | value                                   |
       | $schema              | http://json-schema.org/draft-04/schema# |
       | additionalProperties | false                                   |
       | type                 | object                                  |
     And the YAML document entry "required" should have the items:
       | item      |
       | version   |
       | arguments |
     And the YAML document entry "properties.version" should have the key-values:
       | key     | value      |
       | type    | string     |
       | pattern | ^0.9.\\d+$ |
     And the YAML document entry "properties.arguments" should have the key-values:
       | key             | value |
       | type            | array |
       | additionalItems | false |
     And the YAML document entry "properties.arguments.items.[0]" should have the key-values:
       | key                  | value  |
       | type                 | object |
       | additionalProperties | false  |
     And the YAML document entry "properties.arguments.items.[0].required" should have the items:
       | item  |
       | fastq |
     And the YAML document entry "properties.arguments.items.[0].properties.fastq" should have the key-values:
       | key  | value               |
       | $ref | #/definitions/str_var |
     And the YAML document entry "definitions.str_var" should have the key-values:
       | key                  | value  |
       | type                 | object |
       | additionalProperties | false  |
     And the YAML document entry "definitions.str_var.properties.id" should have the key-values:
       | key  | value  |
       | type | string |
     And the YAML document entry "definitions.str_var.properties.value" should have the key-values:
       | key  | value  |
       | type | string |
     And the YAML document entry "definitions.str_var.properties.type" should have the key-values:
       | key  | value  |
       | type | string |
     And the YAML document entry "definitions.str_var.required" should have the items:
       | item  |
       | id    |
       | value |
       | type  |

    Examples:
      | schema |
      | input  |
      | output |


  Scenario: Parsing a signature with two arguments
    When I run the bash command:
      """
      ${BINARY} --signature "Fastq A, Fastq A -> Fastq A" --schema=input
      """
    Then the stderr should not contain anything
     And the exit status should be 0
     And the stdout should be valid YAML
     And the YAML document entry "properties.arguments.items.[0]" should have the key-values:
       | key                  | value  |
       | type                 | object |
       | additionalProperties | false  |
     And the YAML document entry "properties.arguments.items.[0].required" should have the items:
       | item  |
       | fastq |
     And the YAML document entry "properties.arguments.items.[0].properties.fastq" should have the key-values:
       | key  | value               |
       | $ref | #/definitions/str_var |
     And the YAML document entry "properties.arguments.items.[1].required" should have the items:
       | item  |
       | fastq |
     And the YAML document entry "properties.arguments.items.[1].properties.fastq" should have the key-values:
       | key  | value               |
       | $ref | #/definitions/str_var |


  Scenario: Parsing an signature with Insert_size
    When I run the bash command:
      """
      ${BINARY} --signature "Insert_size A -> Fastq A" --schema=input
      """
    Then the stderr should not contain anything
     And the exit status should be 0
     And the stdout should be valid YAML
     And the YAML document entry "properties.arguments.items.[0].required" should have the items:
       | item  |
       | insert_size |
     And the YAML document entry "properties.arguments.items.[0].properties.insert_size" should have the key-values:
       | key  | value               |
       | $ref | #/definitions/num_var |
     And the YAML document entry "definitions.num_var.properties.value" should have the key-values:
       | key  | value  |
       | type | number |
