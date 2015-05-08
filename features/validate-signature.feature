Feature: Validate a biobox signature
  In order for developers to validate biobox input files
  The validate-biobox-signature can be used to generate a schema
  So that the developer can test the biobox.yml file

  Scenario: Parsing a simple signature
    When I run the bash command:
      """
      ${BINARY}
      """
     Then the exit status should be 0
