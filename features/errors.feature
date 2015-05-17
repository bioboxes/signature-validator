Feature: Returning error messages

  Scenario: An invalid schema is specified
    When I run the bash command:
      """
      ${BINARY} --signature "Fastq A -> Fastq A" --schema=error
      """
    Then the stdout should not contain anything
     And the stderr should contain:
      """
      Error: unknown schema type "error"
      """

  Scenario: An invalid signature is specified
    When I run the bash command:
      """
      ${BINARY} --signature "Fastq A / Fastq A" --schema=input
      """
    Then the stdout should not contain anything
     And the stderr should contain:
      """
      Error parsing biobox signature" (line 1, column 9):
      unexpected "/"
      expecting space or "->"
      """

