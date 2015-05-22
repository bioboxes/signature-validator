Feature: Validate a biobox file from the signature

  Scenario: Validating a simple biobox file
   Given an empty file named "reads.fq.gz"
     And I successfully run the bash command:
      """
      ${BINARY} \
        --signature "Fastq A -> Fastq A" \
        --schema=input \
        > schema.yml
      """
     And a file named "biobox.yml" with:
      """
      version: 0.9.0
      arguments:
        - fastq:
            id: "pe"
            value: "reads.fq.gz"
            type: "single"
      """
    When I run the bash command:
      """
      validate-biobox-file \
        --schema schema.yml \
        --input biobox.yml \
      """
    Then the stderr should not contain anything
     And the exit status should be 0



  Scenario: Validating an insert signature
   Given an empty file named "reads.fq.gz"
     And I successfully run the bash command:
      """
      ${BINARY} \
        --signature "Insert_size A -> Fastq A" \
        --schema=input \
        > schema.yml
      """
     And a file named "biobox.yml" with:
      """
      version: 0.9.0
      arguments:
        - insert_size:
            id: "pe"
            value: 250
            type: insert_size
      """
    When I run the bash command:
      """
      validate-biobox-file \
        --schema schema.yml \
        --input biobox.yml \
      """
    Then the stderr should not contain anything
     And the exit status should be 0
