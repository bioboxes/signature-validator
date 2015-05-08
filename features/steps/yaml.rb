def test_entries(table, document)
  table.hashes.each do |row|
    document[row['key']].to_s.should eq(row['value'])
  end
end

Then(/^the stdout should be valid YAML$/) do
  require 'yaml'
  @document = YAML.load(all_stdout)
end

Then(/^the YAML document should have the entries:$/) do |table|
  test_entries(table, @document)
end

Then(/^the YAML document entry "([^"]*)" should have the entries:$/) do |entry_path, table|
  entry = entry_path.split('.').inject(@document) do |doc, key|
    expect(doc).to include(key)
    doc[key]
  end
  test_entries(table, entry)
end
