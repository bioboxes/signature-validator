def test_entries(table, document)
  table.hashes.each do |row|
    document[row['key']].to_s.should eq(row['value'])
  end
end

def fetch_entry(entry_path, document)
  entry_path.split('.').inject(document) do |doc, key|
    if match = /\[(\d)\]/.match(key)
      key = match[1].to_i
      expect(doc.length).to be > key
    else
      expect(doc).to include(key)
    end

    doc[key]
  end
end



Then(/^the stdout should be valid YAML$/) do
  require 'yaml'
  @document = YAML.load(all_stdout)
end

Then(/^the YAML document entry "([^"]*)" should have the items:$/) do |entry_path, table|
  entry = fetch_entry(entry_path, @document)
  table.hashes.each do |row|
    entry.should include(row['item'])
  end
end

Then(/^the YAML document should have the key\-values:$/) do |table|
  test_entries(table, @document)
end

Then(/^the YAML document entry "([^"]*)" should have the key\-values:$/) do |entry_path, table|
  test_entries table, fetch_entry(entry_path, @document)
end
