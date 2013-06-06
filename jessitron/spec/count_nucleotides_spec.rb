require_relative '../lib/count_nucleotides'

describe CountNucleotides do
  subject { CountNucleotides.new }

  describe 'counting sequences' do
    it 'counts all As' do
      subject.count("AAAAA").should == { "A" => 5 }
    end

    it 'counts four bases' do
      subject.count("AATTCCGCAA").should == { "A" => 4, "T" => 2, "C" => 3, "G" => 1}
    end

    it 'counts other letters too' do
      subject.count("Jess").should == { "J" => 1, "e" => 1, "s" => 2}
    end

  end
end
