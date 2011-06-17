
describe "running drive.rb" do
  def put_and_get(move)
    drive = Kernel.open('| ruby drive.rb', 'r+')
    drive.puts(move)
    dir = drive.readline.chomp
    slot = drive.readline.chomp
    card = drive.readline.chomp
    return [dir, slot, card]
  end

  it "should translate concise to expanded" do
    put_and_get("0 S").should == ['2','0','S']
    put_and_get("S 0").should == ['1','0','S']
    put_and_get("[0]S").should == ['2','0','S']
    put_and_get("S[0]").should == ['1','0','S']
  end
end
