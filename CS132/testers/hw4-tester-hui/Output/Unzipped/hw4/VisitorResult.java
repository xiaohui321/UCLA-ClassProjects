import java.util.TreeSet;


public class VisitorResult {
	public final int line;
	public TreeSet<Integer> nextLines;
	public TreeSet<Integer> prevLines = new TreeSet<Integer>();
	public VisitorResult(int line_num, TreeSet<Integer> next_lines){
		line = line_num;
		nextLines = next_lines;
	}
	
}
