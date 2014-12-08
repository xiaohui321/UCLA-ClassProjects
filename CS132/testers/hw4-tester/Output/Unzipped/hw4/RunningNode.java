import java.util.Set;


public class RunningNode {
	public int lineNumber;
	public Set<Integer> nextLines;
	public Set<Integer> prevLines;
	
	
	public RunningNode(int lineNumber, Set<Integer> nextLines,
			Set<Integer> prevLines) {
		super();
		this.lineNumber = lineNumber;
		this.nextLines = nextLines;
		this.prevLines = prevLines;
	}
}
