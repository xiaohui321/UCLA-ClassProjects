import java.util.Comparator;


public class Variable {
	public String name;
	public int begin;
	public int end;
	
	public Variable(String name, int begin, int end){
		this.name = name;
		this.begin = begin;
		this.end = end;
	}
	
	public static Comparator<Variable> liveRangeComparator = new Comparator<Variable>() {
		public int compare(Variable v1, Variable v2){
			if(v1.begin != v2.begin) return v1.begin - v2.begin;
			return v1.end - v2.end;
		}
	};
}
