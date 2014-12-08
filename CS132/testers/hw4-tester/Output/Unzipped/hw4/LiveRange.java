import java.util.Comparator;

public class LiveRange {
	
	public int begin;
	public int end;
	String variableName;
	
	public LiveRange(String variableName, int begin, int end) {
		super();
		this.variableName = variableName;
		this.begin = begin;
		this.end = end;
	}
	
	public static Comparator<LiveRange> RangeComparator = new Comparator<LiveRange>() {
		public int compare(LiveRange r1, LiveRange r2)
		{
			int ret = Integer.compare(r1.begin, r2.begin);
			if (ret == 0)
				ret = Integer.compare(r1.end, r2.end);
			
			return ret;
		}
	};
}
