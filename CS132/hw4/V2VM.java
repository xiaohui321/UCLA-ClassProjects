import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;

import cs132.util.ProblemException;
import cs132.vapor.ast.VBuiltIn.Op;
import cs132.vapor.ast.VDataSegment;
import cs132.vapor.ast.VFunction;
import cs132.vapor.ast.VLabelRef;
import cs132.vapor.ast.VOperand;
import cs132.vapor.ast.VaporProgram;
import cs132.vapor.parser.VaporParser;

public class V2VM{ 
	public static void main(String [] args) {
		File testDir = new File("../hw4-tester/tests"); // create a folder called tests, and put the tests in it
        for (final File fileEntry : testDir.listFiles()) {
            if (fileEntry.isFile()) {
                FileInputStream in = null;
                try {
                    in = new FileInputStream(fileEntry.getAbsoluteFile());
                }
                catch (FileNotFoundException err) {
                    err.printStackTrace();
                }
                try {
                    System.out.println("\n\nProcessing: " + fileEntry.getName() + "\n-----------------------\n");
                	VaporProgram vp = parseVapor(in,System.err);
                	String result = processDataSegmentResult(vp.dataSegments);
                	for(VFunction fun : vp.functions){
                		VaporToVaporMCompiler c = new VaporToVaporMCompiler(fun);
                		result += c.result;
                	}
                		
                	System.out.println(result);
                	System.out.println("==========================\n");
                } catch (Exception e) {
                	e.printStackTrace();
                }
            }
        }
	 }
	
	public static VaporProgram parseVapor(InputStream in, PrintStream err) throws IOException{
		Op[] ops = {
				Op.Add, Op.Sub, Op.MulS, Op.Eq, Op.Lt, Op.LtS,
				Op.PrintIntS, Op.HeapAllocZ, Op.Error,
		};
		boolean allowLocals = true;
		String[] registers = null;
		boolean allowStack = false;

		VaporProgram program;
		try {
			program = VaporParser.run(new InputStreamReader(in), 1, 1,
					java.util.Arrays.asList(ops),
					allowLocals, registers, allowStack);
		}
		catch (ProblemException ex) {
			err.println(ex.getMessage());
			return null;
		}

		return program;
	}
	
	private static String processDataSegmentResult(VDataSegment[] dataSegments) {
		String result = "";
		for(VDataSegment v: dataSegments){
			result += (v.mutable? "var ": "const ") + v.ident + "\n";
			for(VOperand.Static  op : v.values){
				result += "\t :" + ((VLabelRef) op).ident + "\n";
			}
			result += "\n";
		}
		return result + "\n";
	}
}