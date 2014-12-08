import cs132.util.ProblemException;
import cs132.vapor.parser.VaporParser;
import cs132.vapor.ast.VDataSegment;
import cs132.vapor.ast.VFunction;
import cs132.vapor.ast.VInstr;
import cs132.vapor.ast.VOperand;
import cs132.vapor.ast.VVarRef.Local;
import cs132.vapor.ast.VaporProgram;
import cs132.vapor.ast.VBuiltIn.Op;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

public class V2VM {
	
	private static String[] t_registers = {"$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8"};
	private static String[] s_registers = {"$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"};
	public static void main(String args[])
	{
		InputStream inputStream;
		try {
			inputStream = System.in;
			PrintStream errorStream = System.err;
			VaporProgram program = parseVapor(inputStream, errorStream);
			
			String data = Write_DATA(program.dataSegments);
			String text = Write_VMFUN(program.functions, null);
			
			System.out.print(data);
			System.out.print(text);
		} catch (FileNotFoundException e) {
			
			e.printStackTrace();
		} catch (IOException e) {
			
			e.printStackTrace();
		}
	}
	
	
	
	
	
	public static VaporProgram parseVapor(InputStream in, PrintStream err) throws IOException
	{
		Op[] ops = {Op.Add, Op.Sub, Op.MulS, Op.Eq, Op.Lt, Op.LtS,
				Op.PrintIntS, Op.HeapAllocZ, Op.Error};
		boolean allowLocals = true;
		String[] registers = null;
		boolean allowStack = false;
		
		VaporProgram program;
		try {
			program = VaporParser.run(new InputStreamReader(in), 1, 1,
					java.util.Arrays.asList(ops),
					allowLocals, registers, allowStack);
			} catch (ProblemException ex) {
				err.println(ex.getMessage());
				return null;
			}
		
		return program;
	}
	
	
	
private static String Write_VMFUN(VFunction[] functions, FileOutputStream regAllocStream) {
		
		RunningVisitor runningVisitor = new RunningVisitor();
		V_Visitor secondPass = new V_Visitor();
		String code = "";
		
		try {
			for (VFunction function : functions)
			{
				Map<Integer, VInstr> lineInstructionMap = new HashMap<Integer, VInstr>();
				for (VInstr instruction : function.body)
				{
					lineInstructionMap.put(instruction.sourcePos.line, instruction);
				}
				
				Map<Integer, RunningNode> flowNodeMap = new HashMap<Integer, RunningNode>();
				Map<Integer, Set<Integer>> lineToPrevLinesMap = new HashMap<Integer, Set<Integer>>();
				
				Map<String, SortedSet<Integer>> variableUses = new HashMap<String, SortedSet<Integer>>();
				Map<String, SortedSet<Integer>> variableAssignments = new HashMap<String, SortedSet<Integer>>();
				
				for (Local parameter : function.params)
				{
					RunningInput.updateMap(variableAssignments, parameter.ident, function.sourcePos.line);
				}
				
				RunningInput runningInput = new RunningInput(function.labels, variableUses, variableAssignments);
				for (VInstr instruction : function.body)
				{
					RunningNode node = (RunningNode) instruction.accept(runningInput, runningVisitor);
					flowNodeMap.put(node.lineNumber, node);
					
					for (Integer nextLine : node.nextLines)
					{
						if (lineToPrevLinesMap.containsKey(nextLine))
						{
							Set<Integer> prevLines = lineToPrevLinesMap.get(nextLine);
							Set<Integer> newPrevLines = new HashSet<Integer>(prevLines);
							newPrevLines.add(node.lineNumber);
							lineToPrevLinesMap.put(nextLine, newPrevLines);
						}
						else
						{
							Set<Integer> newPrevLines = new HashSet<Integer>();
							newPrevLines.add(node.lineNumber);
							lineToPrevLinesMap.put(nextLine, newPrevLines);
						}
					}
				}
				
				for (Integer line : flowNodeMap.keySet())
				{
					RunningNode node = flowNodeMap.get(line);
					Set<Integer> prevLines = lineToPrevLinesMap.get(line);
					if (prevLines == null)
					{
						prevLines = new HashSet<Integer>();
						prevLines.add(function.sourcePos.line);
					}
					RunningNode newNode = new RunningNode(line, node.nextLines, prevLines);
					flowNodeMap.put(line, newNode);
				}
				
				Map<String, LiveRange> linearRanges = new HashMap<String, LiveRange>();
				for (String variable : function.vars)
				{
					if (variableUses.containsKey(variable))
					{
						int begin = Integer.MAX_VALUE;
						int end = Integer.MIN_VALUE;
						for (Integer useLine : variableUses.get(variable))
						{
							LiveRange liveRange = findAssignmentForVariableUsage(variable, useLine, variableAssignments.get(variable), flowNodeMap);
							if (liveRange.begin < begin)
								begin = liveRange.begin;
							if (liveRange.end > end)
								end = liveRange.end;
						}
						
						for (Integer assignLine : variableAssignments.get(variable))
						{
							if (assignLine < begin)
								begin = assignLine;
							if (assignLine > end)
								end = assignLine;
						}
						
						LiveRange varRange = new LiveRange(variable, begin, end);
						linearRanges.put(variable, varRange);
					}
				}
				
				int stackIn = 0;
				if (function.params.length > 4)
					stackIn = function.params.length - 4;
				
				Map<String, String> variableToRegister = new HashMap<String, String>();
				int maxConcurrentlyLive = linearScan(linearRanges, runningInput.isLeaf, variableToRegister);
				int spills = 0;
				int calleeSaves = 0;
				if (runningInput.isLeaf && maxConcurrentlyLive > t_registers.length)
				{
					spills = maxConcurrentlyLive - t_registers.length;
				}
				else if (!runningInput.isLeaf)
				{
					calleeSaves = maxConcurrentlyLive;
					if (maxConcurrentlyLive > s_registers.length)
					{
						calleeSaves = s_registers.length;
						spills = maxConcurrentlyLive - s_registers.length;
					}
				}
				int stackLocal = calleeSaves + spills; 
				
				int stackOut = runningInput.largestOut;
				
				code = code + "func " + function.ident + " [in " + String.valueOf(stackIn) + ", out " 
				+ String.valueOf(stackOut) + ", local " + String.valueOf(stackLocal) + "]\n";
				
				
				if (regAllocStream != null)
				{
					String functionHeader = "func " + function.ident + " [in " + String.valueOf(stackIn) + ", out " 
							+ String.valueOf(stackOut) + ", local " + String.valueOf(stackLocal) + "]\n";
					regAllocStream.write(functionHeader.getBytes());
					
					String linearRangesOutput = "";
					for (String variable : linearRanges.keySet())
					{
						LiveRange liveRange = linearRanges.get(variable);
						linearRangesOutput = linearRangesOutput + "  " + variable + ": " + Integer.toString(liveRange.begin) + "-" + Integer.toString(liveRange.end) + "\n";
					}
					
					regAllocStream.write(linearRangesOutput.getBytes());
					regAllocStream.write("\n".getBytes());
				}
				
				
				if (!runningInput.isLeaf)
				{
					for (int regIndex = 0; regIndex < calleeSaves; regIndex++)
					{
						code = code + "  local[" + Integer.toString(regIndex) + "] = $s" + Integer.toString(regIndex) + "\n";
					}
				}
				
				for (String variable : variableToRegister.keySet())
				{
					String reg = variableToRegister.get(variable);
					if (isNumeric(reg))
					{
						reg = "local[" + Integer.toString(calleeSaves + Integer.valueOf(reg)) + "]";
						variableToRegister.put(variable, reg);
					}
				}
				
				for (int argIndex = 0; argIndex < function.params.length && argIndex < 4; argIndex++)
				{
					String parameter = function.params[argIndex].toString();
					String arg = variableToRegister.get(parameter);
					if (arg != null)
					{
						code = code + "  " + arg + " = $a" + Integer.toString(argIndex) + "\n";
					}
				}
				for (int argIndex = 4; argIndex < function.params.length; argIndex++)
				{
					String parameter = function.params[argIndex].toString();
					String arg = variableToRegister.get(parameter);
					
					if (arg != null)
					{
						String argReg = arg;
						String setupArg = "";
						if (arg.startsWith("l"))
						{
							setupArg = "  " + arg + " = $v1\n";
							argReg = "$v1";
						}
						
						code = code + "  " + argReg + " = in[" + Integer.toString(argIndex-4) + "]\n" + setupArg;
					}
				}
				
				Reg_input_pass input2 = new Reg_input_pass(variableToRegister);
				int labelIndex = 0;
				for (VInstr instruction : function.body)
				{
					while (labelIndex < function.labels.length && function.labels[labelIndex].sourcePos.line < instruction.sourcePos.line)
					{
						code = code + function.labels[labelIndex].ident + ":\n";
						labelIndex++;
					}
					
					if (instruction.equals(function.body[function.body.length-1]))
					{
						code = code + instruction.accept(input2, secondPass);
						
						if (!runningInput.isLeaf)
						{
							for (int regIndex = 0; regIndex < calleeSaves; regIndex++)
							{
								code = code + "  $s" + Integer.toString(regIndex) + " = local[" + Integer.toString(regIndex) + "]\n";
							}
						}
						
						code = code + "  ret\n";
					}
					else
						code = code + instruction.accept(input2, secondPass);
				}
				
				code = code + "\n";
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return code;
	}

	private static LiveRange findAssignmentForVariableUsage(String variable, Integer useLine, SortedSet<Integer> assignmentLines, Map<Integer, RunningNode> flowNodeMap) 
	{
		int currentLine = useLine;
		LiveRange liveRange = new LiveRange(variable, currentLine, currentLine);
		Set<Integer> prevLines = flowNodeMap.get(currentLine).prevLines;
		Set<Integer> alreadyVisited = new HashSet<Integer>();
		alreadyVisited.add(currentLine);
		for (Integer prevLine : prevLines)
		{			
			int assignLine = recursivelyFindAssignment(prevLine, assignmentLines, alreadyVisited, flowNodeMap, liveRange);
		}
		return liveRange;
	}

	private static int recursivelyFindAssignment(Integer currentLine,
			SortedSet<Integer> assignmentLines, Set<Integer> alreadyVisited,
			Map<Integer, RunningNode> flowNodeMap, LiveRange liveRange) {
		int assignLine = 0;
		
		if (alreadyVisited.contains(currentLine))
			return 0;
		
		if (currentLine < liveRange.begin)
			liveRange.begin = currentLine;
		if (currentLine > liveRange.end)
			liveRange.end = currentLine;
		
		alreadyVisited.add(currentLine);
		
		if (assignmentLines.contains(currentLine))
			return currentLine;
		
		RunningNode node = flowNodeMap.get(currentLine);
		if (node == null)
			return 0;
		Set<Integer> prevLines = node.prevLines;
		if (prevLines == null)
			System.err.print("something went wrong");
		for (Integer prevLine : prevLines)
		{			
			assignLine = recursivelyFindAssignment(prevLine, assignmentLines, alreadyVisited, flowNodeMap, liveRange);
		}
		return assignLine;
	}

	private static int linearScan(Map<String, LiveRange> variableLives, boolean isLeaf, Map<String, String> variableToRegister) 
	{
		String[] registerArray = null;
		if (isLeaf)
			registerArray = t_registers;
		else
			registerArray = s_registers;
		SortedSet<String> availableRegisters = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
		availableRegisters.addAll(Arrays.asList(registerArray));
		
		int maxVariablesConcurrentlyLive = 0;
		LiveRange[] rangeArray = new LiveRange[variableLives.values().size()];
		variableLives.values().toArray(rangeArray);
		Arrays.sort(rangeArray, LiveRange.RangeComparator);
		Set<String> liveVariables = new HashSet<String>();
		int spills = 0;
		for (LiveRange liveRange : rangeArray)
		{
			int currentBeginning = liveRange.begin;
			
			Set<String> variablesToRemove = new HashSet<String>();
			for (String variable : liveVariables)
			{
				int variableEnd = variableLives.get(variable).end;
				if (variableEnd <= currentBeginning)
					variablesToRemove.add(variable);
			}
			
			for (String variable : variablesToRemove)
			{
				liveVariables.remove(variable);
				availableRegisters.add(variableToRegister.get(variable));
			}
			
			liveVariables.add(liveRange.variableName);
			
			if (liveVariables.size() > maxVariablesConcurrentlyLive)
				maxVariablesConcurrentlyLive = liveVariables.size();
			
			if (availableRegisters.isEmpty())
			{
				variableToRegister.put(liveRange.variableName, Integer.toString(spills)); // Registers start with $, spills start with just the number
				spills++;
			}
			else
			{
				variableToRegister.put(liveRange.variableName, availableRegisters.first());
				availableRegisters.remove(availableRegisters.first());
			}
		}
		
		return maxVariablesConcurrentlyLive;
	}

	private static String Write_DATA(VDataSegment[] dataSegments) {
		String code = "";
		for (VDataSegment dataSegment : dataSegments)
		{
			code = code + (dataSegment.mutable ? "var " : "const ") + dataSegment.ident + "\n";
			for (VOperand.Static value : dataSegment.values)
			{
				code = code + "  " + value.toString() + "\n";
			}
			
			code = code + "\n";
		}
		return code;
	}
	
	private static boolean isNumeric(String str)  
	{
		try 
		{ 
			int i = Integer.parseInt(str);
		} catch(NumberFormatException nfe)  
		{ 
			return false;
		}
		
		return true;  
	}
	
	
}
